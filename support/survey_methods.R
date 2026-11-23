# Shared SURGO estimation. Facility means and equal-area score references are
# deliberately outside the respondent-weighted survey design.

survey_weighted_mean <- function(x, w, na.rm = FALSE, ...) {
  x <- as.numeric(x); w <- as.numeric(w)
  stopifnot(length(x) == length(w))
  if (!length(x) || anyNA(w)) return(NA_real_)
  if (any(!is.finite(w)) || any(w < 0)) stop("Weights must be finite and nonnegative")
  keep <- w > 0
  if (!na.rm && anyNA(x[keep])) return(NA_real_)
  keep <- keep & !is.na(x)
  if (!any(keep)) return(NA_real_)
  a <- data.frame(value = x[keep], weight = w[keep])
  # One valid respondent determines the point mean exactly; a sampling variance
  # is not estimable. This helper returns no interval or variance in any case.
  if (nrow(a) == 1L) return(a$value[1])
  des <- survey::svydesign(ids = ~1, strata = NULL, weights = ~weight, data = a)
  as.numeric(coef(survey::svymean(~value, des, na.rm = na.rm)))
}

surgo_design <- function(data) {
  stopifnot(all(is.finite(data$weight)), all(data$weight > 0))
  survey::svydesign(ids = ~1, strata = NULL, weights = ~weight,
                    fpc = NULL, data = data)
}

fit_survey_logistic <- function(formula, data, design = NULL) {
  if (is.null(design)) design <- surgo_design(data)
  warnings <- character()
  fit <- withCallingHandlers(
    survey::svyglm(formula, design = design,
      family = quasibinomial(link = "logit"), rescale = TRUE,
      control = glm.control(maxit = 100)),
    warning = function(e) {
      warnings <<- c(warnings, conditionMessage(e))
      invokeRestart("muffleWarning")
    })
  stopifnot(fit$converged, fit$rank == length(coef(fit)),
            all(is.finite(coef(fit))))
  attr(fit, "fit_warnings") <- unique(warnings)
  fit
}

geographic_cr2 <- function(fit, cluster) {
  # CR2 is applied to the weighted logistic estimating equations. A native
  # svyglm object's existing survey sandwich must not be used as the GLM bread.
  # Keep the fitted object and its class unchanged; request the documented CR2
  # meat, then apply the inverse weighted information matrix explicitly.
  stopifnot(length(cluster) == nrow(model.matrix(fit)), !anyNA(cluster))
  if (!inherits(fit, "svyglm")) {
    return(clubSandwich::vcovCR(fit, cluster = cluster,
                              type = "CR2", inverse_var = FALSE))
  }
  stopifnot(fit$family$family == "quasibinomial")
  X <- model.matrix(fit)
  working_weights <- weights(fit, type = "working")
  weighted_residuals <- as.numeric(residuals(fit, type = "working")) * working_weights
  dispersion <- sum(weighted_residuals^2) / sum(working_weights)
  scale <- sum(summary(fit)$df[1:2]) * dispersion
  bread <- solve(crossprod(X, working_weights * X)) * scale
  meat <- clubSandwich::vcovCR(fit, cluster = cluster, type = "CR2",
                              inverse_var = FALSE, form = "meat")
  V <- meat
  V[,] <- bread %*% meat %*% bread / attr(meat, "v_scale")
  attr(V, "bread") <- bread
  V
}
