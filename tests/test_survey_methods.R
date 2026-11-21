# Independent numerical checks of the shared weighted-estimation implementation.
args <- commandArgs(trailingOnly=TRUE)
script <- sub('^--file=', '', grep('^--file=',commandArgs(),value=TRUE)[1])
root <- normalizePath(file.path(dirname(script),'..'),mustWork=TRUE)
source(file.path(root,'analysis/common/survey_methods.R'))
set.seed(8142)
n <- 420L
d <- data.frame(weight=runif(n,.3,2.7), geo=rep(seq_len(21),each=20),
                x=rnorm(n), z=rep(rnorm(21),each=20))
d$y <- rbinom(n,1,plogis(-.4+.7*d$x-.3*d$z))
expected <- sum(d$weight*d$y)/sum(d$weight)
stopifnot(identical(survey_weighted_mean(1, 2), 1),
          is.na(survey_weighted_mean(numeric(), numeric())))
stopifnot(abs(survey_weighted_mean(d$y,d$weight)-expected)<1e-12)
v <- d$y; v[1:8] <- NA
stopifnot(is.na(survey_weighted_mean(v,d$weight)),
 abs(survey_weighted_mean(v,d$weight,na.rm=TRUE)-weighted.mean(v,d$weight,na.rm=TRUE))<1e-12)
fit <- fit_survey_logistic(y~x+z,d)
reference <- glm(y~x+z,data=d,weights=weight/mean(weight),family=quasibinomial())
V <- geographic_cr2(fit,d$geo)
reference_V <- clubSandwich::vcovCR(reference,cluster=d$geo,type='CR2',inverse_var=FALSE)
a <- as.data.frame(clubSandwich::coef_test(fit,vcov=V,test='Satterthwaite'))
b <- as.data.frame(clubSandwich::coef_test(reference,vcov=reference_V,test='Satterthwaite'))
stopifnot(inherits(fit,'svyglm'), max(abs(coef(fit)-coef(reference)))<1e-10,
 max(abs(V-reference_V))<1e-10, max(abs(a$SE-b$SE))<1e-10,
 max(abs(a$df_Satt-b$df_Satt))<1e-8, max(abs(a$p_Satt-b$p_Satt))<1e-10)
cat('PASS: weighted proportions, missing-response denominators, svyglm coefficients, unchanged object class, geographic CR2 covariance and Satterthwaite tests.\n')
