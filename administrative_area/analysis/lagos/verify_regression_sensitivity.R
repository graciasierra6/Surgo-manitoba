# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: clubSandwich, jsonlite.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Verification.
# Inputs: Main analysis frame plus saved sensitivity results.
# Outputs: Independent sensitivity verification.json and numerical checks.
# Source context via country pipeline: D01,D05,D09,D10,D11,D12; exact dataset names in
#   docs/COLLEAGUE_RUN_GUIDE.md.
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Independent sample, weighting, coefficient and two-stage jackknife checks.
args<-commandArgs(TRUE);arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
data_dir<-arg("--data-dir");out<-arg("--sensitivity-dir")
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
p<-jsonlite::fromJSON(file.path(out,"sensitivity_results.json"));ob<-readRDS(file.path(out,"sensitivity_fits.rds"))
original<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"))
D<-p$demand_variables;controls<-p$audit$controls
checks<-list();check<-function(name,ok){if(!isTRUE(ok))stop(name);checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE)}
same<-function(a,b,tol=1e-8)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=tol))
check("Original survey variables preserved",all(vapply(names(d),function(k)same(d[[k]],ob$data[[k]],0),logical(1))))
base<-d$need&d$positive&complete.cases(d[c("y_modern",controls)])
check("Broader target independently selected",identical(d$row[base],ob$base$row)&&sum(base)==2132)
common<-base&complete.cases(d[D]);check("Common model sample independently selected",identical(d$row[common],original$score_joint$data$row)&&sum(common)==1896)
for(model in unique(p$available_samples$model))for(version in c("common","available")){
 key<-paste(model,version);a<-ob$available_fits[[key]];needed<-all.vars(a$formula)
 ii<-if(version=="common")common else ob$data$need&ob$data$positive&complete.cases(ob$data[needed])
 check(paste(model,version,"exact rows"),identical(a$data$row,d$row[ii]))
 check(paste(model,version,"need and modern outcome"),all(a$data$need)&&all(a$data$y_modern%in%0:1))
 check(paste(model,version,"unchanged survey weights"),same(a$data$weight,d$weight[ii],0))
 check_data<-a$data;check_data$.verification_weight<-check_data$weight/mean(check_data$weight)
 f<-glm(a$formula,data=check_data,weights=.verification_weight,family=quasibinomial(),control=glm.control(maxit=100))
 check(paste(model,version,"independent weighted coefficients"),same(coef(f),coef(a$fit)))
 V<-clubSandwich::vcovCR(f,cluster=a$data$geo,type="CR2",inverse_var=FALSE)
 ct<-as.data.frame(clubSandwich::coef_test(f,vcov=V,test="Satterthwaite"))
 co<-p$available_coefficients[p$available_coefficients$model==model&p$available_coefficients$sample==version,]
 co<-co[match(rownames(ct),co$term),]
 check(paste(model,version,"LGA CR2 Satterthwaite intervals"),same(co$or_lo,exp(ct$beta-qt(.975,ct$df_Satt)*ct$SE))&&same(co$or_hi,exp(ct$beta+qt(.975,ct$df_Satt)*ct$SE)))
}
for(group in c("Included","Excluded")){
 ii<-if(group=="Included")common else d$need&!common;r<-p$overview[p$overview$group==group,]
 known<-ii&is.finite(d$y_modern)
 check(paste(group,"weighted modern use and known denominator"),same(r$modern_use_pct,100*weighted.mean(d$y_modern[known],d$weight[known]))&&r$known_outcomes==sum(known))
 check(paste(group,"age units independently recovered"),same(r$mean_age,weighted.mean(30+10*d$age10[ii],d$weight[ii])))
}
# Re-estimate completeness probabilities with an independent ordinary weighted GLM.
response_probability<-function(a){
 sums<-tapply(a$responder,a$geo,sum);counts<-table(a$geo)
 complete_areas<-names(sums)[sums==as.numeric(counts[names(sums)])]
 ii<-!a$geo%in%complete_areas;probability<-rep(1,nrow(a))
 if(any(ii)){
  b<-droplevels(a[ii,]);b$response_area<-factor(b$geo)
  b$.verification_weight<-b$weight/mean(b$weight)
  f<-glm(reformulate(c("y_modern",controls,"response_area"),"responder"),data=b,
   weights=.verification_weight,family=quasibinomial(),control=glm.control(maxit=100))
  probability[ii]<-predict(f,type="response")
 }
 probability
}
b<-ob$base;probability<-response_probability(b)
check("Independent response probabilities",same(probability,b$response_probability))
check("Positive response probabilities and inverse adjustments",all(probability>0&probability<=1)&&same(b$response_adjustment,1/probability))
check("Fully observed LGAs require no response adjustment",setequal(p$weight_diagnostics$fully_observed_lgas,c("ETIOSA","IKEJA"))&&all(b$response_adjustment[b$geo%in%c("ETIOSA","IKEJA")]==1))
independent_joint<-function(a,weighted,formula){
 probability<-response_probability(a);ii<-a$responder==1;w<-a$weight[ii]
 if(weighted)w<-w/probability[ii]
 check_data<-a[ii,];check_data$.verification_weight<-w/mean(w)
 glm(formula,data=check_data,weights=.verification_weight,family=quasibinomial(),control=glm.control(maxit=100))
}
for(model in c("score_joint","components_joint"))for(version in c("survey_only","response_weighted")){
 fo<-original[[model]]$formula;key<-paste(model,version)
 f<-independent_joint(b,version=="response_weighted",fo)
 check(paste(key,"independent full-fit coefficients"),same(coef(f),coef(ob$joint_fits[[key]])))
 areas<-sort(unique(b$geo));B<-matrix(NA_real_,length(areas),length(coef(f)),dimnames=list(areas,names(coef(f))))
 for(g in areas){
  a<-droplevels(b[b$geo!=g,]);rep<-independent_joint(a,version=="response_weighted",fo)
  B[g,]<-coef(rep);saved<-ob$replicates[ob$replicates$model==model&ob$replicates$weighting==version&ob$replicates$omitted_lga==g,]
  check(paste(key,g,"refit both stages"),same(coef(rep),saved$estimate[match(names(coef(rep)),saved$term)]))
 }
 V<-matrix(0,ncol(B),ncol(B));mu<-colMeans(B)
 for(j in seq_len(nrow(B)))V<-V+tcrossprod(B[j,]-mu)
 V<-V*(length(areas)-1)/length(areas);se<-sqrt(diag(V))
 co<-p$joint_coefficients[p$joint_coefficients$model==model&p$joint_coefficients$weighting==version,]
 co<-co[match(names(coef(f)),co$term),]
 check(paste(key,"independent jackknife variance"),same(co$se,se))
 check(paste(key,"17-LGA t16 intervals"),all(co$df==16)&&same(co$or_lo,exp(coef(f)-qt(.975,16)*se))&&same(co$or_hi,exp(coef(f)+qt(.975,16)*se)))
}
check("Score units fixed to main models",same(ob$data$demand_score[common],original$score_joint$data$demand_score)&&same(ob$data$supply_risk[common],original$score_joint$data$supply_risk))
check("Whole-LGA supply assignment retained",all(vapply(split(ob$data$supply_risk,ob$data$geo),function(x)length(unique(x))==1,logical(1))))
result<-list(all_passed=TRUE,check_count=length(checks),country="Lagos",supply_resolution="LGA only",
 need_n=2159,main_n=1896,supply_available_n=2132,primary_results_unchanged=TRUE,
 limitations="Implementation checks do not validate the untestable response-selection assumption or reconstruct the original sampling design.")
write.csv(do.call(rbind,checks),file.path(out,"verification_checks.csv"),row.names=FALSE)
jsonlite::write_json(result,file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Passed",length(checks),"independent regression-sensitivity checks.\n")
