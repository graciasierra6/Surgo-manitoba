# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: clubSandwich, jsonlite, tools.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Regression sensitivity.
# Inputs: Saved analysis_frame.rds, model sample/score references and specified country.
# Outputs: Included/excluded profiles, available-case and response-weighted model comparisons.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Complete-response sensitivity checks for administrative-area country analyses.
# Input locations are supplied by the caller; only aggregate summaries enter HTML.
entry<-grep("^--file=",commandArgs(),value=TRUE)
project<-if(length(entry))dirname(normalizePath(gsub("~+~"," ",sub("^--file=","",entry[1]),fixed=TRUE)))else getwd()
while(!file.exists(file.path(project,"analysis/common/survey_methods.R"))){
 parent<-dirname(project);if(identical(parent,project))stop("Run within the complete project");project<-parent
}
source(file.path(project,"analysis/common/survey_methods.R"))
source(file.path(project,"analysis/common/risk_scores.R"))
sensitivity_configuration<-function(country){
 configurations<-list(
  sindh=list(country="Sindh",area_singular="district",area_plural="districts",need=3274L,common=3115L,base=3263L,areas=30L,appendix_title="Appendix · Sensitivity checks"),
  senegal=list(country="Senegal",area_singular="département",area_plural="départements",need=1905L,common=1558L,base=1893L,areas=33L,appendix_title="Appendix C · Sensitivity checks"),
  cotedivoire=list(country="Côte d’Ivoire",area_singular="département",area_plural="départements",need=4292L,common=3951L,base=4185L,areas=77L,appendix_title="Appendix C · Sensitivity checks"))
 if(!country%in%names(configurations))stop("Unsupported administrative-area country")
 configurations[[country]]
}

response_probabilities<-function(base,controls){
 stopifnot(all(complete.cases(base[c("responder","y_modern",controls,"geo","weight")])))
 groups<-split(base$responder,base$geo)
 stopifnot(all(vapply(groups,function(x)any(x==1),logical(1))))
 complete_areas<-names(groups)[vapply(groups,function(x)all(x==1),logical(1))]
 partial<-!base$geo%in%complete_areas;probability<-rep(1,nrow(base));fit<-NULL
 if(any(partial)){
  a<-droplevels(base[partial,]);a$response_area<-factor(a$geo)
  formula<-reformulate(c("y_modern",controls,"response_area"),"responder")
  fit<-fit_survey_logistic(formula,a)
  probability[partial]<-as.numeric(predict(fit,newdata=a,type="response"))
 }
 stopifnot(all(is.finite(probability)),all(probability>0),all(probability<=1))
 list(probability=probability,fit=fit,complete_areas=complete_areas)
}

cluster_jackknife_covariance<-function(estimates){
 G<-nrow(estimates);centered<-sweep(estimates,2,colMeans(estimates),"-")
 crossprod(centered)*(G-1)/G
}

build_regression_sensitivity<-function(data_dir,out,country){
 config<-sensitivity_configuration(country)
 dir.create(out,recursive=TRUE,showWarnings=FALSE)
 frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
 p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables
 stopifnot(p$audit$need_n==config$need,p$audit$supply_score_orientation=="constraints")
 original<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"))
 refs<-read.csv(file.path(data_dir,"models_zscore/individual_demand_score_reference.csv"))
 d$demand_score<-rowMeans(sapply(D,function(k){r<-refs[refs$variable==k,];(d[[k]]-r$weighted_mean)/r$weighted_SD}))
 d$supply_risk<-supply_constraint_score(frame$supply,p$component_trace,TRUE)
 controls<-setdiff(all.vars(original$supply$formula),c("y_modern","supply_risk"))
 common_vars<-c("y_modern",controls,D,"demand_score","supply_risk")
 d$common<-d$need&d$positive&complete.cases(d[common_vars])
 d$base<-d$need&d$positive&complete.cases(d[c("y_modern",controls,"supply_risk")])
 stopifnot(identical(d$row[d$common],original$score_joint$data$row),sum(d$common)==config$common,sum(d$base)==config$base)
 stopifnot(all(vapply(split(d$supply_risk,d$geo),function(v)length(unique(v))==1,logical(1))))
 source_files<-c(file.path(data_dir,"analysis_frame.rds"),file.path(data_dir,"report_data.json"),
  file.path(data_dir,"models_zscore/fitted_models.rds"))
 source_hash<-unname(tools::md5sum(source_files))
 savecsv<-function(x,name)write.csv(x,file.path(out,name),row.names=FALSE,na="")
 wm<-function(x,w){ii<-is.finite(x);if(any(ii))survey_weighted_mean(x[ii],w[ii])else NA_real_}
 full_design<-surgo_design(d[d$positive,])
 cr2_table<-function(f,a){
  V<-geographic_cr2(f,a$geo);ct<-as.data.frame(clubSandwich::coef_test(f,vcov=V,test="Satterthwaite"))
  data.frame(term=rownames(ct),estimate=ct$beta,se=ct$SE,df=ct$df_Satt,p=ct$p_Satt,
   odds_ratio=exp(ct$beta),or_lo=exp(ct$beta-qt(.975,ct$df_Satt)*ct$SE),or_hi=exp(ct$beta+qt(.975,ct$df_Satt)*ct$SE))
 }
 # Describe exclusions using all women in the approved need group, not just known outcomes.
 groups<-list(Included=d$need&d$common,Excluded=d$need&!d$common)
 overview<-do.call(rbind,lapply(names(groups),function(k){a<-d[groups[[k]],]
  data.frame(group=k,n=nrow(a),survey_weight=sum(a$weight),known_outcomes=sum(is.finite(a$y_modern)),
   modern_use_pct=100*wm(a$y_modern,a$weight),mean_age=30+10*wm(a$age10,a$weight),
   urban_pct=if("urban"%in%names(a))100*wm(a$urban,a$weight)else NA_real_,partnered_pct=100*wm(a$partnered,a$weight))
 }))
 categories<-do.call(rbind,lapply(c("education","fertility_goal"),function(k)do.call(rbind,lapply(names(groups),function(g){
  a<-d[groups[[g]],];v<-as.character(a[[k]]);v[is.na(v)]<-"Unresolved"
  do.call(rbind,lapply(sort(unique(v)),function(level)data.frame(variable=k,group=g,level=level,
   n=sum(v==level),weighted_pct=100*survey_weighted_mean(as.numeric(v==level),a$weight))))
 }))))
 missingness<-do.call(rbind,lapply(c("y_modern",controls,D,"supply_risk"),function(k){
  a<-d[d$need,];data.frame(variable=k,n_missing=sum(is.na(a[[k]])),total=nrow(a),
   weighted_missing_pct=100*survey_weighted_mean(as.numeric(is.na(a[[k]])),a$weight))
 }))
 area_coverage<-do.call(rbind,lapply(sort(unique(d$geo[d$need])),function(g){a<-d[d$need&d$geo==g,]
  data.frame(geo=g,area=a$district[1],need_n=nrow(a),common_n=sum(a$common),excluded_n=sum(!a$common),
   excluded_weighted_pct=100*survey_weighted_mean(as.numeric(!a$common),a$weight),
   broader_base_n=sum(a$base))
 }))
 savecsv(overview,"included_excluded_overview.csv");savecsv(categories,"included_excluded_categories.csv")
 savecsv(missingness,"exclusion_drivers.csv");savecsv(area_coverage,"exclusions_by_area.csv")
 # Identical formulas in the common and model-specific available samples.
 formulas<-c(list(supply=original$supply$formula),setNames(lapply(D,function(k)
  reformulate(c(controls,k,"supply_risk"),"y_modern")),paste0("single_",D)))
 coefficients<-samples<-fits<-list()
 for(id in names(formulas))for(version in c("common","available")){
  formula<-formulas[[id]]
  eligible<-if(version=="common")d$common else d$need&d$positive&complete.cases(d[all.vars(formula)])
  fd<-full_design;fd$variables$.sensitivity_eligible<-eligible[d$positive]
  des<-subset(fd,.sensitivity_eligible);a<-droplevels(d[eligible,])
  fit<-fit_survey_logistic(formula,a,des);co<-cr2_table(fit,a)
  co$model<-id;co$sample<-version;coefficients[[paste(id,version)]]<-co
  samples[[paste(id,version)]]<-data.frame(model=id,sample=version,n=nrow(a),events=sum(a$y_modern),
   areas=length(unique(a$geo)),weighted_modern_use=wm(a$y_modern,a$weight))
  fits[[paste(id,version)]]<-list(fit=fit,data=a,formula=formula)
  if(id=="supply"&&version=="common")stopifnot(max(abs(coef(fit)-coef(original$supply$fit)))<1e-9,
   max(abs(geographic_cr2(fit,a$geo)-original$supply$V))<1e-9)
 }
 coefficients<-do.call(rbind,coefficients);samples<-do.call(rbind,samples)
 savecsv(coefficients,"available_case_coefficients.csv");savecsv(samples,"available_case_samples.csv")
 # Response weights target women with known outcome, controls and area-wide supply.
 base<-droplevels(d[d$base,]);base$responder<-as.numeric(base$common)
 response<-response_probabilities(base,controls);base$response_probability<-response$probability
 base$response_adjustment<-1/base$response_probability
 joint_specs<-lapply(original[c("score_joint","components_joint")],`[[`,"formula")
 fit_joint<-function(a,response_weighted,formula){
  cc<-droplevels(a[a$responder==1,]);if(response_weighted)cc$weight<-cc$weight*cc$response_adjustment
  fit_survey_logistic(formula,cc)
 }
 joint_fits<-list();joint_points<-list();replicates<-list()
 for(id in names(joint_specs))for(version in c("survey_only","response_weighted")){
  key<-paste(id,version);f<-fit_joint(base,version=="response_weighted",joint_specs[[id]])
  joint_fits[[key]]<-f;joint_points[[key]]<-coef(f)
  if(version=="survey_only")stopifnot(max(abs(coef(f)-coef(original[[id]]$fit)))<1e-9)
 }
 # Delete one administrative area, refitting both stages each time.
 # This is geographic error-cluster inference, not an asserted sampling-PSU design.
 areas<-sort(unique(base$geo));G<-length(areas);stopifnot(G==config$areas)
 for(g in areas){
  a<-droplevels(base[base$geo!=g,]);rr<-response_probabilities(a,controls)
  a$response_probability<-rr$probability;a$response_adjustment<-1/rr$probability
  for(id in names(joint_specs))for(version in c("survey_only","response_weighted")){
   key<-paste(id,version);f<-fit_joint(a,version=="response_weighted",joint_specs[[id]])
   stopifnot(identical(names(coef(f)),names(joint_points[[key]])))
   replicates[[paste(key,g)]]<-data.frame(model=id,weighting=version,omitted_area=g,
    term=names(coef(f)),estimate=unname(coef(f)))
  }
  cat("Completed sensitivity jackknife:",g,"\n")
 }
 replicates<-do.call(rbind,replicates);joint<-list()
 for(id in names(joint_specs))for(version in c("survey_only","response_weighted")){
  key<-paste(id,version);b<-joint_points[[key]];r<-replicates[replicates$model==id&replicates$weighting==version,]
  B<-sapply(names(b),function(term)r$estimate[match(paste(areas,term),paste(r$omitted_area,r$term))])
  stopifnot(all(is.finite(B)))
  V<-cluster_jackknife_covariance(B);se<-sqrt(diag(V));df<-G-1
  joint[[key]]<-data.frame(model=id,weighting=version,term=names(b),estimate=unname(b),se=se,df=df,
   p=2*pt(-abs(b/se),df),odds_ratio=exp(b),or_lo=exp(b-qt(.975,df)*se),or_hi=exp(b+qt(.975,df)*se),n=sum(base$responder==1))
 }
 joint<-do.call(rbind,joint);savecsv(joint,"response_weighted_joint_coefficients.csv")
 savecsv(replicates,"response_weighted_area_jackknife.csv")
 # Observed balance against the broader eligible target group.
 balance_matrix<-cbind(modern_use=base$y_modern,age_years=30+10*base$age10,partnered=base$partnered,
  supply_risk=base$supply_risk,model.matrix(~education+fertility_goal+factor(geo)-1,base))
 if("urban"%in%names(base))balance_matrix<-cbind(balance_matrix,urban=base$urban)
 complete<-base$responder==1
 balance<-do.call(rbind,lapply(colnames(balance_matrix),function(k){
  v<-balance_matrix[,k];target<-wm(v,base$weight);sd<-sqrt(wm((v-target)^2,base$weight))
  before<-wm(v[complete],base$weight[complete]);after<-wm(v[complete],base$weight[complete]*base$response_adjustment[complete])
  data.frame(variable=k,target_mean=target,common_mean=before,response_weighted_mean=after,
   standardized_difference_before=if(sd>0)(before-target)/sd else 0,
   standardized_difference_after=if(sd>0)(after-target)/sd else 0)
 }))
 savecsv(balance,"response_weighting_balance.csv")
 adjusted<-base$weight[complete]*base$response_adjustment[complete]
 kish<-function(w)sum(w)^2/sum(w^2)
 diagnostics<-list(target_n=nrow(base),complete_n=sum(complete),areas=G,fully_observed_areas=response$complete_areas,
  response_probability_min=min(base$response_probability),response_probability_max=max(base$response_probability),
  response_adjustment_min=min(base$response_adjustment[complete]),response_adjustment_median=median(base$response_adjustment[complete]),
  response_adjustment_p99=unname(quantile(base$response_adjustment[complete],.99)),response_adjustment_max=max(base$response_adjustment[complete]),
  kish_effective_n_original=kish(base$weight[complete]),kish_effective_n_adjusted=kish(adjusted),
  maximum_absolute_balance_difference_before=max(abs(balance$standardized_difference_before)),
  maximum_absolute_balance_difference_after=max(abs(balance$standardized_difference_after)),
  response_model_formula=paste("Complete demand information ~ modern use +",paste(controls,collapse=" + "),"+ administrative-area indicators; probability 1 in areas fully observed within the eligible target."),
  response_weight_definition="Original SURGO weight / estimated probability of complete demand information. No trimming or imputation.",
  inference=paste0("For response-weighted joint comparisons, delete-one-area jackknife refits the response and outcome models; variance=(G-1)/G times centered replicate crossproducts; t intervals use G-1=",G-1," df. Survey-only joint comparison uses the same jackknife; primary report inference remains CR2/Satterthwaite."))
 audit<-list(country=config$country,country_key=country,supply_resolution=paste(config$area_singular,"only"),area_singular=config$area_singular,area_plural=config$area_plural,appendix_title=config$appendix_title,geographic_clusters=G,need_n=sum(d$need),common_n=sum(d$common),
  excluded_n=sum(d$need&!d$common),base_n=sum(d$base),regression_function="survey::svyglm",
  design_ids="~1",design_strata="None",weights="Original setting-specific SURGO weight; response adjustments only in the named sensitivity fits",
  controls=controls,demand_references_fixed=TRUE,supply_references_fixed=TRUE,
  main_results_unchanged=TRUE,source_files_unchanged=identical(source_hash,unname(tools::md5sum(source_files))),
  uncertainty="Response weighting requires the completeness model to account adequately for selection using observed information. It cannot establish robustness to selection on unrecorded concerns. Jackknife inference uses the available administrative-area clusters and conditions on fixed score references.")
 stopifnot(audit$source_files_unchanged)
 payload<-list(audit=audit,overview=overview,categories=categories,missingness=missingness,area_coverage=area_coverage,
  available_coefficients=coefficients,available_samples=samples,joint_coefficients=joint,weight_diagnostics=diagnostics,balance=balance,
  demand_variables=D,demand_labels=p$demand_labels)
 jsonlite::write_json(payload,file.path(out,"sensitivity_results.json"),auto_unbox=TRUE,pretty=TRUE,dataframe="rows",na="null",digits=12)
 saveRDS(list(data=d,base=base,response=response,available_fits=fits,joint_fits=joint_fits,replicates=replicates),
  file.path(out,"sensitivity_fits.rds"))
 cat("Built",config$country,"administrative-area sensitivity checks with",sum(d$common),"common and",sum(d$base),"broader eligible women.\n")
 invisible(payload)
}

if(sys.nframe()==0L){
 args<-commandArgs(TRUE);arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
 build_regression_sensitivity(arg("--data-dir"),arg("--output-dir"),arg("--country"))
}
