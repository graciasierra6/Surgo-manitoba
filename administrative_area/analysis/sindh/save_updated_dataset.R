# Save a complete woman-level analytic join; restricted outputs are not packaged.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-arg("--input");data_dir<-arg("--data-dir");out<-arg("--output-dir")
x<-readRDS(input);frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables
ref<-read.csv(file.path(data_dir,"models_zscore/individual_demand_score_reference.csv"))
models<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"))
stopifnot(nrow(x)==nrow(d),nrow(x)==10364,identical(x$col_id,d$id))
original<-x;original_fields<-names(x)
if(!"fpn2_derived"%in%names(x))x$fpn2_derived<-d$need_code
x$analysis_in_need_fp<-d$need;x$analysis_modern_use<-d$y_modern
for(k in D)x[[paste0("demand_",k)]]<-d[[k]]
Z<-sapply(D,function(k){r<-ref[ref$variable==k,];(d[[k]]-r$weighted_mean)/r$weighted_SD})
x$demand_risk_score<-rowMeans(Z);x$demand_risk_score[!d$need]<-NA_real_
z<-p$scores[p$scores$method=="zscore",]
x$supply_readiness_score<-z$supply_score[match(d$geo,z$geo)]
x$supply_risk_score<- -x$supply_readiness_score
x$model_complete_case<-d$row%in%models$score_joint$data$row
stopifnot(all(vapply(original_fields,function(k)identical(x[[k]],original[[k]]),logical(1))))
ii<-models$score_joint$data$row
stopifnot(max(abs(x$demand_risk_score[ii]-models$score_joint$data$demand_score))<1e-10,
 max(abs(x$supply_risk_score[ii]-models$score_joint$data$supply_risk))<1e-10)
attr(x$demand_risk_score,"label")<-"Personal mean standardized concern score; need group only"
attr(x$supply_risk_score,"label")<-"Negative mean favorable district supply z-score; higher is worse"
attr(x$supply_readiness_score,"label")<-"Mean favorable district supply z-score; higher is better"
dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(x,file.path(out,"sindh_women_manitoba_updated.rds"),compress="gzip")
haven::write_dta(x,file.path(out,"sindh_women_manitoba_updated.dta"),version=15)
jsonlite::write_json(list(rows=nrow(x),original_join_columns_preserved=TRUE,need_n=sum(d$need),
 common_model_n=sum(x$model_complete_case),supply_contexts=length(unique(d$geo)),
 demand_score_scope="Each woman's own six responses; weighted need-group component references; missing outside the need group.",
 supply_score_scope="Assigned district FP-provider summaries; all residents share their district score; no Rural/Urban split.",
 restricted_output="Woman-level microdata; excluded from source-code upload packages."),
 file.path(out,"updated_dataset_verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Saved updated individual dataset: 10364 women,",sum(x$model_complete_case),"common model cases.\n")
