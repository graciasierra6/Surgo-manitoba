# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: jsonlite.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Test/validation utility.
# Inputs: Test fixtures or caller-supplied earlier/current saved results, as specified in the
#   code.
# Outputs: Assertions, validation records or aggregate before/after comparisons.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Compare a prior Sindh build with the corrected training-measure build.
args<-commandArgs(TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
previous<-arg("--previous-dir");current<-arg("--current-dir");output<-arg("--output")
a<-jsonlite::fromJSON(file.path(previous,"report_data.json"))
b<-jsonlite::fromJSON(file.path(current,"report_data.json"))
same<-function(x,y)isTRUE(all.equal(x,y,check.attributes=FALSE,tolerance=1e-9))
check<-function(ok,message)if(!isTRUE(ok))stop(message)
check(a$audit$source_rows==b$audit$source_rows&&a$audit$need_n==b$audit$need_n,"Source/need populations differ")
check(same(a$bars,b$bars),"Use-distribution estimates or intervals changed")
x<-a$heatmap[a$heatmap$variable!="training",];y<-b$heatmap[b$heatmap$variable!="training",]
check(same(x,y),"A component other than training changed")
x<-a$scores[a$scores$method=="zscore",];y<-b$scores[b$scores$method=="zscore",]
check(identical(x$geo,y$geo)&&same(x$demand_score,y$demand_score)&&identical(x$demand_flag,y$demand_flag),"Area demand scores/flags changed")
old<-readRDS(file.path(previous,"models_zscore/fitted_models.rds"))
new<-readRDS(file.path(current,"models_zscore/fitted_models.rds"))
check(identical(names(old),names(new)),"Model specifications differ")
for(k in names(old)){
 u<-old[[k]];v<-new[[k]]
 check(identical(u$data$row,v$data$row)&&same(u$data$y_modern,v$data$y_modern)&&same(u$data$weight,v$data$weight),paste(k,"model women/outcomes/weights differ"))
 check(same(u$data$demand_score,v$data$demand_score),paste(k,"personal demand score differs"))
 check(inherits(v$fit,"svyglm"),paste(k,"does not use svyglm"))
 if(!"supply_risk"%in%all.vars(v$formula))check(same(coef(u$fit),coef(v$fit))&&same(as.vector(u$V),as.vector(v$V)),paste(k,"demand-only fit differs"))
}
old_co<-read.csv(file.path(previous,"models_zscore/model_coefficients.csv"))
new_co<-read.csv(file.path(current,"models_zscore/model_coefficients.csv"))
ix<-match(paste(new_co$model,new_co$term),paste(old_co$model,old_co$term))
check(!anyNA(ix),"Coefficient identity mismatch")
co<-data.frame(model=new_co$model,term=new_co$term,original_OR=old_co$odds_ratio[ix],
 revised_OR=new_co$odds_ratio,revised_OR_lo=new_co$or_lo,revised_OR_hi=new_co$or_hi,
 original_p=old_co$p[ix],revised_p=new_co$p,
 significance_changed=(old_co$p[ix]<.05)!=(new_co$p<.05))
changes<-read.csv(file.path(current,"training_classification_comparison.csv"))
verification<-jsonlite::fromJSON(file.path(current,"checks/verification.json"))
check(isTRUE(verification$all_passed),"Independent report checks failed")
result<-list(scope="Sindh Q406 training reconstruction; other scientific definitions retained",
 all_passed=TRUE,independent_report_checks=verification$check_count,unit_scenarios=16,
 unchanged=c("Source/need populations","Use distribution and intervals","Six demand components",
 "Five other supply components","Woman-level model rows, outcomes and weights","Demand-only fits"),
 fp_facilities=b$audit$linked_fp_facilities,original_training_valid=b$audit$training_original_valid,
 revised_training_valid=b$audit$training_reconstructed_valid,known_staff=b$audit$training_known_staff,
 trained_staff=b$audit$training_trained_staff,dont_know_records=b$audit$training_dont_know_records,
 contradictory_records=b$audit$training_conflicting_records,malformed_records=b$audit$training_malformed_records,
 checkbox_text_overlap_staff=b$audit$training_encoding_overlap_staff,
 checkbox_text_topic_comparisons=b$audit$training_encoding_topic_comparisons,
 checkbox_text_topic_disagreements=b$audit$training_encoding_topic_disagreements,
 classification_changes=changes[changes$changed,],
 primary_model_women=nrow(new$score_joint$data),
 coefficient_significance_changes=sum(co$significance_changed),
 selected_coefficients=co[co$term%in%c("demand_score","supply_risk"),],
 interpretation="A revised measured component changes supply scores and some map flags. The unchanged need cohort, other components, demand-only fits and model samples are independently compared. This is not a claim that all numerical results are unchanged or that unknown training status is ignorable.")
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(result,output,auto_unbox=TRUE,pretty=TRUE,na="null",dataframe="rows",digits=12)
cat("Passed Sindh training-update comparisons;",sum(changes$changed),"classification changes across four thresholds.\n")
