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

# Compare a regenerated analysis with its prior saved numerical results.
# Only aggregate validation statistics are exported; no woman-level records.
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k) {
  i <- match(k,args)
  if(is.na(i)||i==length(args))stop('Supply ',k)
  args[i+1L]
}
previous <- arg('--previous-dir'); current <- arg('--current-dir')
output <- arg('--output'); label <- arg('--label')
a <- jsonlite::fromJSON(file.path(previous,'report_data.json'))
b <- jsonlite::fromJSON(file.path(current,'report_data.json'))
checks <- list()
check <- function(name,passed,difference=NA_real_) {
  if(!isTRUE(passed))stop('Comparison failed: ',label,' / ',name,'; difference=',difference)
  checks[[length(checks)+1L]] <<- data.frame(check=name,passed=TRUE,maximum_absolute_difference=difference)
}
maxdiff <- function(x,y) {
  if(length(x)!=length(y)||!identical(is.na(x),is.na(y)))return(Inf)
  if(!any(is.finite(x)|is.finite(y)))return(0)
  max(abs(x-y),na.rm=TRUE)
}
ordered <- function(x,keys) x[do.call(order,x[keys]),,drop=FALSE]
check('Original source and need populations',a$audit$source_rows==b$audit$source_rows && a$audit$need_n==b$audit$need_n)
x<-ordered(a$heatmap,c('geo','side','variable'));y<-ordered(b$heatmap,c('geo','side','variable'))
check('Heatmap keys and valid-response denominators',identical(x[c('geo','side','variable','n_valid','n_total')],y[c('geo','side','variable','n_valid','n_total')]))
e<-maxdiff(x$value,y$value);check('Weighted demand and unweighted facility percentages unchanged',e<1e-8,e)
x<-a$bars[match(b$bars$field,a$bars$field),];y<-b$bars
e<-maxdiff(x$estimate,y$estimate);check('Use-distribution weighted percentages unchanged',e<1e-10,e)
check('Use-distribution actual counts and denominators unchanged',identical(x$n,y$n)&&identical(x$denominator,y$denominator))
bar_interval_changes <- sum((is.na(x$lo)!=is.na(y$lo))|(is.na(x$hi)!=is.na(y$hi))|
  ifelse(is.finite(x$lo)&is.finite(y$lo),abs(x$lo-y$lo)>1e-9,FALSE)|
  ifelse(is.finite(x$hi)&is.finite(y$hi),abs(x$hi-y$hi)>1e-9,FALSE))
x<-ordered(a$scores[a$scores$method=='zscore',], 'geo');y<-ordered(b$scores[b$scores$method=='zscore',], 'geo')
check('Every primary area flag/category unchanged',identical(x$geo,y$geo)&&identical(x$demand_flag,y$demand_flag)&&identical(x$supply_flag,y$supply_flag)&&identical(x$category,y$category))
e<-maxdiff(x$demand_score,y$demand_score);check('Area demand scores unchanged',e<1e-8,e)
expected_supply <- if(identical(a$audit$supply_score_orientation,'constraints'))x$supply_score else -x$supply_score
e<-maxdiff(expected_supply,y$supply_score);check('Supply constraints exactly reverse prior readiness',e<1e-8,e)
for(id in intersect(unique(a$scores$method),unique(b$scores$method))) {
 x<-ordered(a$scores[a$scores$method==id,],'geo');y<-ordered(b$scores[b$scores$method==id,],'geo')
 check(paste('Earlier scoring category preserved',id),identical(x$category,y$category))
}
check('Four map rules present',identical(sort(unique(b$sd_thresholds$threshold_sd)),c(0,.5,1,1.5)))
for(side in c('demand','supply')) {
 field<-paste0(side,'_score');sdfield<-paste0(side,'_sd');flag<-paste0(side,'_flag')
 z<-b$scores[b$scores$method=='zscore',];v<-z[[field]]
 distance<-(v-mean(v,na.rm=TRUE))/sd(v,na.rm=TRUE)
 for(cut in c(0,.5,1,1.5)) {
  r<-b$sd_thresholds[b$sd_thresholds$threshold_sd==cut,];r<-r[match(z$geo,r$geo),]
  e<-maxdiff(distance,r[[sdfield]])
  check(paste('Final-score SD distance',side,cut),e<1e-8,e)
  expected<-if(cut==0)v>1e-10 else distance>=cut
  check(paste('Independent SD flag',side,cut),identical(expected,r[[flag]]))
 }
}
models <- list();sample_n<-NA_integer_
for(folder in c('models_zscore','pca_appendix/models')) {
 old_file<-file.path(previous,folder,'fitted_models.rds')
 new_file<-file.path(current,folder,'fitted_models.rds')
 if(!file.exists(old_file))next
 check(paste('Model output exists',folder),file.exists(new_file))
 old<-readRDS(old_file);new<-readRDS(new_file)
 check(paste('Existing specifications preserved',folder),identical(names(old),names(new)))
 for(id in names(old)) {
  x<-old[[id]];y<-new[[id]];sample_n<-nrow(y$data)
  check(paste(folder,id,'same women, outcome and weights'),identical(x$data$row,y$data$row)&&identical(x$data$y_modern,y$data$y_modern)&&identical(x$data$weight,y$data$weight))
  check(paste(folder,id,'svyglm engine'),inherits(y$fit,'svyglm'))
  e<-maxdiff(x$data$demand_score,y$data$demand_score);check(paste(folder,id,'personal demand score'),e<1e-8,e)
  e<-maxdiff(x$data$supply_risk,y$data$supply_risk);check(paste(folder,id,'assigned supply risk unchanged'),e<1e-8,e)
  e<-maxdiff(coef(x$fit),coef(y$fit));check(paste(folder,id,'coefficients unchanged'),e<1e-8,e)
  e<-maxdiff(as.vector(x$V),as.vector(y$V));check(paste(folder,id,'geographic CR2 covariance unchanged'),e<1e-8,e)
 }
 for(file in c('model_coefficients.csv','model_block_tests.csv','score_predicted_probabilities.csv','individual_demand_probability_gaps.csv')) {
  x<-read.csv(file.path(previous,folder,file),stringsAsFactors=FALSE)
  y<-read.csv(file.path(current,folder,file),stringsAsFactors=FALSE)
  check(paste(folder,file,'same dimensions'),nrow(x)==nrow(y)&&identical(names(x),names(y)))
  for(k in names(x)) {
   if(is.numeric(x[[k]])) {
    e<-maxdiff(x[[k]],y[[k]]);check(paste(folder,file,k),e<1e-6,e)
   }else check(paste(folder,file,k),identical(x[[k]],y[[k]]))
  }
 }
 models[[folder]]<-list(specifications=length(new),women=sample_n)
}
result<-list(analysis=label,all_passed=TRUE,checks=do.call(rbind,checks),models=models,
 descriptive_interval_rows_changed=bar_interval_changes,
 interpretation='Weighted percentages, populations, primary flags and all existing model point/inference results are compared independently. Supply display sign and SD maps are intended changes. Descriptive intervals follow the newly explicit weights-only design and may differ from older sampling-cluster overview intervals.')
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(result,output,auto_unbox=TRUE,pretty=TRUE,na='null',dataframe='rows')
cat(label,':',length(checks),'comparisons passed; descriptive interval rows changed:',bar_interval_changes,'\n')
