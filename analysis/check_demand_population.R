# Compare geographic classification using all eligible women versus women in need.
# Read-only with respect to report inputs, source data and main classifications.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
data_dir<-arg("--data-dir");out<-arg("--output-dir")
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables
current<-p$scores[p$scores$method=="zscore",];areas<-current$geo
stopifnot(!anyDuplicated(areas),length(D)==6,all(d$need<=d$positive))
selected<-d$positive&is.finite(d$weight)&d$weight>0&d$geo%in%areas
source_hash<-unname(tools::md5sum(file.path(data_dir,c("analysis_frame.rds","report_data.json"))))
wm<-function(v,w){ok<-is.finite(v)&is.finite(w)&w>0;if(any(ok))weighted.mean(v[ok],w[ok])else NA_real_}
classify<-function(df,sf)ifelse(is.na(df)|is.na(sf),"unassessed",ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
cells<-list();result<-list();refs<-list()
for(group in c("all_eligible","in_need")){
 use<-selected&if(group=="in_need")d$need else TRUE
 m<-sapply(D,function(k)vapply(areas,function(g)wm(d[[k]][use&d$geo%in%g],d$weight[use&d$geo%in%g]),numeric(1)))
 rownames(m)<-areas;complete<-complete.cases(m)
 mu<-colMeans(m[complete,,drop=FALSE]);sd<-apply(m[complete,,drop=FALSE],2,stats::sd)
 stopifnot(all(is.finite(mu)),all(sd>0),all(complete))
 z<-sweep(sweep(m,2,mu,"-"),2,sd,"/");score<-rowMeans(z);flag<-score>1e-10
 category<-classify(flag,current$supply_flag)
 if(group=="in_need"){
  stopifnot(max(abs(score-current$demand_score))<1e-9,
   identical(as.logical(flag),current$demand_flag),identical(as.character(category),as.character(current$category)))
 }
 result[[group]]<-data.frame(geo=areas,demand_score=score,demand_flag=flag,
  supply_flag=current$supply_flag,category=category,n_women=vapply(areas,function(g)sum(use&d$geo%in%g),integer(1)))
 for(k in D)for(g in areas){
  ii<-use&d$geo%in%g;valid<-ii&is.finite(d[[k]])
  cells[[length(cells)+1L]]<-data.frame(population=group,geo=g,variable=k,weighted_pct=100*m[g,k],n_valid=sum(valid),n_total=sum(ii))
 }
 refs[[group]]<-data.frame(population=group,variable=D,mean_pct=100*mu,SD_pct=100*sd,reference_areas=sum(complete))
}
a<-result$all_eligible;b<-result$in_need
labels<-if("district"%in%names(d))vapply(areas,function(g)as.character(d$district[match(g,d$geo)]),character(1))else areas
comparison<-data.frame(geo=areas,area=labels,all_eligible_n=a$n_women,in_need_n=b$n_women,
 all_eligible_demand_score=a$demand_score,in_need_demand_score=b$demand_score,
 all_eligible_demand_flag=a$demand_flag,in_need_demand_flag=b$demand_flag,
 supply_flag=current$supply_flag,all_eligible_category=a$category,current_in_need_category=b$category,
 changed=a$category!=b$category)
summary<-list(country=p$audit$country,areas=nrow(comparison),changed=sum(comparison$changed),unchanged=sum(!comparison$changed),
 assessed=sum(comparison$current_in_need_category!="unassessed"),demand_flags_changed=sum(a$demand_flag!=b$demand_flag),
 all_eligible_categories=as.list(table(a$category)),current_in_need_categories=as.list(table(b$category)),
 current_need_classification_reproduced=TRUE,
 method="Keep six domain definitions, valid-answer rules, fixed geographic reference set and supply flags. Re-estimate weighted concern percentages, component means/SDs and mean six z-scores for each population. Demand>0; supply-readiness flag unchanged.",
 all_eligible_definition="Consent/positive final survey weight where required; all current use and need categories. Original source records outside weighted estimation are excluded from both weighted comparisons.",
 no_main_report_changes=TRUE)
dir.create(out,recursive=TRUE,showWarnings=FALSE)
write.csv(comparison,file.path(out,"classification_population_comparison.csv"),row.names=FALSE,na="")
write.csv(do.call(rbind,cells),file.path(out,"demand_prevalence_by_population.csv"),row.names=FALSE,na="")
write.csv(do.call(rbind,refs),file.path(out,"normalization_by_population.csv"),row.names=FALSE,na="")
jsonlite::write_json(summary,file.path(out,"summary.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
stopifnot(identical(source_hash,unname(tools::md5sum(file.path(data_dir,c("analysis_frame.rds","report_data.json"))))))
cat(summary$country,":",summary$changed,"of",summary$areas,"classifications differ; current need-only results reproduced.\n")
