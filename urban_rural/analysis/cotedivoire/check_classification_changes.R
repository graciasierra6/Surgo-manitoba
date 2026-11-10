# Controlled classification comparisons; saved reports and model fits are unchanged.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
old_dir<-arg("--friday-data-dir");new_dir<-arg("--updated-data-dir")
facility_input<-arg("--facility-input");raw_facility<-arg("--raw-facility");out<-arg("--output-dir")
script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
project_root<-normalizePath(file.path(dirname(script),"..","..",".."),mustWork=TRUE)
source(file.path(project_root,"analysis/cotedivoire/cdi_definitions.R"))
source(file.path(dirname(script),"supply_definitions.R"))
old<-jsonlite::fromJSON(file.path(old_dir,"report_data.json"));new<-jsonlite::fromJSON(file.path(new_dir,"report_data.json"))
stopifnot(old$audit$need_n==4292,new$audit$need_n==4292,identical(old$demand_variables,new$demand_variables))
old_scores<-old$scores[old$scores$method=="zscore",];new_scores<-new$scores[new$scores$method=="zscore",]
areas<-sort(old_scores$geo);stopifnot(identical(areas,sort(new_scores$geo)),length(areas)==82)
old_scores<-old_scores[match(areas,old_scores$geo),];new_scores<-new_scores[match(areas,new_scores$geo),]
D<-old$demand_variables
f<-readRDS(facility_input);f<-f[f$country=="cotedivoire",]
r<-haven::read_dta(raw_facility,col_select=tidyselect::all_of(c("fid","s2_5_sp","s7_2",cdi_frequency_fields())))
ri<-match(f$fid,as.character(r$fid));stopifnot(nrow(f)==3078,!anyNA(ri))
f$geo<-cdi_key(f$assigned_join_area);f$fp<-as.numeric(r$s2_5_sp[ri])%in%1
b<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
f$offering<-b(f$one_larc_3_sa_offer);f$free<-as.numeric(f$p_free_prod)
f$training<-as.numeric(f$p_train);f$larc_stock<-b(f$one_larc_stockin)
f$popular_stock<-b(f$mp_in_stock);cq<-as.numeric(r$s7_2[ri])
f$counselor<-ifelse(cq%in%1,1,ifelse(cq%in%2,0,NA_real_))
stopifnot(isTRUE(all.equal(f$counselor,as.numeric(f$counselor_available),check.attributes=FALSE)))
f$stockout_burden<-as.numeric(f$p_any_stockout)
f$daily_provision<-cdi_frequency(r[ri,])$daily_provision
mean_valid<-function(v)if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_
scoring<-function(m,common_reference=TRUE){
 ref<-complete.cases(m)
 if(common_reference){mu<-colMeans(m[ref,,drop=FALSE]);ss<-apply(m[ref,,drop=FALSE],2,sd)}else{mu<-colMeans(m,na.rm=TRUE);ss<-apply(m,2,sd,na.rm=TRUE)}
 stopifnot(all(ss>0));z<-sweep(sweep(m,2,mu,"-"),2,ss,"/")
 list(scores=rowMeans(z),n=sum(ref),mean=mu,sd=ss)
}
heatmat<-function(p,side,fields){
 ans<-sapply(fields,function(k){q<-p$heatmap[p$heatmap$side==side&p$heatmap$variable==k,];q$value[match(areas,q$geo)]})
 rownames(ans)<-areas;ans
}
supply_mat<-function(new_variables,fp_only){
 fields<-if(new_variables)new$supply_variables else old$supply_variables
 m<-sapply(fields,function(k)vapply(areas,function(g){v<-f[[k]][f$geo%in%g&(!fp_only|f$fp)]
 a<-mean_valid(v);if(k%in%c("offering","larc_stock"))a<-round(a,5);100*a},numeric(1)))
 rownames(m)<-areas
 if(new_variables)m[,"stockout_burden"]<-100-m[,"stockout_burden"]
 m
}
# Exact endpoint checks anchor the exercise to the previously rendered reports.
dm<-list(old=heatmat(old,"Demand",D),new=heatmat(new,"Demand",D))
dscore<-lapply(dm,scoring)
supply<-list();sinputs<-list()
for(v in 0:1)for(ff in 0:1)for(cc in 0:1){k<-paste(v,ff,cc,sep="_");sinputs[[k]]<-supply_mat(v==1,ff==1);supply[[k]]<-scoring(sinputs[[k]],cc==1)}
old_sm<-heatmat(old,"Supply",old$supply_variables);new_sm<-heatmat(new,"Supply",new$supply_variables);new_sm[,"stockout_burden"]<-100-new_sm[,"stockout_burden"]
maxdiff<-function(a,b){stopifnot(identical(dim(a),dim(b)),identical(as.vector(is.na(a)),as.vector(is.na(b))));max(abs(a-b),na.rm=TRUE)}
endpoint_errors<-c(old_demand=maxdiff(dscore$old$scores,old_scores$demand_score),new_demand=maxdiff(dscore$new$scores,new_scores$demand_score),
 old_supply_inputs=maxdiff(sinputs[['0_0_0']],old_sm),new_supply_inputs=maxdiff(sinputs[['1_1_1']],new_sm),
 old_supply=maxdiff(supply[['0_0_0']]$scores,old_scores$supply_score),new_supply=maxdiff(supply[['1_1_1']]$scores,new_scores$supply_score))
print(endpoint_errors)
stopifnot(max(endpoint_errors)<1e-8)
classify<-function(df,sf)ifelse(is.na(df)|is.na(sf),"unassessed",ifelse(df&sf,"both",ifelse(df,"demand",ifelse(sf,"supply","neither"))))
variants<-expand.grid(cutoff_zero=0:1,health_pb2a=0:1,new_supply_variables=0:1,fp_providers_only=0:1,common_supply_reference=0:1)
results<-details<-list()
for(i in seq_len(nrow(variants))){
 v<-variants[i,];ds<-dscore[[if(v$health_pb2a==1)"new"else"old"]]$scores
 sk<-paste(v$new_supply_variables,v$fp_providers_only,v$common_supply_reference,sep="_");ss<-supply[[sk]]$scores
 dc<-if(v$cutoff_zero==1)0 else median(ds,na.rm=TRUE);sc<-if(v$cutoff_zero==1)0 else median(ss,na.rm=TRUE)
 df<-ds>dc+1e-10;sf<-ss<sc-1e-10;cat<-classify(df,sf)
 id<-paste(v,collapse="")
 q<-data.frame(variant=id,geo=areas,demand_score=ds,supply_score=ss,demand_flag=df,supply_flag=sf,category=cat)
 for(k in names(v))q[[k]]<-v[[k]]
 details[[id]]<-q
 results[[id]]<-cbind(data.frame(variant=id),v,data.frame(
  demand_cutoff=dc,supply_cutoff=sc,demand_reference_n=dscore[[if(v$health_pb2a==1)"new"else"old"]]$n,supply_reference_n=supply[[sk]]$n,
  changed_from_friday=sum(cat!=old_scores$category),changed_from_updated=sum(cat!=new_scores$category),
  demand_flags_changed_from_friday=sum(df!=old_scores$demand_flag,na.rm=TRUE),supply_flags_changed_from_friday=sum(sf!=old_scores$supply_flag,na.rm=TRUE),
  supply_assessability_changes=sum(is.na(sf)!=is.na(old_scores$supply_flag)),
  both=sum(cat=="both"),demand_only=sum(cat=="demand"),supply_only=sum(cat=="supply"),neither=sum(cat=="neither"),unassessed=sum(cat=="unassessed")))
}
results<-do.call(rbind,results);detail<-do.call(rbind,details)
stopifnot(identical(details[['00000']]$category,old_scores$category),identical(details[['11111']]$category,new_scores$category))
labels<-c(cutoff_zero="Use zero instead of final-score medians",health_pb2a="PB2A-only health beliefs",new_supply_variables="Replace counselor/popular stock with daily provision/stockout burden",fp_providers_only="Restrict facilities to s2_5_sp Yes",common_supply_reference="Use common complete-department supply normalization reference")
comparisons<-do.call(rbind,lapply(seq_along(labels),function(j){
 a<-rep(0,5);a[j]<-1;b<-rep(1,5);b[j]<-0
 one<-results[results$variant==paste(a,collapse=""),];omit<-results[results$variant==paste(b,collapse=""),]
 data.frame(change=unname(labels[j]),one_change_variant=one$variant,category_changes_if_only_this_update=one$changed_from_friday,
 demand_flag_changes_if_only_this_update=one$demand_flags_changed_from_friday,supply_flag_changes_if_only_this_update=one$supply_flags_changed_from_friday,
 category_changes_if_undone_in_updated=omit$changed_from_updated)
}))
dir.create(out,recursive=TRUE,showWarnings=FALSE)
write.csv(results,file.path(out,"all_32_update_combinations.csv"),row.names=FALSE,na="")
write.csv(detail,file.path(out,"department_classifications_by_update_combination.csv"),row.names=FALSE,na="")
write.csv(comparisons,file.path(out,"one_update_at_a_time.csv"),row.names=FALSE)
jsonlite::write_json(list(endpoint_maximum_errors=as.list(endpoint_errors),need_n=4292,areas=82,
 baseline_category_changes=0,full_update_category_changes=sum(old_scores$category!=new_scores$category),
 interpretation="Controlled classification sensitivity, not regression or causal attribution. Effects overlap; counts from one-change variants are not additive. Every variant re-estimates its equal-department references using either Friday component-specific valid areas or the updated common complete-area reference, as indicated. Existing reports remain unchanged."),
 file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
print(comparisons,row.names=FALSE);print(results,row.names=FALSE)
