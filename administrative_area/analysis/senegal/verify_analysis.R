# Independently verify source preservation, geography, exposure assignment and fits.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-opt("--input");admin_input<-input;raw_file<-opt("--raw")
facility_file<-opt("--facility-input");data_dir<-opt("--data-dir")
library(haven);library(jsonlite);library(clubSandwich)
out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
x<-readRDS(input);admin<-readRDS(admin_input);raw<-read_dta(raw_file)
hybrid<-x
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
p<-fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables;S<-p$supply_variables
checks<-list();add<-function(name,ok,detail=""){
 if(!isTRUE(ok))stop("Verification failed: ",name," ",detail)
 checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE,detail=as.character(detail))}
same<-function(a,b)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=1e-12))
add("All original women and IDs retained",nrow(raw)==7169&&nrow(hybrid)==7169&&identical(raw$IDW,hybrid$IDW))
raw_same<-vapply(names(raw),function(k)same(raw[[k]],hybrid[[k]]),logical(1))
add("Every raw SURGO field unchanged",all(raw_same),length(raw_same))
num<-function(k)as.numeric(raw[[k]])
valid<-num("IRB1")==1&is.finite(num("WEIGHT"))&num("WEIGHT")>0
need<-valid&num("FPN2")%in%1
add("Original SURGO need flag, not a reconstructed flag",identical(need,d$need)&&sum(need)==1905)
add("Consent and positive weights",identical(valid,d$positive)&&sum(valid)==7158)
add("Original final survey weights",identical(d$weight,num("WEIGHT")))
add("Recorded census district retained",identical(d$psu,as.character(raw$RB0H)))
key<-function(v)toupper(gsub("[^A-Za-z0-9]","",iconv(as.character(v),to="ASCII//TRANSLIT")))
add("Department matches original woman label",identical(d$geo,key(as.character(haven::as_factor(raw$RB0C)))))
add("Woman residence matches original FV_MILIEU",identical(d$urban,as.numeric(num("FV_MILIEU")==2)))
add("Census districts nest in departments",all(vapply(split(d$geo[valid],d$psu[valid]),function(v)length(unique(v))==1,logical(1))))
add("Census districts nest in residence",all(vapply(split(d$urban[valid],d$psu[valid]),function(v)length(unique(v))==1,logical(1))))
fallback<-rep(FALSE,nrow(d))
add("Every source woman has department supply",all(x$link_has_supply))
add("No geographic fallback",!any(d$fallback))
add("No residence-specific supply key",all(d$source_profile==d$geo))
reconciliation<-read.csv(file.path(data_dir,"supply_input_reconciliation.csv"))
add("Every assigned and department supply component reconciled",nrow(reconciliation)==492&&all(reconciliation$passed))
add("33 department profiles with all six supply measures including counseling",nrow(frame$profiles)==33&&all(complete.cases(frame$profiles[S]))&&length(S)==6&&"counselor"%in%S)
fac<-readRDS(facility_file);fac<-fac[fac$country=="senegal",];fac$geo<-key(fac$assigned_join_area)
boolean<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
fac$offering<-boolean(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
fac$training<-as.numeric(fac$p_train);fac$counselor<-as.numeric(fac$counselor_available)
fac$larc_stock<-boolean(fac$one_larc_stockin);fac$popular_stock<-boolean(fac$mp_in_stock)
S5<-setdiff(S,"counselor")
fac$complete_supply<-complete.cases(fac[S5])
add("Known facility inventory and complete sample",sum(fac$geo%in%unique(d$geo))==1648&&sum(fac$geo%in%unique(d$geo)&fac$complete_supply)==929)
for(g in unique(d$geo)){
 ff<-fac[fac$geo==g & (p$audit$facility_sample=="all" | fac$complete_supply),]
 add(paste("Department has facility evidence",g),nrow(ff)>0,nrow(ff))
 for(k in S){v<-ff[[k]];v<-v[is.finite(v)];expected<-mean(v);observed<-frame$supply[[k]][d$geo==g]
  add(paste("All women assigned own department",g,k),all(abs(expected-observed)<=5.00001e-6),length(observed))
 }
}
add("All 46 facility departments kept for coverage",length(unique(fac$geo))==46&&nrow(fac)==2043)
add("Mbacke has one department profile for both residences",all(vapply(frame$supply[d$geo=="MBACKE",S],function(v)length(unique(v))==1,logical(1))))
add("Three-LARC indicator is constant, not secretly modeled",all(boolean(fac$three_larc_stockin)==0))
item<-function(k,pos,valid){v<-num(k);ifelse(v%in%valid,as.integer(v%in%pos),NA_real_)}
combine<-function(mat)apply(mat,1,function(v)if(any(v==1,na.rm=TRUE))1 else if(anyNA(v))NA_real_ else 0)
ind<-list(agency=item("HS7C",1,1:2),stigma=combine(cbind(item("PB7B",1:2,1:4),item("PB7C",1:2,1:4))),
 health=combine(cbind(item("PB2A",1:2,1:4),item("PB2B",1:2,1:4),item("PB2C",1:2,1:4))),
 confidence=combine(cbind(item("MC5A",3:4,1:4),item("MC5B",3:4,1:4))),opposes=item("FP1",3:4,1:4))
mp<-sapply(paste0("MP4_",letters[1:9]),num)
ind$fertility<-vapply(seq_len(nrow(mp)),function(i){v<-mp[i,]
 if(!all(v%in%0:1)||num("MP4_99")[i]!=0||!any(v==1)||v[9]==1&&any(v[1:8]==1))return(NA_real_)
 as.numeric(any(v[1:6]==1))},numeric(1))
for(k in D)add(paste("Independent demand coding",k),same(ind[[k]],d[[k]]))
unknown<-do.call(rbind,lapply(D,function(k)data.frame(variable=k,n_unresolved=sum(need&is.na(d[[k]])),
 weighted_pct=100*weighted.mean(is.na(d[[k]])[need],d$weight[need]))))
write.csv(unknown,file.path(out,"demand_unresolved_summary.csv"),row.names=FALSE)
item_unknown<-do.call(rbind,lapply(c("HS7C","PB7B","PB7C","PB2A","PB2B","PB2C","MP4_99","MC5A","MC5B","FP1"),function(k){
 v<-num(k);codes<-if(k=="MP4_99")1 else if(k=="FP1")c(5,99) else 99
 un<-v%in%codes
 data.frame(variable=k,unresolved_codes=paste(codes,collapse=";"),n=sum(need&un),blank_n=sum(need&is.na(v)),
  weighted_pct=100*weighted.mean(un[need],d$weight[need]))}))
write.csv(item_unknown,file.path(out,"unresolved_item_responses.csv"),row.names=FALSE)
prompt<-as.matrix(raw[paste0("CU5",LETTERS[1:4])])
current<-ifelse(num("CU1")%in%c(1,3)|rowSums(prompt==1,na.rm=TRUE)>0,1,
 ifelse(num("CU1")%in%2&rowSums(prompt==2,na.rm=TRUE)==4,0,NA_real_))
modern<-rowSums(as.matrix(raw[paste0("CU4_",1:8)])==1,na.rm=TRUE)>0|num("CU5C")%in%1|num("CU5D")%in%1|
 (num("CU4_11")%in%1&num("CU4_lam")%in%1)
traditional<-rowSums(as.matrix(raw[paste0("CU4_",c(9,10,12:15))])==1,na.rm=TRUE)>0|num("CU5A")%in%1|num("CU5B")%in%1
y<-ifelse(current%in%0,0,ifelse(current%in%1&modern,1,ifelse(current%in%1&traditional,0,NA_real_)))
add("Independent modern-use outcome",same(y,d$y_modern))
for(i in seq_len(nrow(p$bars))){b<-p$bars[i,];v<-d[[b$field]][need];w<-d$weight[need]
 add(paste("Weighted need-group bar",b$field),abs(sum(v*w)/sum(w)-b$estimate)<1e-10&&sum(v)==b$n&&b$denominator==1905)}
for(g in unique(d$geo))for(k in D){r<-p$heatmap[p$heatmap$geo==g&p$heatmap$variable==k&p$heatmap$side=="Demand",]
 ok<-need&d$geo==g&is.finite(ind[[k]])
 add(paste("Need-group heatmap",g,k),abs(100*weighted.mean(ind[[k]][ok],d$weight[ok])-r$value)<1e-9&&sum(ok)==r$n_valid)
}
wcorr<-function(a,b,w){ok<-is.finite(a)&is.finite(b)&is.finite(w)&w>0;a<-a[ok];b<-b[ok];w<-w[ok]/sum(w[ok]);
 a<-a-sum(a*w);b<-b-sum(b*w);sum(w*a*b)/sqrt(sum(w*a^2)*sum(w*b^2))}
corr<-list()
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")D else S;mat<-if(side=="Demand")d[need,fields]else frame$profiles[fields]
 ww<-if(side=="Demand")d$weight[need]else rep(1,nrow(mat))
 for(j in seq_along(fields))for(k in seq_along(fields))corr[[length(corr)+1L]]<-data.frame(side=side,item1=fields[j],item2=fields[k],
  correlation=wcorr(mat[[j]],mat[[k]],ww),n_pair=sum(complete.cases(mat[c(j,k)])))
}
write.csv(do.call(rbind,corr),file.path(out,"component_correlations.csv"),row.names=FALSE)
z<-sapply(D,function(k){v<-ind[[k]];ok<-need&is.finite(v);mu<-weighted.mean(v[ok],d$weight[ok]);
 sd<-sqrt(weighted.mean((v[ok]-mu)^2,d$weight[ok]));(v-mu)/sd})
personal<-rowSums(z)/6
zs<-sapply(S,function(k){r<-p$component_trace[p$component_trace$side=="Supply"&p$component_trace$variable==k,];
 (100*frame$supply[[k]]-unique(r$indicator_mean))/unique(r$indicator_SD)})
supply<- -rowSums(zs)/length(S)
pv<-readRDS(file.path(data_dir,"pca_appendix/pca_scores.rds"));max_svd<-0
for(side in c("demand","supply")){
 f<-pv[[paste0(side,"_fit")]];mat<-if(side=="demand")as.matrix(d[D])else as.matrix(frame$supply[S])
 use<-need&complete.cases(mat);w<-d$weight[use]/sum(d$weight[use])
 z0<-sweep(sweep(mat[use,,drop=FALSE],2,f$center,"-"),2,f$scale,"/")
 sv<-svd(z0*sqrt(w));err<-max(abs(sv$d^2-f$eigenvalues));max_svd<-max(max_svd,err)
 add(paste("Independent weighted SVD",side),err<1e-10,err)
 score<-drop(sweep(sweep(mat,2,f$center,"-"),2,f$scale,"/")%*%f$rotation[,1]);if(side=="supply")score<- -score
 saved<-if(side=="demand")pv$scores$demand_score else pv$scores$supply_risk
 add(paste("PCA score for every woman",side),same(score,saved))
}
previous<-NULL
for(method in c("zscore","pca")){
 folder<-if(method=="zscore")file.path(data_dir,"models_zscore")else file.path(data_dir,"pca_appendix/models")
 fits<-readRDS(file.path(folder,"fitted_models.rds"))
 for(id in names(fits)){
  ob<-fits[[id]];dd<-ob$data;ix<-dd$row
  expectedD<-if(method=="zscore")personal[ix]else pv$scores$demand_score[ix]
  expectedS<-if(method=="zscore")supply[ix]else pv$scores$supply_risk[ix]
  add(paste(method,id,"personal demand assignment"),max(abs(dd$demand_score-expectedD))<1e-12)
  add(paste(method,id,"department-only supply assignment"),max(abs(dd$supply_risk-expectedS))<1e-12)
  add(paste(method,id,"need-only common sample and modern outcome"),nrow(dd)==sum(need&complete.cases(d[c("y_modern","age10","age10_sq","education","partnered","fertility_goal","urban",D)]))&&sum(dd$y_modern)==737&&all(dd$need)&&all(dd$y_modern%in%0:1))
  add(paste(method,id,"agreed covariates including residence"),all(c("age10","age10_sq","education","partnered","fertility_goal","urban")%in%all.vars(ob$formula)))
  add(paste(method,id,"no random effects or interactions"),!grepl("[|:*]",paste(deparse(ob$formula),collapse=" ")))
  add(paste(method,id,"same women in all ten specifications"),is.null(previous)||identical(previous,ix));previous<-ix
  dd$independent_w<-dd$weight/mean(dd$weight)
  ff<-glm(ob$formula,data=dd,weights=independent_w,family=quasibinomial(),control=glm.control(maxit=100))
  add(paste(method,id,"independent coefficient reproduction"),max(abs(coef(ff)-coef(ob$fit)))<1e-10)
  V<-clubSandwich::vcovCR(ff,cluster=dd$geo,type="CR2",inverse_var=FALSE)
  add(paste(method,id,"department CR2 reproduction"),max(abs(V-ob$V))<1e-10)
 }
}
zm<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"));pm<-readRDS(file.path(data_dir,"pca_appendix/models/fitted_models.rds"))
add("Demand-indicator-only fits identical across score methods",identical(coef(zm$components_demand$fit),coef(pm$components_demand$fit)))
add("33 department-only supply scores",length(unique(zm$score_joint$data$supply_risk))==33&&length(unique(zm$score_joint$data$geo))==33&&
 all(vapply(split(zm$score_joint$data$supply_risk,zm$score_joint$data$geo),function(v)length(unique(v))==1,logical(1))))
# Recompute the primary and median-count classifications independently.
areas<-sort(unique(d$geo)); hm<-p$heatmap
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")D else S
 mat<-sapply(fields,function(k){rr<-hm[hm$side==side&hm$variable==k,];rr$value[match(areas,rr$geo)]})
 if(side=="Supply" && isTRUE(p$audit$primary_reference_fixed)){
  tt<-p$component_trace[p$component_trace$side==side,]
  zz<-sapply(seq_along(fields),function(j){rr<-tt[tt$variable==fields[j],];(mat[,j]-unique(rr$indicator_mean))/unique(rr$indicator_SD)})
 }else zz<-scale(mat)
 expected<-rowMeans(zz);rr<-p$scores[p$scores$method=="zscore",];rr<-rr[match(areas,rr$geo),]
 observed<-if(side=="Demand")rr$demand_score else rr$supply_score
 add(paste("Independent department z-score",side),max(abs(expected-observed))<1e-9)
 threshold<-p$thresholds[p$thresholds$method=="zscore",if(side=="Demand")"demand_cutoff"else"supply_cutoff"]
 flag<-if(side=="Demand")expected>threshold+1e-10 else expected<threshold-1e-10
 add(paste("Independent median classification",side),identical(flag,if(side=="Demand")rr$demand_flag else rr$supply_flag))
}
model_rows<-zm$score_joint$data$row
remaining<-need
flow<-list(data.frame(step="Original SURGO FPN2 = 1",excluded_at_step=0,remaining=sum(remaining)))
for(step in c("Demand indicators","Modern-use outcome","Partnership","Other covariates")){
 fields<-switch(step,"Demand indicators"=D,"Modern-use outcome"="y_modern","Partnership"="partnered",
  "Other covariates"=c("age10","age10_sq","education","fertility_goal","urban"))
 remove<-remaining&!complete.cases(d[fields]);remaining<-remaining&!remove
 flow[[length(flow)+1L]]<-data.frame(step=step,excluded_at_step=sum(remove),remaining=sum(remaining))
}
add("Complete-case sample flow reproduced",identical(which(remaining),model_rows))
write.csv(do.call(rbind,flow),file.path(out,"model_sample_flow.csv"),row.names=FALSE)
samples<-do.call(rbind,lapply(c(TRUE,FALSE),function(in_model){ok<-need&(d$row%in%model_rows)==in_model
 wmean<-function(v){valid<-ok&is.finite(v);weighted.mean(v[valid],d$weight[valid])}
 data.frame(group=if(in_model)"Included in common models"else"Excluded from common models",n=sum(ok),
  weighted_share_of_need=sum(d$weight[ok])/sum(d$weight[need]),modern_use_among_known=wmean(d$y_modern),
  urban_share=wmean(d$urban),fallback_share=wmean(as.numeric(d$fallback)))}))
write.csv(samples,file.path(out,"model_sample_comparison.csv"),row.names=FALSE)
# Five-domain bridge separates counseling omission from facility selection.
bridge_dir<-file.path(data_dir,"five_measure_reference")
bp<-fromJSON(file.path(bridge_dir,"report_data.json"));bf<-readRDS(file.path(bridge_dir,"analysis_frame.rds"))
add("Five-domain reference excludes only counseling",identical(bp$supply_variables,S5)&&!bp$audit$counseling_included&&bp$audit$facility_records_used==1648)
for(k in S5)add(paste("Bridge raw supply input unchanged",k),same(bf$supply[[k]],frame$supply[[k]]))
bridge_fits<-readRDS(file.path(bridge_dir,"models_zscore/fitted_models.rds"))
# Verify the complete-facility appendix on the same women and the five-domain references.
sens_dir<-file.path(data_dir,"complete_facility_sensitivity")
ss<-fromJSON(file.path(sens_dir,"report_data.json"));sf<-readRDS(file.path(sens_dir,"analysis_frame.rds"))
ssfits<-readRDS(file.path(sens_dir,"models_zscore/fitted_models.rds"))
add("Sensitivity uses exactly 929 complete facilities",ss$audit$facility_records_used==929&&ss$audit$primary_reference_fixed)
add("Complete-facility sensitivity omits counseling",identical(ss$supply_variables,S5)&&!ss$audit$counseling_included)
add("Sensitivity retains identical need group and women",identical(sf$women$need,d$need)&&identical(sf$women$id,d$id))
for(g in unique(d$geo))for(k in S5){ff<-fac[fac$geo==g&fac$complete_supply,];v<-mean(ff[[k]])
 add(paste("Complete-facility profile",g,k),all(abs(sf$supply[[k]][sf$women$geo==g]-v)<1e-12))
}
main_ref<-bp$component_trace[bp$component_trace$side=="Supply",c("geo","variable","indicator_mean","indicator_SD")]
alt_ref<-ss$component_trace[ss$component_trace$side=="Supply",c("geo","variable","indicator_mean","indicator_SD")]
add("Sensitivity holds five-domain reference means and SDs fixed",same(main_ref$indicator_mean,alt_ref$indicator_mean)&&same(main_ref$indicator_SD,alt_ref$indicator_SD))
add("Sensitivity holds five-domain score cutoffs fixed",same(bp$thresholds[bp$thresholds$method=="zscore",c("demand_cutoff","supply_cutoff")],ss$thresholds[ss$thresholds$method=="zscore",c("demand_cutoff","supply_cutoff")]))
for(group in c("bridge","complete")){
 fits<-if(group=="bridge")bridge_fits else ssfits
 ffdata<-if(group=="bridge")bf else sf
 score<- -rowMeans(sapply(S5,function(k){rr<-bp$component_trace[bp$component_trace$side=="Supply"&bp$component_trace$variable==k,];
  (100*ffdata$supply[[k]]-unique(rr$indicator_mean))/unique(rr$indicator_SD)}))
 for(id in names(fits)){
  ob<-fits[[id]];dd<-ob$data
  add(paste(group,"common model women",id),identical(dd$row,zm[[id]]$data$row))
  add(paste(group,"supply assignment",id),max(abs(dd$supply_risk-score[dd$row]))<1e-12)
  ff<-glm(ob$formula,data=dd,weights=weight/mean(weight),family=quasibinomial(),control=glm.control(maxit=100))
  add(paste(group,"coefficients independently reproduced",id),max(abs(coef(ff)-coef(ob$fit)))<1e-10)
  V<-clubSandwich::vcovCR(ff,cluster=dd$geo,type="CR2",inverse_var=FALSE)
  add(paste(group,"department covariance reproduced",id),max(abs(V-ob$V))<1e-10)
 }
 add(paste(group,"demand-only fits unchanged"),identical(coef(fits$components_demand$fit),coef(zm$components_demand$fit)))
}
# Demand complete-case summaries change the contextual map inputs, not personal coding.
demand_dir<-file.path(data_dir,"complete_demand_sensitivity")
dp<-fromJSON(file.path(demand_dir,"report_data.json"));df<-readRDS(file.path(demand_dir,"analysis_frame.rds"))
complete_need<-need&complete.cases(d[D]);add("Demand complete-case sample is 1451 women",sum(complete_need)==1451)
add("Demand sensitivity preserves all raw responses and need flags",same(df$women[D],d[D])&&identical(df$women$need,need))
for(g in unique(d$geo))for(k in D){rr<-dp$heatmap[dp$heatmap$geo==g&dp$heatmap$side=="Demand"&dp$heatmap$variable==k,]
 ok<-complete_need&d$geo==g
 add(paste("Complete-demand prevalence",g,k),rr$n_valid==sum(ok)&&abs(rr$value-100*weighted.mean(d[[k]][ok],d$weight[ok]))<1e-9)
}
add("Demand sensitivity keeps all six supply components unchanged",same(df$supply[S],frame$supply[S])&&identical(dp$supply_variables,S))
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")D else S
 value<-vapply(areas,function(g)mean(vapply(fields,function(k){r<-dp$heatmap[dp$heatmap$geo==g&dp$heatmap$side==side&dp$heatmap$variable==k,]
  ref<-p$component_trace[p$component_trace$side==side&p$component_trace$variable==k,];
  (r$value-unique(ref$indicator_mean))/unique(ref$indicator_SD)},numeric(1))),numeric(1))
 pp<-dp$scores[dp$scores$method=="zscore",];pp<-pp[match(areas,pp$geo),]
 add(paste("Complete-demand fixed-reference score",side),max(abs(value-if(side=="Demand")pp$demand_score else pp$supply_score))<1e-9)
 cut<-p$thresholds[p$thresholds$method=="zscore",if(side=="Demand")"demand_cutoff"else"supply_cutoff"]
 flag<-if(side=="Demand")value>cut+1e-10 else value<cut-1e-10
 add(paste("Complete-demand fixed threshold",side),identical(unname(flag),if(side=="Demand")pp$demand_flag else pp$supply_flag))
}
add("Common models already require complete demand",all(complete_need[zm$score_joint$data$row]))
save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
write_json(list(check_count=nrow(save),all_passed=all(save$passed),source_rows=nrow(x),need_n=sum(need),model_n=nrow(zm$score_joint$data),
 models=20,source_raw_columns_verified=length(raw_same),facility_component_checks=nrow(reconciliation),
 woman_supply_value_checks=nrow(d)*length(S),weighted_SVD_max_error=max_svd,all_models_same_women=TRUE,
 all_model_outcomes_modern=TRUE,no_random_effects=TRUE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
cat('Passed',nrow(save),'independent checks.\n')
