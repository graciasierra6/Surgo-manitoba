# Independently verify source preservation, geography, exposure assignment and fits.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-opt("--input");admin_input<-opt("--admin-input");raw_file<-opt("--raw")
facility_file<-opt("--facility-input");data_dir<-opt("--data-dir")
library(haven);library(jsonlite);library(clubSandwich)
out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
x<-readRDS(input);admin<-readRDS(admin_input);raw<-read_dta(raw_file)
hybrid<-readRDS(file.path(data_dir,"senegal_women_manitoba_hybrid.rds"))
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
fallback<-!x$link_has_supply;ii<-match(x$IDW,admin$IDW)
add("Fallback only when exact profile absent",identical(hybrid$analysis_supply_fallback,fallback)&&sum(fallback)==1815)
add("Fallback retains same department",all(x$link_geo_join==admin$link_geo_join[ii]))
add("All fallback women are Rural",all(d$residence[fallback]=="Rural"))
add("Need-group fallback count",sum(fallback&need)==522)
add("Every woman now has a geographic profile",all(hybrid$analysis_has_supply))
added<-setdiff(intersect(names(x),names(admin)),names(raw));added<-added[!grepl("^link_",added)]
for(k in added){
 add(paste("Exact-match values preserved",k),same(hybrid[[k]][!fallback],x[[k]][!fallback]))
 add(paste("Department fallback values",k),same(hybrid[[k]][fallback],admin[[k]][ii[fallback]]))
}
reconciliation<-read.csv(file.path(data_dir,"supply_input_reconciliation.csv"))
add("Every assigned and department supply component reconciled",nrow(reconciliation)==492&&all(reconciliation$passed))
add("49 context profiles with complete supply",nrow(frame$assigned_profiles)==49&&all(complete.cases(frame$assigned_profiles[S])))
fac<-readRDS(facility_file);fac<-fac[fac$country=="senegal",];fac$geo<-key(fac$assigned_join_area)
boolean<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
fac$offering<-boolean(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
fac$training<-as.numeric(fac$p_train);fac$counselor<-as.numeric(fac$counselor_available)
fac$larc_stock<-boolean(fac$one_larc_stockin);fac$popular_stock<-boolean(fac$mp_in_stock)
for(j in seq_len(nrow(d))){
 subset_fac<-fac$geo==d$geo[j]
 if(!fallback[j])subset_fac<-subset_fac&!is.na(fac$rurb)&fac$rurb==d$residence[j]
 ff<-fac[subset_fac,]
 if(!nrow(ff))stop("Assigned profile has no facility records")
 for(k in S){v<-ff[[k]];v<-v[is.finite(v)];expected<-if(length(v))mean(v)else NA_real_;observed<-frame$supply[[k]][j]
  if(is.na(expected)||is.na(observed)){if(!(is.na(expected)&&is.na(observed)))stop("Missingness differs")}
  else if(abs(expected-observed)>5.00001e-6)stop("Woman supply profile mismatch")
 }
}
add("Every woman's six assigned measures independently recomputed",TRUE,nrow(d)*6)
add("All 46 facility departments kept for coverage",length(unique(fac$geo))==46&&nrow(fac)==2043)
add("Mbacke counts and fallback rule",sum(fac$geo=="MBACKE")==64&&all(fac$rurb[fac$geo=="MBACKE"]=="Urban")&&
 sum(need&d$geo=="MBACKE"&d$residence=="Rural")==56&&sum(fallback&need&d$geo=="MBACKE")==56)
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
supply<- -rowSums(zs)/6
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
  add(paste(method,id,"residence or flagged fallback supply assignment"),max(abs(dd$supply_risk-expectedS))<1e-12)
  add(paste(method,id,"need-only common sample and modern outcome"),nrow(dd)==1445&&sum(dd$y_modern)==737&&all(dd$need)&&all(dd$y_modern%in%0:1))
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
add("39 distinct supply scores, 49 assigned contexts, 33 departments",length(unique(zm$score_joint$data$supply_risk))==39&&
 nrow(unique(zm$score_joint$data[c("geo","residence")]))==49&&length(unique(zm$score_joint$data$geo))==33)
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
save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
write_json(list(check_count=nrow(save),all_passed=all(save$passed),source_rows=nrow(x),need_n=sum(need),model_n=1445,
 models=10,source_raw_columns_verified=length(raw_same),facility_component_checks=nrow(reconciliation),
 woman_supply_value_checks=nrow(d)*6,weighted_SVD_max_error=max_svd,all_models_same_women=TRUE,
 all_model_outcomes_modern=TRUE,no_random_effects=TRUE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
cat('Passed',nrow(save),'independent checks.\n')
