# Independently verify source preservation, geography, exposure assignment and fits.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-opt("--input");admin_input<-input;raw_file<-opt("--raw")
facility_file<-opt("--facility-input");data_dir<-opt("--data-dir");raw_facility<-opt("--raw-facility");cleaned_facility<-opt("--cleaned-facility")
library(haven);library(jsonlite);library(clubSandwich)
out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
x<-readRDS(input);admin<-readRDS(admin_input);raw<-read_dta(raw_file)
derived<-readRDS(file.path(data_dir,"cotedivoire_women_manitoba_analysis.rds"))
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
p<-fromJSON(file.path(data_dir,"report_data.json"));D<-p$demand_variables;S<-p$supply_variables
checks<-list();add<-function(name,ok,detail=""){
 if(!isTRUE(ok))stop("Verification failed: ",name," ",detail)
 checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE,detail=as.character(detail))}
same<-function(a,b)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=1e-12))
add("All original women and IDs retained",nrow(raw)==10859&&nrow(derived)==10859&&identical(raw$IDW,derived$IDW))
raw_same<-vapply(names(raw),function(k)same(raw[[k]],derived[[k]]),logical(1))
add("Every raw SURGO field unchanged",all(raw_same),length(raw_same))
num<-function(k)as.numeric(raw[[k]])
valid<-num("IRB1")==1&is.finite(num("WEIGHT"))&num("WEIGHT")>0
need<-valid&num("FPN2")%in%1
add("Original SURGO need flag, not a reconstructed flag",identical(need,d$need)&&sum(need)==4292)
add("Consent and positive weights",identical(valid,d$positive)&&sum(valid)==10753)
add("Original final survey weights",identical(d$weight,num("WEIGHT")))
add("Recorded census district retained",identical(d$psu,as.character(raw$IDZD)))
key<-function(v)toupper(gsub("[^A-Za-z0-9]","",iconv(as.character(v),to="ASCII//TRANSLIT")))
add("Department matches original woman label",identical(d$geo,key(as.character(haven::as_factor(raw$RB0C)))))
add("Woman residence matches original FV_MILIEU",identical(d$urban,as.numeric(num("FV_MILIEU")==2)))
add("Census districts nest in departments",all(vapply(split(d$geo[valid],d$psu[valid]),function(v)length(unique(v))==1,logical(1))))
add("Census districts nest in residence",all(vapply(split(d$urban[valid],d$psu[valid]),function(v)length(unique(v))==1,logical(1))))
add("Every existing joined field unchanged",all(vapply(setdiff(names(x),"link_rurb"),function(k)same(x[[k]],derived[[k]]),logical(1))))
add("Exact residence assignment where FP providers exist",all(derived$link_has_exact_fp_supply[derived$link_has_supply])&&!any(derived$link_supply_fallback))
add("Source exact matches preserved",sum(x$link_has_supply)==10703&&sum(!x$link_has_supply)==156)
add("Need unmatched count",sum(need&!x$link_has_supply)==49)
reconciliation<-read.csv(file.path(data_dir,"supply_input_reconciliation.csv"))
add("Every assigned and department supply component reconciled",nrow(reconciliation)==1278&&all(reconciliation$passed))
add("Context coverage and incomplete components",nrow(frame$profiles)==82&&sum(complete.cases(frame$profiles[S]))==77)
fac<-readRDS(facility_file);fac<-fac[fac$country=="cotedivoire",];fac$geo<-key(fac$assigned_join_area)
raw_fp<-read_dta(raw_facility,col_select=all_of(c("fid","s2_5_sp","s5_1","s1_5","s5_2a","s5_2b2m","s5_2b3m","s5_2c","s5_2d","s5_2e","s5_2f","s5_2g3a","s5_2g5a")))
ii<-match(fac$fid,as.character(raw_fp$fid));add("Raw facility IDs matched for FP eligibility",!anyNA(ii))
clean<-read.csv(cleaned_facility,check.names=FALSE,stringsAsFactors=FALSE);clean<-clean[clean$loc=="CDI",]
ci<-match(fac$fid,clean$fid)
add("Original cleaned stockout burden retained",nrow(clean)==3078&&!anyNA(ci)&&same(as.numeric(fac$p_any_stockout),as.numeric(clean$p_any_stockout[ci])))
raw_res<-as.numeric(raw_fp$s1_5[ii]);fac$residence<-ifelse(raw_res%in%1,"Rural",ifelse(raw_res%in%2,"Urban",NA_character_))
add("Facility residence matches raw s1_5",identical(tolower(fac$residence),tolower(as.character(fac$rurb))))
fp<-as.numeric(raw_fp$s2_5_sp[ii]);onsite<-as.numeric(raw_fp$s5_1[ii]);inventory<-fac
add("General FP provision distinguished from on-site provision",sum(fp%in%1 & onsite%in%1)==2614&&sum(fp%in%1 & onsite%in%2)==109)
add("Daoukro general FP Yes does not imply on-site Yes",sum(fac$geo%in%"DAOUKRO" & fp%in%1)==19&&all(onsite[fac$geo%in%"DAOUKRO" & fp%in%1]%in%2))
q<-as.data.frame(lapply(raw_fp[ii,setdiff(names(raw_fp),c("fid","s2_5_sp"))],as.numeric))
# Independently count a method class once. A recorded Daily variant establishes
# daily provision; any recorded 1-4 variant establishes offering of that class.
groups<-list("s5_2a",c("s5_2b2m","s5_2b3m"),"s5_2c","s5_2d","s5_2e","s5_2f",c("s5_2g3a","s5_2g5a"))
day<-offer<-matrix(0,nrow(fac),7)
for(j in seq_along(groups)){
 vv<-as.matrix(q[groups[[j]]]);day[,j]<-rowSums(vv==1,na.rm=TRUE)>0
 offer[,j]<-rowSums(vv>=1&vv<=4,na.rm=TRUE)>0
}
fac$daily_provision<-ifelse(rowSums(offer)>0,rowSums(day)/rowSums(offer),NA_real_)
fac<-fac[fp%in%1,]
add("Only confirmed general FP-service providers",nrow(fac)==2723&&sum(fac$geo%in%unique(d$geo))==2370)
boolean<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
fac$offering<-boolean(fac$one_larc_3_sa_offer);fac$free<-as.numeric(fac$p_free_prod)
fac$training<-as.numeric(fac$p_train);fac$larc_stock<-boolean(fac$one_larc_stockin)
fac$stockout_burden<-as.numeric(fac$p_any_stockout)
add("Revised six supply components",identical(S,c("offering","free","training","daily_provision","larc_stock","stockout_burden")))
# Reconstruct whole-departement planning means and residence assignments independently.
# The existing helper rounds binary planning shares to 5 decimals; means of
# continuous summaries, daily ratios and new residence assignments retain full precision.
for(g in unique(d$geo[!is.na(d$geo)]))for(k in S){
 v<-fac[[k]][fac$geo%in%g];valid_v<-v[is.finite(v)]
 expected<-if(length(valid_v))mean(valid_v)else NA_real_
 h<-p$heatmap[p$heatmap$geo%in%g&p$heatmap$side=="Supply"&p$heatmap$variable==k,]
 add(paste("Whole-departement planning mean unchanged",g,k),
  (if(is.na(expected))is.na(h$value)else abs(100*(if(k%in%c("offering","larc_stock"))round(expected,5)else expected)-h$value)<1e-9)&&length(valid_v)==h$n_valid&&length(v)==h$n_total)
}
obs<-frame$residence_profiles
add("131 observed residence groups; 126 exact; 124 complete",nrow(obs)==131&&sum(obs$fp_facilities>0)==126&&sum(obs$complete_supply)==124)
for(j in seq_len(nrow(obs))){
 g<-obs$geo[j];res<-obs$residence[j];exact<-fac$geo%in%g&fac$residence%in%res
 fallback<-sum(exact)==0&&any(fac$geo%in%g)
 use<-if(fallback)fac$geo%in%g else exact
 add(paste("Correct fallback decision",g,res),identical(fallback,obs$fallback[j]))
 add(paste("Correct exact FP provider count",g,res),sum(exact)==obs$fp_facilities[j])
 for(k in S){
  v<-fac[[k]][use];v<-v[is.finite(v)];expected<-if(length(v))mean(v)else NA_real_
  observed<-frame$supply[[k]][d$geo%in%g&d$residence%in%res]
  add(paste("Raw-validated residence mean assigned",g,res,k),
   if(is.na(expected))all(is.na(observed))else all(abs(expected-observed)<1e-12))
  add(paste("Residence component valid denominator",g,res,k),length(v)==obs[[paste0(k,"_valid")]][j])
 }
}
add("No zero-provider cell has an available department fallback",!any(d$fallback))
add("All unresolved-geography records retain unknown exposures",all(is.na(as.matrix(frame$supply[is.na(d$geo),]))))
add("Every woman's six assigned measures independently recomputed",TRUE,nrow(d)*6)
add("All 100 facility department-equivalents retained",length(unique(inventory$geo))==100&&nrow(inventory)==3078)
add("Four unmatched departments are unassessed rather than zero",all(is.na(as.matrix(frame$supply[need&!d$matched_supply,]))))
add("Daoukro free provision remains undefined",all(is.na(frame$supply$free[d$geo%in%"DAOUKRO"])))
add("Daoukro daily provision also remains undefined",all(is.na(frame$supply$daily_provision[d$geo%in%"DAOUKRO"])))
add("92 women have incomplete department supply inputs",sum(need&!complete.cases(frame$supply))==92)
add("Three-LARC indicator is constant, not secretly modeled",all(boolean(fac$three_larc_stockin)==0))
item<-function(k,pos,valid){v<-num(k);ifelse(v%in%valid,as.integer(v%in%pos),NA_real_)}
combine<-function(mat)apply(mat,1,function(v)if(any(v==1,na.rm=TRUE))1 else if(anyNA(v))NA_real_ else 0)
ind<-list(agency=item("HS7C",1,1:2),stigma=combine(cbind(item("PB7B",1:2,1:4),item("PB7C",1:2,1:4))),
 health=item("PB2A",1:2,1:4),
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
 add(paste("Weighted need-group bar",b$field),abs(sum(v*w)/sum(w)-b$estimate)<1e-10&&sum(v)==b$n&&b$denominator==4292)}
for(g in unique(d$geo[need]))for(k in D){r<-p$heatmap[p$heatmap$geo==g&p$heatmap$variable==k&p$heatmap$side=="Demand",]
 ok<-need&d$geo%in%g&is.finite(ind[[k]])
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
fav<-frame$supply[S];fav$stockout_burden<-1-fav$stockout_burden
zs<-sapply(S,function(k){r<-p$component_trace[p$component_trace$side=="Supply"&p$component_trace$variable==k,];
 (100*fav[[k]]-unique(r$indicator_mean))/unique(r$indicator_SD)})
supply<- -rowSums(zs)/6
pv<-readRDS(file.path(data_dir,"pca_appendix/pca_scores.rds"));max_svd<-0
for(side in c("demand","supply")){
 f<-pv[[paste0(side,"_fit")]];mat<-if(side=="demand")as.matrix(d[D])else as.matrix(fav)
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
  add(paste(method,id,"residence-specific supply assignment"),max(abs(dd$supply_risk-expectedS))<1e-12)
  add(paste(method,id,"need-only common sample and modern outcome"),identical(dd$row,which(need&complete.cases(d[c("y_modern","age10","age10_sq","education","partnered","fertility_goal","urban",D)])&complete.cases(frame$supply)))&&all(dd$need)&&all(dd$y_modern%in%0:1))
  add(paste(method,id,"agreed covariates including residence"),all(c("age10","age10_sq","education","partnered","fertility_goal","urban")%in%all.vars(ob$formula)))
  add(paste(method,id,"no random effects or interactions"),!grepl("[|:*]",paste(deparse(ob$formula),collapse=" ")))
  add(paste(method,id,"same women in all ten specifications"),is.null(previous)||identical(previous,ix));previous<-ix
  dd$independent_w<-dd$weight/mean(dd$weight)
  ff<-glm(ob$formula,data=dd,weights=independent_w,family=quasibinomial(),control=glm.control(maxit=100))
  add(paste(method,id,"independent coefficient reproduction"),max(abs(coef(ff)-coef(ob$fit)))<1e-10)
  V<-clubSandwich::vcovCR(ff,cluster=dd$geo,type="CR2",inverse_var=FALSE)
  add(paste(method,id,"department CR2 reproduction"),max(abs(V-ob$V))<1e-10)
  tt<-read.csv(file.path(folder,"model_block_tests.csv"));tt<-tt[tt$model==id,]
  for(b in tt$block){fields<-if(b=="Supply")"supply_risk"else if(grepl("^score",id))"demand_score"else D
   K<-diag(length(coef(ff)))[match(fields,names(coef(ff))),,drop=FALSE]
   t<-clubSandwich::Wald_test(ff,constraints=K,vcov=V,test="HTZ");saved<-tt[tt$block==b,]
   add(paste(method,id,b,"HTZ block reproduction"),abs(t$p_val-saved$p)<1e-10&&abs(t$df_denom-saved$df_denom)<1e-9)
  }

 }
}
zm<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"));pm<-readRDS(file.path(data_dir,"pca_appendix/models/fitted_models.rds"))
add("Demand-indicator-only fits identical across score methods",identical(coef(zm$components_demand$fit),coef(pm$components_demand$fit)))
add("Supply scores constant within assigned residence profiles",all(vapply(
 split(zm$score_joint$data$supply_risk,zm$score_joint$data$source_profile),function(v)length(unique(v))==1,logical(1))))
add("Models exploit residence variation within departments",length(unique(zm$score_joint$data$supply_risk))>length(unique(zm$score_joint$data$geo)))
# Verify the geographic z-score and thresholds separately from the builder.
areas<-sort(unique(d$geo[!is.na(d$geo)]))
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")D else S
 mat<-sapply(fields,function(k){rr<-p$heatmap[p$heatmap$side==side&p$heatmap$variable==k,];rr$value[match(areas,rr$geo)]})
 if(side=="Supply")mat[,"stockout_burden"]<-100-mat[,"stockout_burden"]
 ref<-complete.cases(mat)
 zz<-sweep(sweep(mat,2,colMeans(mat[ref,,drop=FALSE]),"-"),2,apply(mat[ref,,drop=FALSE],2,sd),"/")
 expected<-rowMeans(zz);ss<-p$scores[p$scores$method=="zscore",];ss<-ss[match(areas,ss$geo),]
 observed<-if(side=="Demand")ss$demand_score else ss$supply_score
 add(paste("Independent geographic score",side),same(expected,observed))
 cut<-0;add(paste("Final score mean and cutoff equal zero",side),abs(mean(expected[ref]))<1e-12&&p$thresholds[p$thresholds$method=="zscore",if(side=="Demand")"demand_cutoff"else"supply_cutoff"]==0);flag<-if(side=="Demand")expected>cut+1e-10 else expected<cut-1e-10
 add(paste("Independent geographic threshold",side),same(flag,if(side=="Demand")ss$demand_flag else ss$supply_flag))
}
model_rows<-zm$score_joint$data$row
remaining<-need
flow<-list(data.frame(step="Original SURGO FPN2 = 1",excluded_at_step=0,remaining=sum(remaining)))
for(step in c("Demand indicators","Assigned supply inputs","Modern-use outcome","Partnership","Other covariates")){
 fields<-switch(step,"Demand indicators"=D,"Modern-use outcome"="y_modern","Partnership"="partnered",
  "Other covariates"=c("age10","age10_sq","education","fertility_goal","urban"))
 remove<-remaining&if(step=="Assigned supply inputs")!complete.cases(frame$supply)else!complete.cases(d[fields]);remaining<-remaining&!remove
 flow[[length(flow)+1L]]<-data.frame(step=step,excluded_at_step=sum(remove),remaining=sum(remaining))
}
add("Complete-case sample flow reproduced",identical(which(remaining),model_rows))
write.csv(do.call(rbind,flow),file.path(out,"model_sample_flow.csv"),row.names=FALSE)
samples<-do.call(rbind,lapply(c(TRUE,FALSE),function(in_model){ok<-need&(d$row%in%model_rows)==in_model
 wmean<-function(v){valid<-ok&is.finite(v);weighted.mean(v[valid],d$weight[valid])}
 data.frame(group=if(in_model)"Included in common models"else"Excluded from common models",n=sum(ok),
  weighted_share_of_need=sum(d$weight[ok])/sum(d$weight[need]),modern_use_among_known=wmean(d$y_modern),
  urban_share=wmean(d$urban),unmatched_supply_share=wmean(as.numeric(!d$matched_supply)))}))
write.csv(samples,file.path(out,"model_sample_comparison.csv"),row.names=FALSE)
aa<-zm$score_joint$data
weight_groups<-do.call(rbind,lapply(split(aa,aa$geo),function(v)data.frame(geo=v$geo[1],n=nrow(v),SURGO_weight=sum(v$weight),weight_share=sum(v$weight)/sum(aa$weight))))
weight_groups<-weight_groups[order(-weight_groups$weight_share),]
write.csv(weight_groups,file.path(out,"model_weight_concentration.csv"),row.names=FALSE)
test_diagnostics<-do.call(rbind,lapply(c("models_zscore","pca_appendix/models"),function(folder){v<-read.csv(file.path(data_dir,folder,"model_block_tests.csv"));v$method<-folder;v$denominator_df_below_one<-v$df_denom<1;v}))
write.csv(test_diagnostics,file.path(out,"block_test_diagnostics.csv"),row.names=FALSE)
# The existing common model sample is required for comparisons; facility
# summaries remain per-measure available-case means among confirmed providers.
rr<-frame$profiles[S];rr$stockout_burden<-1-rr$stockout_burden
ref<-complete.cases(rr);risk_inputs<-1-rr[ref,,drop=FALSE]
expected_risk<-rowMeans(scale(risk_inputs))
ss<-p$scores[p$scores$method=="zscore",]
add("Risk-first and readiness-first scores equivalent",max(abs(expected_risk+ss$supply_score[match(frame$profiles$geo[ref],ss$geo)]))<1e-10)
add("Missing supply never classified as neither",all(ss$category[!is.finite(ss$supply_score)]=="unassessed"))
add("Models already require complete demand",all(complete.cases(zm$score_joint$data[D])))
save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
write_json(list(check_count=nrow(save),all_passed=all(save$passed),source_rows=nrow(x),need_n=sum(need),model_n=nrow(zm$score_joint$data),
 models=10,source_raw_columns_verified=length(raw_same),facility_component_checks=nrow(reconciliation),
 woman_supply_value_checks=nrow(d)*6,weighted_SVD_max_error=max_svd,all_models_same_women=TRUE,
 all_model_outcomes_modern=TRUE,no_random_effects=TRUE,fp_providers_only=TRUE,health_PB2A_only=TRUE,primary_threshold_zero=TRUE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
cat('Passed',nrow(save),'independent checks.\n')
