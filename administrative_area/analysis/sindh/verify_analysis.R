# Independent checks of raw inputs, facility means, assignments, scores and models.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
data_dir<-arg("--data-dir");input<-arg("--input");facility_input<-arg("--facility-input")
raw_facility<-arg("--raw-facility");raw_women<-arg("--raw-women");cleaned<-arg("--cleaned-facility");need_file<-arg("--need-file");aliases_file<-arg("--district-aliases")
out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));x<-readRDS(input)
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women;D<-p$demand_variables;S<-p$supply_variables
checks<-list();verify<-function(name,ok){
 if(!isTRUE(ok))stop("Verification failed: ",name)
 checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE)
}
same<-function(a,b,tol=1e-10)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=tol))
key<-function(v)toupper(gsub("[^A-Za-z0-9]","",as.character(v)))
valid_mean<-function(v){v<-as.numeric(v);if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_}
bool<-function(v)ifelse(tolower(v)%in%c("1","true","yes"),1,ifelse(tolower(v)%in%c("0","false","no"),0,NA_real_))
raw<-haven::read_dta(raw_women)
verify("Original 10364 women and IDs",nrow(x)==10364&&identical(x$col_id,raw$col_id))
for(k in names(raw))verify(paste("Original SURGO field",k),same(x[[k]],raw[[k]],0))
ref<-readRDS(need_file);nr<-match(x$col_id,ref$col_id)
exclude<-raw$sf0b%in%1|raw$sf0c%in%1|raw$sf1a%in%3|raw$sf1b%in%3|raw$sf1c%in%3|
 raw$sa0%in%2|raw$sa1%in%c(5,6,95,97)|raw$pc0%in%1|raw$sf2%in%1|raw$pc18%in%1|raw$pc19%in%2
positive<-raw$irb1%in%1&is.finite(as.numeric(raw$Normalizedweights))&raw$Normalizedweights>0
verify("Authorized need flag and unchanged policy",all(!is.na(nr[positive]))&&
 all(d$need[positive]==ref$fpn2_derived[nr[positive]])&&identical(d$need,positive&!exclude)&&sum(d$need)==3274)
verify("Original final woman weights",same(d$weight,as.numeric(raw$Normalizedweights),0))
verify("Recorded sampling clusterCode retained",identical(d$psu,as.character(raw$clusterCode)))
verify("Sampling clusters nested within district",all(vapply(split(d$geo[positive],d$psu[positive]),function(v)length(unique(v))==1,logical(1))))
verify("Every bar uses all need women",all(p$bars$denominator==3274))
verify("Raw woman district is join district",identical(key(haven::as_factor(raw$rb0c)),d$geo)&&identical(d$geo,key(x$link_geo_join)))
agency<-ifelse(raw$hs7c%in%1:2,as.numeric(raw$hs7c%in%1),NA_real_)
health<-ifelse(raw$pb2a%in%1:4,as.numeric(raw$pb2a%in%1:2),NA_real_)
verify("Health uses PB2A only",same(d$health,health))
verify("Agency uses general healthcare permission",same(d$agency,agency))
for(k in c("stigma","confidence")){
 cols<-if(k=="stigma")c("pb7b","pb7c")else c("mc5a","mc5b")
 mat<-as.matrix(raw[cols]);positive<-if(k=="stigma")mat%in%1:2 else mat%in%3:4
 dim(positive)<-dim(mat);known<-mat%in%1:4;dim(known)<-dim(mat)
 expected<-ifelse(rowSums(positive)>0,1,ifelse(rowSums(known)==2,0,NA_real_))
 verify(paste("Raw demand domain",k),same(expected,d[[k]]))
}
mp<-as.matrix(raw[paste0("mp4_",1:9)]);sel<-rowSums(mp[,1:8,drop=FALSE]==1,na.rm=TRUE)>0
none<-mp[,9]%in%1;ok<-apply(mp,1,function(v)all(v%in%0:1))&(sel|none)&!(sel&none)
fert<-ifelse(ok,as.numeric(rowSums(mp[,1:6,drop=FALSE]==1,na.rm=TRUE)>0),NA_real_)
verify("Raw coherent fertility concerns",same(fert,d$fertility))
verify("Raw opposition to FP",same(ifelse(raw$fp1%in%1:4,as.numeric(raw$fp1%in%3:4),NA_real_),d$opposes))
f<-readRDS(facility_input);f<-f[f$country=="sindh",];r<-haven::read_dta(raw_facility)
raw_id<-as.character(r$key);empty<-is.na(raw_id)|trimws(raw_id)=="";raw_id[empty]<-as.character(r$facility_id[empty])
ri<-match(f$fid,raw_id);verify("Facility raw IDs one-to-one",!anyNA(ri)&&!anyDuplicated(ri)&&length(ri)==4308&&sum(empty)==13)
f$geo<-key(f$assigned_join_area)
ali<-read.csv(aliases_file,stringsAsFactors=FALSE);expected_geo<-key(haven::as_factor(r$district[ri]))
ai<-match(expected_geo,ali$source_normalized);expected_geo[!is.na(ai)]<-ali$target_normalized[ai[!is.na(ai)]]
verify("Raw named district plus supplied aliases",identical(expected_geo,f$geo))
verify("Previously matched districts unchanged",all(f$geo[!is.na(f$geo_join_pipeline)]==key(f$geo_join_pipeline[!is.na(f$geo_join_pipeline)])))
verify("881 missing source-crosswalk facilities now located",sum(is.na(f$geo_join_pipeline))==881&&all(!is.na(f$geo)))
fp<-as.numeric(r$q506[ri]);verify("FP eligibility counts",sum(fp%in%1)==2823&&sum(fp%in%2)==1035&&sum(is.na(fp))==450)
areas<-sort(unique(d$geo[d$need]));verify("2823 linked FP providers in 30 districts",sum(fp%in%1&f$geo%in%areas)==2823&&length(areas)==30)
verify("Uncollected frequency variants not coded No",all(is.na(r$q507ab)|trimws(r$q507ab)=="")&&all(is.na(r$q507ai)|trimws(r$q507ai)==""))
# Independently collapse frequency variants; unknowns never become absent or zero.
q<-lapply(r[ri,grep("^q507a",names(r),value=TRUE)],as.numeric)
collapse<-function(cols)vapply(seq_len(nrow(f)),function(i){
 v<-vapply(cols,function(k)q[[k]][i],numeric(1));a<-v[v%in%1:4]
 if(length(a))min(a)else if(all(v%in%5))5 else NA_real_
},numeric(1))
m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah")),male_condoms=q$q507ae,
 female_condoms=q$q507af,ecp=q$q507ao,iud=collapse(c("q507aa","q507aa1","q507aa2")),implants=q$q507aj)
den<-rowSums(m>=1&m<=4,na.rm=TRUE);num<-rowSums(m==1,na.rm=TRUE)
daily<-ifelse(den>0,num/den,NA_real_)
daily_audit<-read.csv(file.path(data_dir,"daily_provision_facility_audit.csv"))
verify("Independent daily-frequency formula",same(daily_audit$daily_provision,daily[match(daily_audit$fid,f$fid)]))
f$offering<-bool(f$one_larc_3_sa_offer);f$free<-as.numeric(f$p_free_prod);f$training<-as.numeric(f$p_train)
f$larc_stock<-bool(f$one_larc_stockin);f$stockout_burden<-as.numeric(f$p_any_stockout);f$daily_provision<-daily
cl<-readr::read_csv(cleaned,show_col_types=FALSE);ci<-match(f$fid,as.character(cl$fid))
verify("Stockout source coverage",!anyNA(ci))
verify("Original cleaned stockout values preserved",same(f$stockout_burden,as.numeric(cl$p_any_stockout[ci])))
inventory<-f;f<-f[fp%in%1,]
for(k in S)for(g in areas){
 v<-f[[k]][f$geo==g];h<-p$heatmap[p$heatmap$side=="Supply"&p$heatmap$variable==k&p$heatmap$geo==g,]
 verify(paste("FP-only facility mean",g,k),same(valid_mean(v)*100,h$value))
 verify(paste("FP-only denominator",g,k),h$n_total==sum(f$geo==g)&&h$n_valid==sum(is.finite(v)))
 verify(paste("Genuine zeros retained",g,k),same(h$numerator,sum(v[is.finite(v)])))
 verify(paste("district supply assigned to every resident",g,k),same(frame$supply[[k]][d$geo==g],rep(valid_mean(v),sum(d$geo==g))))
}
for(k in D)for(g in areas){
 ii<-d$need&d$geo==g&is.finite(d[[k]]);h<-p$heatmap[p$heatmap$side=="Demand"&p$heatmap$variable==k&p$heatmap$geo==g,]
 verify(paste("Need-only weighted concern",g,k),same(h$value,100*weighted.mean(d[[k]][ii],d$weight[ii]))&&h$n_valid==sum(ii))
}
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")D else S
 mat<-sapply(fields,function(k){a<-p$heatmap[p$heatmap$side==side&p$heatmap$variable==k,];a$value[match(areas,a$geo)]})
 if(side=="Supply")mat[,"stockout_burden"]<-100-mat[,"stockout_burden"]
 zz<-scale(mat);expected<-rowMeans(zz);z<-p$scores[p$scores$method=="zscore",];z<-z[match(areas,z$geo),]
 observed<-if(side=="Demand")z$demand_score else z$supply_score
 verify(paste("Independent mean z-score",side),same(expected,observed)&&abs(mean(observed))<1e-12)
 flag<-if(side=="Demand")expected>1e-10 else expected< -1e-10
 verify(paste("Zero-mean classification",side),identical(as.logical(flag),if(side=="Demand")z$demand_flag else z$supply_flag))
 for(k in fields){t<-p$component_trace[p$component_trace$side==side&p$component_trace$variable==k,]
  verify(paste("Aligned component z",side,k),same(t$zscore,zz[match(t$geo,areas),k]))}
}
cut<-p$thresholds[p$thresholds$method=="zscore",];verify("Primary cutoffs zero",cut$demand_cutoff==0&&cut$supply_cutoff==0)
z<-p$scores[p$scores$method=="zscore",]
verify("Classification categories",identical(z$category,ifelse(z$demand_flag&z$supply_flag,"both",ifelse(z$demand_flag,"demand",ifelse(z$supply_flag,"supply","neither")))))
for(g in areas){
 plan<-p$planning[p$planning$geo==g,]
 for(side in c("Demand","Supply")){
  fields<-if(side=="Demand")D else S
  t<-p$component_trace[p$component_trace$geo==g&p$component_trace$side==side,]
  ordered<-if(side=="Demand")order(-t$zscore,match(t$variable,fields))else order(t$zscore,match(t$variable,fields))
  expected<-head(t$variable[ordered],3)
  observed<-if(side=="Demand")unlist(plan$top_demand)else unlist(plan$top_supply)
  verify(paste("Top three standardized components",g,side),identical(as.character(expected),as.character(observed)))
 }
}
previous<-NULL
for(folder in "models_zscore"){
 models<-readRDS(file.path(data_dir,folder,"fitted_models.rds"));co<-read.csv(file.path(data_dir,folder,"model_coefficients.csv"))
 tt<-read.csv(file.path(data_dir,folder,"model_block_tests.csv"))
 for(id in names(models)){
  ob<-models[[id]];aa<-ob$data;fo<-ob$formula
  verify(paste(folder,id,"common women across all five fits"),is.null(previous)||identical(previous,aa$row));previous<-aa$row
  verify(paste(folder,id,"modern use and original need"),all(aa$need)&&all(aa$y_modern%in%0:1)&&same(aa$y_modern,d$y_modern[aa$row]))
  verify(paste(folder,id,"agreed covariates"),all(c("age10","age10_sq","education","fertility_goal")%in%all.vars(fo)))
  verify(paste(folder,id,"no random effects or interactions"),!grepl("[|:*]",paste(deparse(fo),collapse=" ")))
  fit<-glm(fo,data=aa,weights=aa$weight/mean(aa$weight),family=quasibinomial(),control=glm.control(maxit=100))
  verify(paste(folder,id,"weighted coefficients"),same(coef(fit),coef(ob$fit)))
  V<-clubSandwich::vcovCR(fit,cluster=aa$geo,type="CR2",inverse_var=FALSE)
  verify(paste(folder,id,"district CR2 covariance"),same(as.vector(V),as.vector(ob$V)))
  ct<-as.data.frame(clubSandwich::coef_test(fit,vcov=V,test="Satterthwaite"))
  saved_co<-co[co$model==id,];saved_co<-saved_co[match(rownames(ct),saved_co$term),]
  verify(paste(folder,id,"Satterthwaite odds-ratio intervals"),
   same(saved_co$or_lo,exp(ct$beta-qt(.975,ct$df_Satt)*ct$SE))&&
   same(saved_co$or_hi,exp(ct$beta+qt(.975,ct$df_Satt)*ct$SE)))
  for(block in tt$block[tt$model==id]){
   fields<-if(block=="Supply")"supply_risk"else if(grepl("^score",id))"demand_score"else D
   K<-diag(length(coef(fit)))[match(fields,names(coef(fit))),,drop=FALSE]
   t<-clubSandwich::Wald_test(fit,constraints=K,vcov=V,test="HTZ");saved<-tt[tt$model==id&tt$block==block,]
   verify(paste(folder,id,block,"HTZ block test"),abs(t$p_val-saved$p)<1e-10&&abs(t$df_denom-saved$df_denom)<1e-9)
  }
 }
}
models<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"));aa<-models$score_joint$data
refs<-read.csv(file.path(data_dir,"models_zscore/individual_demand_score_reference.csv"))
zz<-sapply(D,function(k){v<-d[[k]];ii<-d$need&is.finite(v);mu<-weighted.mean(v[ii],d$weight[ii]);sd<-sqrt(weighted.mean((v[ii]-mu)^2,d$weight[ii]));(v-mu)/sd})
verify("Personal demand score from own six answers",same(aa$demand_score,rowMeans(zz)[aa$row]))
context_score<-z$supply_score[match(aa$geo,z$geo)]
verify("Supply risk is negative district readiness",same(aa$supply_risk,-context_score))
verify("30 distinct district supply profiles",length(unique(aa$supply_risk))==30)
verify("Only z-score geographic scoring",identical(unique(p$scores$method),"zscore"))
verify("No zero-coded missing supply",all(complete.cases(frame$supply)))
expected_common<-which(d$need&complete.cases(d[c("y_modern","age10","age10_sq","education","fertility_goal",D)]))
verify("Common model rows independently recovered",identical(aa$row,expected_common))
verify("Constant partnership and absent rural/urban",all(aa$partnered[is.finite(aa$partnered)]==1)&&!"urban"%in%names(d))
# Rebuild current-use classification from original responses independently.
prompts<-as.matrix(raw[paste0("cu5",letters[1:4])])
current<-ifelse(raw$cu1%in%c(1,3)|rowSums(prompts==1,na.rm=TRUE)>0,1,
 ifelse(raw$cu1%in%2&rowSums(prompts==2,na.rm=TRUE)==4,0,NA_real_))
modern<-rowSums(as.matrix(raw[paste0("cu4_",1:8)])==1,na.rm=TRUE)>0|raw$cu5c%in%1|raw$cu5d%in%1|(raw$cu4_11%in%1&raw$cu4_lam%in%1)
traditional<-rowSums(as.matrix(raw[paste0("cu4_",c(9,10,12:15))])==1,na.rm=TRUE)>0|raw$cu5a%in%1|raw$cu5b%in%1
y<-ifelse(current%in%0,0,ifelse(current%in%1&modern,1,ifelse(current%in%1&traditional,0,NA_real_)))
verify("Raw modern-use outcome independently reproduced",same(y,d$y_modern))
for(id in names(models)){
 ob<-models[[id]];tt<-read.csv(file.path(data_dir,"models_zscore/model_block_tests.csv"));tt<-tt[tt$model==id,]
 verify(paste(id,"within-model Holm correction"),same(tt$holm_p,p.adjust(tt$p,"holm")))
 if(id%in%c("score_joint","components_joint"))verify(paste(id,"two block tests"),nrow(tt)==2)
}
gaps<-read.csv(file.path(data_dir,"models_zscore/individual_demand_probability_gaps.csv"))
ob<-models$components_joint
for(k in D){
 a0<-ob$data;a1<-ob$data;a0[[k]]<-0;a1[[k]]<-1
 p0<-weighted.mean(predict(ob$fit,newdata=a0,type="response"),a0$weight)
 p1<-weighted.mean(predict(ob$fit,newdata=a1,type="response"),a1$weight)
 saved<-gaps[gaps$term==k,]
 verify(paste("Independent weighted probability contrast",k),same(p0,saved$probability_absent)&&
  same(p1,saved$probability_present)&&same(p1-p0,saved$estimate))
}
save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
jsonlite::write_json(list(all_passed=all(save$passed),check_count=nrow(save),source_rows=10364,need_n=3274,
 fp_facilities=2823,linked_fp_facilities=2823,model_n=nrow(aa),models=5,
 fp_providers_only=TRUE,health_PB2A_only=TRUE,primary_cutoffs_zero=TRUE,all_models_same_women=TRUE,
 no_random_effects=TRUE,limitations="Implementation verification; not causal validation, full survey-design reconstruction or a formal power analysis."),
 file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Passed",nrow(save),"independent checks.\n")
