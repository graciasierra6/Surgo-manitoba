# Independent checks of raw inputs, facility means, assignments, scores and models.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
data_dir<-arg("--data-dir");input<-arg("--input");facility_input<-arg("--facility-input")
raw_facility<-arg("--raw-facility");raw_women<-arg("--raw-women");cleaned<-arg("--cleaned-facility")
out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));x<-readRDS(input)
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women;D<-p$demand_variables;S<-p$supply_variables
checks<-list();verify<-function(name,ok){
 if(!isTRUE(ok))stop("Verification failed: ",name)
 checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE)
}
same<-function(a,b,tol=1e-10)isTRUE(all.equal(as.vector(a),as.vector(b),check.attributes=FALSE,tolerance=tol))
key<-function(v){v<-toupper(gsub("[^A-Za-z0-9]","",as.character(v)));v[v%in%"IFAKOIJAIYE"]<-"IFAKOIJAYE";v}
valid_mean<-function(v){v<-as.numeric(v);if(any(is.finite(v)))mean(v[is.finite(v)])else NA_real_}
bool<-function(v)ifelse(tolower(v)%in%c("1","true","yes"),1,ifelse(tolower(v)%in%c("0","false","no"),0,NA_real_))
raw<-readr::read_csv(raw_women,show_col_types=FALSE,guess_max=20000,name_repair="minimal",na=c("","NA"))
verify("Original 4555 women and IDs",nrow(x)==4555&&identical(x$uid,raw$uid))
for(k in names(raw))verify(paste("Original SURGO field",k),same(x[[k]],raw[[k]],0))
verify("Original SURGO need status",identical(d$need,as.numeric(raw$fpn2)%in%1)&&sum(d$need)==2159)
verify("Original final woman weights",same(d$weight,raw$weight,0))
verify("Every bar uses all need women",all(p$bars$denominator==2159))
verify("Raw woman LGA is the join LGA",identical(key(raw$lga),d$geo)&&identical(d$geo,key(x$link_geo_join)))
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
mp<-as.matrix(raw[paste0("mp4_",letters[1:9])]);sel<-rowSums(mp[,1:8,drop=FALSE]==1,na.rm=TRUE)>0
none<-mp[,9]%in%1;ok<-apply(mp,1,function(v)all(v%in%0:1))&raw$mp4_99%in%0&(sel|none)&!(sel&none)
fert<-ifelse(ok,as.numeric(rowSums(mp[,1:6,drop=FALSE]==1,na.rm=TRUE)>0),NA_real_)
verify("Raw coherent fertility concerns",same(fert,d$fertility))
verify("Raw opposition to FP",same(ifelse(raw$fp1%in%1:4,as.numeric(raw$fp1%in%3:4),NA_real_),d$opposes))
f<-readRDS(facility_input);f<-f[f$country=="lagos",];r<-haven::read_dta(raw_facility)
ri<-match(f$fid,as.character(r$key));verify("Facility raw IDs one-to-one",!anyNA(ri)&&!anyDuplicated(ri)&&length(ri)==1741)
f$geo<-key(f$assigned_join_area)
verify("Raw facility LGA assignments",identical(key(haven::as_factor(r$district_name[ri])),f$geo))
fp<-as.numeric(r$q506[ri]);verify("FP eligibility counts",sum(fp%in%1)==1040&&sum(fp%in%2)==465&&sum(is.na(fp))==236)
areas<-sort(unique(d$geo[d$need]));verify("882 linked FP providers",sum(fp%in%1&f$geo%in%areas)==882)
exception<-f$geo=="IBEJULEKKI"&f$rurb%in%"Urban"
verify("Previous zero exception excluded by FP eligibility",sum(exception)==1&&fp[exception]==2&&is.na(f$p_free_prod[exception]))
# Independently collapse frequency variants; unknowns never become absent or zero.
q<-lapply(r[ri,grep("^q507a",names(r),value=TRUE)],as.numeric)
collapse<-function(cols)vapply(seq_len(nrow(f)),function(i){
 v<-vapply(cols,function(k)q[[k]][i],numeric(1));a<-v[v%in%1:4]
 if(length(a))min(a)else if(all(v%in%5))5 else NA_real_
},numeric(1))
m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah","q507ai")),male_condoms=q$q507ae,
 female_condoms=q$q507af,ecp=q$q507ao,iud=collapse(c("q507aa","q507ab")),implants=q$q507aj)
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
 verify(paste("LGA supply assigned to every resident",g,k),same(frame$supply[[k]][d$geo==g],rep(valid_mean(v),sum(d$geo==g))))
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
previous<-NULL
for(folder in c("models_zscore","pca_appendix/models")){
 models<-readRDS(file.path(data_dir,folder,"fitted_models.rds"));co<-read.csv(file.path(data_dir,folder,"model_coefficients.csv"))
 tt<-read.csv(file.path(data_dir,folder,"model_block_tests.csv"))
 for(id in names(models)){
  ob<-models[[id]];aa<-ob$data;fo<-ob$formula
  verify(paste(folder,id,"common women across all ten fits"),is.null(previous)||identical(previous,aa$row));previous<-aa$row
  verify(paste(folder,id,"modern use and original need"),all(aa$need)&&all(aa$y_modern%in%0:1)&&same(aa$y_modern,d$y_modern[aa$row]))
  verify(paste(folder,id,"agreed covariates"),all(c("age10","age10_sq","education","partnered","fertility_goal","urban")%in%all.vars(fo)))
  verify(paste(folder,id,"no random effects or interactions"),!grepl("[|:*]",paste(deparse(fo),collapse=" ")))
  fit<-glm(fo,data=aa,weights=aa$weight/mean(aa$weight),family=quasibinomial(),control=glm.control(maxit=100))
  verify(paste(folder,id,"weighted coefficients"),same(coef(fit),coef(ob$fit)))
  V<-clubSandwich::vcovCR(fit,cluster=aa$geo,type="CR2",inverse_var=FALSE)
  verify(paste(folder,id,"LGA CR2 covariance"),same(as.vector(V),as.vector(ob$V)))
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
verify("Supply risk is negative LGA readiness",same(aa$supply_risk,-context_score))
verify("17 distinct LGA supply profiles",length(unique(aa$supply_risk))==17)
pca<-readRDS(file.path(data_dir,"pca_appendix/pca_scores.rds"))
for(side in c("demand","supply")){
 fit<-pca[[paste0(side,"_fit")]];mat<-if(side=="demand")as.matrix(d[D])else as.matrix(frame$supply[S])
 if(side=="supply")mat[,"stockout_burden"]<-1-mat[,"stockout_burden"]
 ii<-d$need&complete.cases(mat);w<-d$weight[ii]/sum(d$weight[ii]);mu<-colSums(mat[ii,,drop=FALSE]*w)
 sd<-sqrt(colSums(sweep(mat[ii,,drop=FALSE],2,mu,"-")^2*w));Z<-sweep(sweep(mat[ii,,drop=FALSE],2,mu,"-"),2,sd,"/")
 sv<-svd(Z*sqrt(w));loading<-sv$v[,1];if(sum(loading*fit$rotation[,1])<0)loading<- -loading
 verify(paste(side,"PCA weighted center"),same(mu,fit$center))
 verify(paste(side,"PCA weighted scale"),same(sd,fit$scale))
 verify(paste(side,"PCA independent weighted SVD"),same(loading,fit$rotation[,1],1e-8))
}
save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
jsonlite::write_json(list(all_passed=all(save$passed),check_count=nrow(save),source_rows=4555,need_n=2159,
 fp_facilities=1040,linked_fp_facilities=882,model_n=nrow(aa),models=10,
 fp_providers_only=TRUE,health_PB2A_only=TRUE,primary_cutoffs_zero=TRUE,all_models_same_women=TRUE,
 no_random_effects=TRUE,limitations="Implementation verification; not causal validation, full survey-design reconstruction or a formal power analysis."),
 file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Passed",nrow(save),"independent checks.\n")
