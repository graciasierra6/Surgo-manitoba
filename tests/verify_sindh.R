# Independent arithmetic, linkage, model and PCA checks; no source mutations.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
input<-opt("--input");raw_file<-opt("--raw");need_file<-opt("--need-file");data_dir<-opt("--data-dir")
library(haven);library(jsonlite);library(clubSandwich)
out<-file.path(data_dir,"checks");dir.create(out,recursive=TRUE,showWarnings=FALSE)
x<-readRDS(input);raw<-read_dta(raw_file);nf<-readRDS(need_file)
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women;p<-fromJSON(file.path(data_dir,"report_data.json"))
checks<-list();add<-function(name,ok,detail=""){stopifnot(isTRUE(ok));checks[[length(checks)+1L]]<<-data.frame(check=name,passed=TRUE,detail=as.character(detail))}
add("All original raw women and IDs retained",nrow(raw)==nrow(x)&&identical(as.numeric(raw$col_id),as.numeric(x$col_id)),nrow(x))
same<-vapply(names(raw),function(k)isTRUE(all.equal(as.vector(raw[[k]]),as.vector(x[[k]]),check.attributes=FALSE)),logical(1))
add("Every original SURGO field preserved",all(same),length(same))
num<-function(k)as.numeric(raw[[k]])
valid<-num("irb1")==1 & is.finite(num("Normalizedweights")) & num("Normalizedweights")>0
exclude<-num("sa0")%in%2 | num("sa1")%in%c(5,6,95,97) | num("pc0")%in%1 | num("sf2")%in%1 |
 num("pc18")%in%1 | num("pc19")%in%2 | num("sf0b")%in%1 | num("sf0c")%in%1 |
 num("sf1a")%in%3 | num("sf1b")%in%3 | num("sf1c")%in%3
need<-valid & !exclude
add("Authorized need independently reproduced",identical(need,d$need)&&sum(need)==3274)
add("Derived flag ID match has no disagreements",all(nf$fpn2_derived[match(x$col_id,nf$col_id)][valid]==as.integer(need[valid])))
add("Positive-weight consent frame",sum(valid)==9926&&identical(valid,d$positive))
add("No permanent-protection additions",!any(d$need & nf$need_n_orig[match(x$col_id,nf$col_id)]!=1,na.rm=TRUE))
add("Final normalized weights retained",identical(d$weight,num("Normalizedweights")))
add("Unrounded clusterCode retained",identical(d$psu,as.character(raw$clusterCode)))
add("PSUs uniquely nested within districts",all(vapply(split(d$geo[valid],d$psu[valid]),function(v)length(unique(v))==1,logical(1))))
key<-function(v)toupper(gsub("[^A-Za-z0-9]","",v))
add("District match exactly agrees with recorded woman district",identical(d$geo,key(as.character(raw$rb0c))))
add("No invented residence variable",!"urban"%in%names(d))
reconcile<-read.csv(file.path(data_dir,"supply_input_reconciliation.csv"))
add("All 180 facility-to-joined supply checks pass",nrow(reconcile)==180&&all(reconcile$passed))
add("No supply profile varies within district",nrow(unique(cbind(geo=d$geo,frame$supply)))==30)
add("Unobserved supply is NA rather than zero",all(is.na(frame$supply$training[d$geo=="TANDOALLAHYAR"]))&&all(is.na(frame$supply$counselor[d$geo=="TANDOALLAHYAR"])))
# Independently code the demand domains without calling the analysis helper.
item<-function(k,pos,valid){v<-num(k);ifelse(v%in%valid,as.integer(v%in%pos),NA_real_)}
combine<-function(mat)apply(mat,1,function(v)if(any(v==1,na.rm=TRUE))1 else if(anyNA(v))NA_real_ else 0)
ind<-list(agency=item("hs7c",1,1:2),stigma=combine(cbind(item("pb7b",1:2,1:4),item("pb7c",1:2,1:4))),
 health=combine(cbind(item("pb2a",1:2,1:4),item("pb2b",1:2,1:4),item("pb2c",1:2,1:4))),
 confidence=combine(cbind(item("mc5a",3:4,1:4),item("mc5b",3:4,1:4))),opposes=item("fp1",3:4,1:4))
mp<-sapply(paste0("mp4_",1:9),num)
ind$fertility<-apply(mp,1,function(v){if(!all(v%in%0:1))return(NA_real_);if(!any(v==1))return(NA_real_);if(v[9]==1&&any(v[1:8]==1))return(NA_real_);as.integer(any(v[1:6]==1))})
for(k in p$demand_variables)add(paste("Demand coding",k),isTRUE(all.equal(ind[[k]],d[[k]],check.attributes=FALSE)))
prompts<-as.matrix(raw[paste0("cu5",letters[1:4])])
current<-ifelse(num("cu1")%in%c(1,3)|rowSums(prompts==1,na.rm=TRUE)>0,1,
 ifelse(num("cu1")%in%2 & rowSums(prompts==2,na.rm=TRUE)==4,0,NA_real_))
modern<-rowSums(as.matrix(raw[paste0("cu4_",1:8)])==1,na.rm=TRUE)>0 | num("cu5c")%in%1 | num("cu5d")%in%1 |
 (num("cu4_11")%in%1 & num("cu4_lam")%in%1)
traditional<-rowSums(as.matrix(raw[paste0("cu4_",c(9,10,12:15))])==1,na.rm=TRUE)>0 | num("cu5a")%in%1 | num("cu5b")%in%1
known_modern<-ifelse(current%in%0,0,ifelse(current%in%1 & modern,1,ifelse(current%in%1 & traditional,0,NA_real_)))
add("Independent modern-use outcome coding",isTRUE(all.equal(known_modern,d$y_modern,check.attributes=FALSE)))
for(bi in seq_len(nrow(p$bars))){
 b<-p$bars[bi,]
 v<-d[[b$field]][need];w<-d$weight[need]
 add(paste("Independent weighted overview",b$field),abs(sum(v*w)/sum(w)-b$estimate)<1e-10 && sum(v)==b$n && b$denominator==3274)
}
# Correlations describe overlap in selected domains, not outcome significance.
wcorr<-function(a,b,w){ok<-is.finite(a)&is.finite(b)&is.finite(w)&w>0;a<-a[ok];b<-b[ok];w<-w[ok]/sum(w[ok]);
 a<-a-sum(a*w);b<-b-sum(b*w);sum(w*a*b)/sqrt(sum(w*a^2)*sum(w*b^2))}
corr<-list()
for(side in c("Demand","Supply")){
 fields<-if(side=="Demand")p$demand_variables else p$supply_variables
 mat<-if(side=="Demand")d[need,fields]else frame$profiles[fields]
 ww<-if(side=="Demand")d$weight[need]else rep(1,nrow(mat))
 for(j in seq_len(length(fields)))for(k in seq_len(length(fields))){
  corr[[length(corr)+1L]]<-data.frame(side=side,item1=fields[j],item2=fields[k],
   correlation=wcorr(mat[[j]],mat[[k]],ww),n_pair=sum(complete.cases(mat[c(j,k)])))
 }
}
write.csv(do.call(rbind,corr),file.path(out,"component_correlations.csv"),row.names=FALSE)
z<-sapply(p$demand_variables,function(k){v<-ind[[k]];ok<-need&is.finite(v);mu<-sum(v[ok]*d$weight[ok])/sum(d$weight[ok]);sd<-sqrt(sum((v[ok]-mu)^2*d$weight[ok])/sum(d$weight[ok]));(v-mu)/sd})
personal<-rowSums(z)/6
zS<-sapply(p$supply_variables,function(k){r<-p$component_trace[p$component_trace$side=="Supply"&p$component_trace$variable==k,];(100*frame$supply[[k]]-unique(r$indicator_mean))/unique(r$indicator_SD)})
supply<- -rowSums(zS)/6
pv<-readRDS(file.path(data_dir,"pca_appendix/pca_scores.rds"))
max_svd<-0
for(side in c("demand","supply")){
 fit<-pv[[paste0(side,"_fit")]];mat<-if(side=="demand")as.matrix(d[p$demand_variables])else as.matrix(frame$supply[p$supply_variables])
 use<-need & complete.cases(mat);w<-d$weight[use]/sum(d$weight[use]);
 zz<-sweep(sweep(mat[use,,drop=FALSE],2,fit$center,"-"),2,fit$scale,"/")
 sv<-svd(zz*sqrt(w));err<-max(abs(sv$d^2-fit$eigenvalues));max_svd<-max(max_svd,err)
 add(paste("Independent weighted SVD",side),err<1e-10,err)
 score<-drop(sweep(sweep(mat,2,fit$center,"-"),2,fit$scale,"/")%*%fit$rotation[,1]);if(side=="supply")score<- -score
 saved<-if(side=="demand")pv$scores$demand_score else pv$scores$supply_risk
 add(paste("PCA projection for every source woman",side),isTRUE(all.equal(score,saved,check.attributes=FALSE,tolerance=1e-12)))
}
previous_rows<-NULL
for(method in c("zscore","pca")){
 folder<-if(method=="zscore")file.path(data_dir,"models_zscore")else file.path(data_dir,"pca_appendix/models")
 fits<-readRDS(file.path(folder,"fitted_models.rds"))
 for(id in names(fits)){
  ob<-fits[[id]];dd<-ob$data;ix<-dd$row
  expectedD<-if(method=="zscore")personal[ix]else pv$scores$demand_score[ix]
  expectedS<-if(method=="zscore")supply[ix]else pv$scores$supply_risk[ix]
  add(paste(method,id,"personal demand score match"),max(abs(dd$demand_score-expectedD))<1e-12)
  add(paste(method,id,"assigned district supply score match"),max(abs(dd$supply_risk-expectedS))<1e-12)
  add(paste(method,id,"common sample and outcome"),nrow(dd)==3048&&sum(dd$y_modern)==1670&&all(dd$need)&&all(dd$y_modern%in%0:1))
  add(paste(method,id,"modern-use formula and expected adjustment"),identical(all.vars(ob$formula)[1],"y_modern")&&all(c("age10","age10_sq","education","fertility_goal")%in%all.vars(ob$formula)))
  add(paste(method,id,"same women across all specifications"),is.null(previous_rows)||identical(ix,previous_rows));previous_rows<-ix
  independent<-dd;independent$indw<-independent$weight/mean(independent$weight)
  ff<-glm(ob$formula,data=independent,weights=indw,family=quasibinomial(),control=glm.control(maxit=100))
  add(paste(method,id,"independent GLM coefficient reproduction"),max(abs(coef(ff)-coef(ob$fit)))<1e-10)
  V<-clubSandwich::vcovCR(ff,cluster=dd$geo,type="CR2",inverse_var=FALSE)
  add(paste(method,id,"district CR2 reproduction"),max(abs(V-ob$V))<1e-10)
 }
}
zm<-readRDS(file.path(data_dir,"models_zscore/fitted_models.rds"));pm<-readRDS(file.path(data_dir,"pca_appendix/models/fitted_models.rds"))
add("Demand-indicator-only models identical across score methods",identical(coef(zm$components_demand$fit),coef(pm$components_demand$fit)))
add("Exactly 29 modeled supply profiles",length(unique(zm$score_joint$data$supply_risk))==29)
save<-do.call(rbind,checks);write.csv(save,file.path(out,"audit_checks.csv"),row.names=FALSE)
write_json(list(check_count=nrow(save),all_passed=all(save$passed),source_rows=nrow(x),need_n=sum(need),model_n=3048,
 models=10,source_raw_columns_verified=length(same),facility_component_checks=180,weighted_SVD_max_error=max_svd,
 women_level_scores_verified=TRUE,all_models_same_women=TRUE,all_model_outcomes_modern=TRUE,
 facility_join_version="pipeline_crosswalk",no_random_effects=TRUE),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE,digits=12)
cat('Passed',nrow(save),'independent checks.\n')
