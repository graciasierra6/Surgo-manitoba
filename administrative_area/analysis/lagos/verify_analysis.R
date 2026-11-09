# Independent numerical checks of the LGA-level report and model outputs.
args<-commandArgs(trailingOnly=TRUE)
value<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
data_dir<-value("--data-dir");input<-value("--input");facility_input<-value("--facility-input")
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));x<-readRDS(input)
f<-readRDS(facility_input);f<-f[f$country=="lagos",]
key<-function(v)toupper(gsub("[^A-Za-z0-9]","",as.character(v)))
f$geo<-ifelse(!is.na(f$geo_join_pipeline),key(f$geo_join_pipeline),key(f$source_geo_name))
bool<-function(v)ifelse(tolower(v)%in%c("true","1","yes"),1,ifelse(tolower(v)%in%c("false","0","no"),0,NA_real_))
f$offering<-bool(f$one_larc_3_sa_offer);f$free<-as.numeric(f$p_free_prod);f$training<-as.numeric(f$p_train)
f$counselor<-as.numeric(f$counselor_available);f$larc_stock<-bool(f$three_larc_stockin);f$popular_stock<-bool(f$mp_in_stock)
one<-f$geo=="IBEJULEKKI" & f$rurb=="Urban";stopifnot(sum(one)==1,is.na(f$free[one]));f$free[one]<-0
D<-p$demand_variables;S<-p$supply_variables;frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
areas<-sort(unique(d$geo[d$need]));check<-list()
verify<-function(name,condition){check[[length(check)+1L]]<<-data.frame(check=name,passed=isTRUE(condition));if(!isTRUE(condition))stop(name)}
close<-function(a,b,tol=1e-8)isTRUE(all.equal(as.numeric(a),as.numeric(b),tolerance=tol,check.attributes=FALSE))
verify("original source rows retained",nrow(x)==4555 && nrow(d)==4555)
verify("original need indicator unchanged",identical(d$need,as.numeric(x$fpn2)%in%1) && sum(d$need)==2159)
verify("original weight unchanged",close(d$weight,as.numeric(x$weight)))
verify("all women have a recorded residence",all(d$residence%in%c("Rural","Urban")))
verify("checked geography retained",identical(d$geo,key(x$link_geo_join)))
verify("linked facility count",sum(f$geo%in%areas)==1447)
verify("complete facility count",sum(complete.cases(f[S]) & f$geo%in%areas)==614)
verify("complete demand count",sum(complete.cases(d[d$need,D]))==1891)
for(k in S)for(g in areas){v<-f[[k]][f$geo==g];v<-v[is.finite(v)];h<-p$heatmap[p$heatmap$geo==g & p$heatmap$side=="Supply" & p$heatmap$variable==k,]
 verify(paste("facility mean",g,k),close(100*mean(v),h$value));verify(paste("facility denominator",g,k),length(v)==h$n_valid)
 verify(paste("supply assigned to all LGA women",g,k),close(frame$supply[[k]][d$geo==g],rep(mean(v),sum(d$geo==g))))
}
for(k in D)for(g in areas){ii<-d$need & d$geo==g & is.finite(d[[k]]);h<-p$heatmap[p$heatmap$geo==g & p$heatmap$side=="Demand" & p$heatmap$variable==k,]
 verify(paste("weighted concern",g,k),close(100*weighted.mean(d[[k]][ii],d$weight[ii]),h$value))
 verify(paste("demand denominator",g,k),sum(ii)==h$n_valid)
}
for(side in c("Demand","Supply"))for(k in if(side=="Demand")D else S){t<-p$component_trace[p$component_trace$side==side & p$component_trace$variable==k,]
 verify(paste("component standardization",side,k),close(t$zscore,(t$original_pct-mean(t$original_pct))/sd(t$original_pct)))
}
z<-p$scores[p$scores$method=="zscore",];cut<-p$thresholds[p$thresholds$method=="zscore",]
verify("demand median threshold",close(cut$demand_cutoff,median(z$demand_score)))
verify("supply median threshold",close(cut$supply_cutoff,median(z$supply_score)))
verify("demand flag rule",all(z$demand_flag==(z$demand_score>cut$demand_cutoff+1e-10)))
verify("supply flag rule",all(z$supply_flag==(z$supply_score<cut$supply_cutoff-1e-10)))
for(folder in c("complete_facility_sensitivity","complete_demand_sensitivity")){
 q<-jsonlite::fromJSON(file.path(data_dir,folder,"report_data.json"));qz<-q$scores[q$scores$method=="zscore",]
 qc<-q$thresholds[q$thresholds$method=="zscore",]
 verify(paste(folder,"fixed cutoffs"),close(qc$demand_cutoff,cut$demand_cutoff)&&close(qc$supply_cutoff,cut$supply_cutoff))
 for(side in c("Demand","Supply"))for(k in if(side=="Demand")D else S){ref<-p$component_trace[p$component_trace$side==side & p$component_trace$variable==k,];t<-q$component_trace[q$component_trace$side==side & q$component_trace$variable==k,]
 verify(paste(folder,"fixed references",side,k),close(t$zscore,(t$original_pct-unique(ref$indicator_mean))/unique(ref$indicator_SD)))
 }
}
for(folder in c("models_zscore","complete_facility_sensitivity/models_zscore","pca_appendix/models")){
 models<-readRDS(file.path(data_dir,folder,"fitted_models.rds"));co<-read.csv(file.path(data_dir,folder,"model_coefficients.csv"));samples<-read.csv(file.path(data_dir,folder,"model_samples.csv"))
 verify(paste(folder,"same women and outcome"),all(samples$n==1865 & samples$events==943))
 for(id in names(models)){
  obj<-models[[id]];a<-obj$data;f0<-obj$fit
  ref<-suppressWarnings(glm(obj$formula,data=a,weights=a$weight/mean(a$weight),family=binomial(),control=glm.control(maxit=100)))
  verify(paste(folder,id,"independent weighted coefficients"),close(coef(ref),coef(f0),1e-7))
  V<-clubSandwich::vcovCR(ref,cluster=a$geo,type="CR2",inverse_var=FALSE)
  verify(paste(folder,id,"CR2 covariance"),close(V,obj$V,1e-7))
  verify(paste(folder,id,"modern-use outcome"),all(a$y_modern%in%0:1) && all(a$need))
  verify(paste(folder,id,"full rank and convergence"),f0$converged && f0$rank==length(coef(f0)))
  t<-co[co$model==id,];t<-t[match(names(coef(f0)),t$term),]
  verify(paste(folder,id,"odds ratios"),close(t$odds_ratio,exp(coef(f0))))
 }
}
pca<-readRDS(file.path(data_dir,"pca_appendix/pca_scores.rds"))
for(side in c("demand","supply")){
 fit<-pca[[paste0(side,"_fit")]];m<-if(side=="demand")as.matrix(d[D])else as.matrix(frame$supply[S]);ii<-d$need & complete.cases(m)
 w<-d$weight[ii]/sum(d$weight[ii]);mu<-colSums(m[ii,,drop=FALSE]*w);sd0<-sqrt(colSums(sweep(m[ii,,drop=FALSE],2,mu,"-")^2*w));Z<-sweep(sweep(m[ii,,drop=FALSE],2,mu,"-"),2,sd0,"/")
 sv<-svd(Z*sqrt(w));loading<-sv$v[,1];if(sum(loading*fit$rotation[,1])<0)loading<- -loading
 verify(paste(side,"PCA center"),close(mu,fit$center))
 verify(paste(side,"PCA scale"),close(sd0,fit$scale))
 verify(paste(side,"PCA independent SVD"),close(loading,fit$rotation[,1],1e-7))
}
out<-file.path(data_dir,"checks");dir.create(out,showWarnings=FALSE)
checks<-do.call(rbind,check);write.csv(checks,file.path(out,"audit_checks.csv"),row.names=FALSE)
jsonlite::write_json(list(all_passed=all(checks$passed),check_count=nrow(checks),
 limitations="Reproduction and numerical verification only; not a formal power analysis, causal validation or confirmation of full sampling design."),file.path(out,"verification.json"),auto_unbox=TRUE,pretty=TRUE)
cat("Passed",nrow(checks),"independent numerical checks.\n")
