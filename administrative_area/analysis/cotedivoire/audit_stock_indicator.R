# Diagnose the supplied indicator without changing any stock flag or analysis input.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
source_file<-opt("--facility-input");raw_file<-opt("--raw-facility");data_dir<-opt("--data-dir")
f<-readRDS(source_file);f<-f[f$country=="cotedivoire",]
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"))
key<-function(x)toupper(gsub("[^A-Za-z0-9]","",iconv(as.character(x),to="ASCII//TRANSLIT")))
f<-f[key(f$assigned_join_area)%in%p$profiles$geo,]
raw<-haven::read_dta(raw_file);raw<-raw[match(f$fid,as.character(raw$fid)),]
stopifnot(nrow(f)==2712,!anyNA(raw$fid))
bool<-function(v)ifelse(tolower(v)%in%c("true","1"),1,ifelse(tolower(v)%in%c("false","0"),0,NA_real_))
flag<-bool(f$mp_in_stock);method<-as.character(f$mpop_prdct)
missing<-is.na(flag);reason<-rep(NA_character_,nrow(f))
reason[missing&method=="no popular product"]<-"No popular product identified"
reason[missing&method=="condom_m"]<-"Male-condom stock question excluded by upstream selection"
reason[missing&method%in%c("steriliz_f","steriliz_m","lam","fdm")]<-"Method has no comparable commodity-stock flag"
columns<-list(condom_f="s6_16a",ec="s6_16b",iud="s6_16h",
 inj=c("s6_16c","s6_16d","s6_16d2m"),implant=c("s6_16e3","s6_16e5"),ocp=c("s6_16f","s6_16g"))
for(k in names(columns)){
 v<-as.data.frame(lapply(raw[columns[[k]]],as.numeric))
 # Replicate the existing upstream first-nonmissing choice for these groups.
 if(k%in%c("implant","ocp"))v<-data.frame(value=ifelse(is.na(v[[1]]),v[[2]],v[[1]]))
 present<-as.data.frame(lapply(v,function(z)ifelse(z%in%1:2,1,ifelse(z%in%3,0,NA_real_))))
 for(i in which(missing&method==k)){
  value<-unlist(present[i,]);stopifnot(!any(value==1,na.rm=TRUE),anyNA(value))
  reason[i]<-if(all(is.na(value)))"Selected stock responses inapplicable or never ordered"else"Known not-in-stock combined with inapplicable brand responses"
 }
}
stopifnot(all(!is.na(reason[missing])))
distribution<-data.frame(status=c("In stock","Not in stock","Undefined"),
 n=c(sum(flag==1,na.rm=TRUE),sum(flag==0,na.rm=TRUE),sum(missing)))
distribution$percent<-100*distribution$n/nrow(f)
reasons<-as.data.frame(table(reason[missing]),stringsAsFactors=FALSE);names(reasons)<-c("reason","n")
reasons$percent_all_facilities<-100*reasons$n/nrow(f)
counsel<-as.numeric(raw$s7_2)
counsel_distribution<-data.frame(response=c("Yes","No","Not applicable","Other or blank"),
 n=c(sum(counsel%in%1),sum(counsel%in%2),sum(counsel%in%-9),sum(!counsel%in%c(1,2,-9))))
fp<-as.numeric(raw$s2_5_sp)
services<-data.frame(response=c("Yes","No","Other or blank"),n=c(sum(missing&fp%in%1),sum(missing&fp%in%2),sum(missing&!fp%in%1:2)))
stopifnot(identical(distribution$n,c(1050L,169L,1493L)),sum(reasons$n)==1493)
write.csv(distribution,file.path(data_dir,"popular_stock_distribution.csv"),row.names=FALSE)
write.csv(reasons,file.path(data_dir,"popular_stock_missing_reasons.csv"),row.names=FALSE)
write.csv(counsel_distribution,file.path(data_dir,"counseling_response_distribution.csv"),row.names=FALSE)
write.csv(services,file.path(data_dir,"popular_stock_missing_fp_services.csv"),row.names=FALSE)
method_values<-sort(unique(method))
by_method<-do.call(rbind,lapply(method_values,function(k){ii<-method==k
 data.frame(method=k,in_stock=sum(ii&flag==1,na.rm=TRUE),not_in_stock=sum(ii&flag==0,na.rm=TRUE),
 undefined=sum(ii&is.na(flag)),total=sum(ii))}))
stopifnot(sum(by_method$total)==2712,all(rowSums(by_method[c("in_stock","not_in_stock","undefined")])==by_method$total))
write.csv(by_method,file.path(data_dir,"popular_stock_by_method.csv"),row.names=FALSE)
jsonlite::write_json(list(facilities=nrow(f),distribution=distribution,reasons=reasons,
 counseling=counsel_distribution,fp_services_among_undefined=services,
 coding="Most popular is ranked from Q803 product volumes and joined to Q616 stock groups. The upstream CDI selection excludes unlettered s6_16 (male condoms). any(present) retains NA when no TRUE is present and at least one brand response is NA. Inapplicable/never-ordered codes remain unknown. No underlying flag is repaired here.",
 implication="86.1% stocked is conditional on 1219 known flags. The indicator does not estimate availability across all 2712 facilities. Missing flags include structural applicability and coding limitations, not only item nonresponse or absence of FP services."),
 file.path(data_dir,"stock_indicator_audit.json"),dataframe="rows",auto_unbox=TRUE,pretty=TRUE,digits=12)
cat("Stock and counseling diagnostics saved; original indicators unchanged.\n")
