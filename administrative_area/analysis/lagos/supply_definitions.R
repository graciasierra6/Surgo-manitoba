# Lagos-specific definitions for the separate updated aggregate report.
lagos_domains <- function() list(
 D=c("agency","stigma","health","fertility","confidence","opposes"),
 DL=c("Healthcare permission barriers","Anticipated social stigma","Health / side-effect beliefs",
      "Future-fertility concerns","Low method knowledge","Opposes family planning"),
 S=c("offering","free","training","daily_provision","larc_stock","stockout_burden"),
 SL=c("Short + long-acting offering","Free-product provision","Trained-provider share",
      "Daily provision of offered modern methods","At least one LARC in stock","Stockout burden"))
lagos_favorable_supply <- function(x) {
 x<-as.data.frame(x);x$stockout_burden<-1-x$stockout_burden;x
}
lagos_frequency_fields <- function() paste0("q507a",c("a","b","d","e","f","g","h","i","j","o"))
lagos_frequency <- function(raw) {
 q<-as.data.frame(lapply(raw[lagos_frequency_fields()],as.numeric))
 collapse<-function(fields)apply(as.matrix(q[fields]),1,function(v){
   offered<-v[v%in%1:4]
   if(length(offered))min(offered)else if(all(v%in%5))5 else NA_real_
 })
 m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah","q507ai")),
   male_condoms=q$q507ae,female_condoms=q$q507af,ecp=q$q507ao,
   iud=collapse(c("q507aa","q507ab")),implants=q$q507aj)
 offered<-rowSums(m>=1&m<=4,na.rm=TRUE);daily<-rowSums(m==1,na.rm=TRUE)
 data.frame(daily_provision=ifelse(offered>0,daily/offered,NA_real_),
   at_least_weekly_provision=ifelse(offered>0,rowSums(m>=1&m<=2,na.rm=TRUE)/offered,NA_real_),
   frequency_daily_categories=daily,frequency_offered_categories=offered,
   frequency_valid_categories=rowSums(m>=1&m<=5,na.rm=TRUE),
   frequency_unknown_categories=7L-rowSums(m>=1&m<=5,na.rm=TRUE),
   frequency_unknown_source_items=rowSums(vapply(q,function(v)!v%in%1:5,logical(nrow(raw)))))
}
