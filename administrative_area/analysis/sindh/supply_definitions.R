# Country-specific selected inputs for the updated district-only Sindh report.
sindh_updated_domains <- function() list(
 D=c("agency","stigma","health","fertility","confidence","opposes"),
 DL=c("Healthcare permission barriers","Anticipated social stigma","Health / side-effect beliefs",
      "Future-fertility concerns","Low method knowledge","Opposes family planning"),
 S=c("offering","free","training","daily_provision","larc_stock","stockout_burden"),
 SL=c("Short + long-acting offering","Free-product provision","Trained-provider share",
      "Daily provision of offered modern methods","At least one LARC in stock","Stockout burden"))
sindh_updated_supply <- function(x) {
 map<-c(offering="share_offer_larc_and_3_sa",free="mean_p_free_prod",training="mean_p_train",
        daily_provision="mean_daily_modern_provision",larc_stock="share_stocked_1plus_larc",
        stockout_burden="mean_facility_any_stockout")
 stopifnot(all(unname(map)%in%names(x)))
 as.data.frame(lapply(map,function(k)as.numeric(x[[k]])))
}
sindh_favorable_supply <- function(x) {x<-as.data.frame(x);x$stockout_burden<-1-x$stockout_burden;x}
sindh_frequency_fields <- function() c("q507aa","q507aa1","q507aa2","q507ad","q507ae","q507af",
                                       "q507ag","q507ah","q507aj","q507ao")
sindh_frequency <- function(raw) {
 q<-as.data.frame(lapply(raw[sindh_frequency_fields()],as.numeric))
 collapse<-function(fields)apply(as.matrix(q[fields]),1,function(v){
  offered<-v[v%in%1:4]
  if(length(offered))min(offered)else if(all(v%in%5))5 else NA_real_
 })
 # Seven classes. IUD contexts and injectable variants each count only once.
 # q507ab/ai are entirely unpopulated in this source; neither is coded as No.
 m<-cbind(pills=q$q507ad,injectables=collapse(c("q507ag","q507ah")),
   male_condoms=q$q507ae,female_condoms=q$q507af,ecp=q$q507ao,
   iud=collapse(c("q507aa","q507aa1","q507aa2")),implants=q$q507aj)
 offered<-rowSums(m>=1&m<=4,na.rm=TRUE);daily<-rowSums(m==1,na.rm=TRUE)
 data.frame(daily_provision=ifelse(offered>0,daily/offered,NA_real_),
   at_least_weekly_provision=ifelse(offered>0,rowSums(m>=1&m<=2,na.rm=TRUE)/offered,NA_real_),
   frequency_daily_categories=daily,frequency_offered_categories=offered,
   frequency_valid_categories=rowSums(m>=1&m<=5,na.rm=TRUE),
   frequency_unknown_categories=7L-rowSums(m>=1&m<=5,na.rm=TRUE),
   frequency_unknown_source_items=rowSums(vapply(q,function(v)!v%in%1:5,logical(nrow(raw)))))
}
