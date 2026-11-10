# Report-specific supply definitions; the previous country report is unchanged.
base_cdi_domains <- cdi_domains
cdi_domains <- function() {
  a <- base_cdi_domains()
  a$S <- c("offering","free","training","daily_provision","larc_stock","stockout_burden")
  a$SL <- c("Short + long-acting offering","Free-product provision","Trained-provider share",
    "Daily provision of offered modern methods","At least one LARC in stock","Stockout burden")
  a
}
cdi_supply <- function(x) {
  map <- c(offering="share_offer_larc_and_3_sa",free="mean_p_free_prod",training="mean_p_train",
    daily_provision="mean_daily_modern_provision",larc_stock="share_stocked_1plus_larc",
    stockout_burden="mean_facility_any_stockout")
  stopifnot(all(unname(map)%in%names(x)))
  as.data.frame(lapply(map,function(k)as.numeric(x[[k]])))
}
cdi_favorable_supply <- function(x) {
  x<-as.data.frame(x)
  if("stockout_burden"%in%names(x))x$stockout_burden<-1-x$stockout_burden
  x
}
cdi_frequency_fields <- function() c("s5_2a","s5_2b2m","s5_2b3m","s5_2c","s5_2d",
  "s5_2e","s5_2f","s5_2g3a","s5_2g5a")
cdi_frequency <- function(raw) {
  fields<-cdi_frequency_fields();stopifnot(all(fields%in%names(raw)))
  q<-as.data.frame(lapply(raw[fields],as.numeric))
  collapse<-function(fields)apply(as.matrix(q[fields]),1,function(v){
    offered<-v[v%in%1:4]
    if(length(offered))min(offered)else if(all(v%in%5))5 else NA_real_
  })
  m<-cbind(pills=q$s5_2a,injectables=collapse(c("s5_2b2m","s5_2b3m")),
    male_condoms=q$s5_2c,female_condoms=q$s5_2d,ecp=q$s5_2e,iud=q$s5_2f,
    implants=collapse(c("s5_2g3a","s5_2g5a")))
  n_offered<-rowSums(m>=1&m<=4,na.rm=TRUE);n_daily<-rowSums(m==1,na.rm=TRUE)
  n_weekly<-rowSums(m>=1&m<=2,na.rm=TRUE)
  data.frame(daily_provision=ifelse(n_offered>0,n_daily/n_offered,NA_real_),
    at_least_weekly_provision=ifelse(n_offered>0,n_weekly/n_offered,NA_real_),
    frequency_daily_categories=n_daily,frequency_offered_categories=n_offered,
    frequency_valid_categories=rowSums(m>=1&m<=5,na.rm=TRUE),
    frequency_unknown_categories=7L-rowSums(m>=1&m<=5,na.rm=TRUE),
    frequency_unknown_source_items=rowSums(vapply(q,function(v)!v%in%1:5,logical(nrow(raw)))))
}
