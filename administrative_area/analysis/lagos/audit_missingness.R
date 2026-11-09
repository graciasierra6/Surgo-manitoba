# Diagnose the saved Lagos indicators without changing analytical definitions.
args<-commandArgs(trailingOnly=TRUE)
value<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
raw_file<-value("--raw");facility_file<-value("--facility-input");uom_file<-value("--uom-rdata");out<-value("--output-dir")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
f<-readRDS(facility_file);f<-f[f$country=="lagos",]
e<-new.env();load(uom_file,envir=e);v<-e$fclty_list$lagos
v<-v[match(f$fid,v$fid),];stopifnot(!anyNA(v$fid))
r<-haven::read_dta(raw_file,col_select=c("key","q506","fac_status","consent"))
r<-r[match(f$fid,as.character(r$key)),];stopifnot(!anyNA(r$key))
key<-function(v)toupper(gsub("[^A-Za-z0-9]","",as.character(v)))
f$geo<-ifelse(!is.na(f$geo_join_pipeline),key(f$geo_join_pipeline),key(f$source_geo_name))
linked<-!is.na(f$geo_join_pipeline)
exception<-f$geo=="IBEJULEKKI" & f$rurb=="Urban";stopifnot(sum(exception)==1,is.na(f$p_free_prod[exception]))
free_missing<-is.na(f$p_free_prod);free_missing[exception]<-FALSE
stock_missing<-is.na(f$mp_in_stock)
# Original b06 excludes s5_8a_c and s5_8a_k. All original included numerators
# and denominators are independently recovered before diagnosing missingness.
free_cols<-setdiff(grep("^s5_8a_[a-z]$",names(v),value=TRUE),c("s5_8a_c","s5_8a_k"))
Q<-sapply(v[free_cols],as.character)
ny<-rowSums(Q=="Yes",na.rm=TRUE)
nn<-rowSums(matrix(Q%in%c("No","Never"),nrow=nrow(Q)))
stopifnot(all(ny==as.numeric(f$t_free_prod)),all(nn==as.numeric(f$t_not_free_prod)),
 all((ny+nn)[free_missing]==0),all(is.na(Q[free_missing,,drop=FALSE])))
M0<-sapply(v[grep("^s8_3[a-t]$",names(v))],function(z)suppressWarnings(as.numeric(as.character(z))))
M<-M0;M[M%in%c(998,999,9999,9998,36950,10000,10018,998998)]<-NA_real_
nv<-rowSums(is.finite(M));np<-rowSums(M>0,na.rm=TRUE)
method<-as.character(f$mpop_prdct)
no_pop<-method=="no popular product"
reason<-rep(NA_character_,nrow(f))
reason[stock_missing & no_pop & nv==0]<-"No popular product: no valid volume response"
reason[stock_missing & no_pop & nv>0 & np==0]<-"No popular product: available valid volumes are zero"
reason[stock_missing & no_pop & np>0]<-"No popular product: positive volume only in an excluded category"
reason[stock_missing & method=="iud"]<-"IUD: observed out-of-stock response combined with missing brand status"
reason[stock_missing & method%in%c("std_cycle","steriliz_f")]<-"Method without comparable commodity-stock item"
iud<-linked & stock_missing & method=="iud"
stopifnot(all(as.character(v$s6_16j[iud])=="Out of stock"),all(is.na(v$s6_16k[iud])))
positive_excluded<-which(linked & stock_missing & no_pop & np>0)
stopifnot(length(positive_excluded)==1,M[positive_excluded,"s8_3c"]==2,
 sum(M[positive_excluded,]>0,na.rm=TRUE)==1)
stopifnot(all(!is.na(reason[linked & stock_missing])))
fp<-as.character(v$s5_6);fp[is.na(fp)]<-"Unanswered"
status<-as.character(haven::as_factor(r$fac_status));consent<-as.character(haven::as_factor(r$consent))
by_lga<-do.call(rbind,lapply(sort(unique(f$geo)),function(g){ii<-f$geo==g;data.frame(
 LGA=f$source_geo_name[which(ii)[1]],in_analysis=g%in%f$geo[linked],facilities=sum(ii),
 free_missing=sum(free_missing & ii),free_missing_pct=100*mean(free_missing[ii]),
 popular_stock_missing=sum(stock_missing & ii),popular_stock_missing_pct=100*mean(stock_missing[ii]))}))
by_fp<-do.call(rbind,lapply(c("Free-product proportion","Popular-product stock"),function(label){ii<-linked & if(label=="Free-product proportion")free_missing else stock_missing
 data.frame(indicator=label,as.data.frame(table(FP_service=fp[ii])),row.names=NULL)}))
reason_table<-as.data.frame(table(reason=reason[linked & stock_missing]),stringsAsFactors=FALSE)
reason_table$percent_of_undefined<-100*reason_table$Freq/sum(linked & stock_missing)
coverage_status<-as.data.frame(table(status=status[linked & is.na(v$s5_6)],consent=consent[linked & is.na(v$s5_6)],useNA="ifany"))
write.csv(by_lga,file.path(out,"Lagos_missingness_by_LGA.csv"),row.names=FALSE)
write.csv(by_fp,file.path(out,"missingness_by_FP_service_status.csv"),row.names=FALSE)
write.csv(reason_table,file.path(out,"popular_stock_missingness_reasons.csv"),row.names=FALSE)
write.csv(coverage_status,file.path(out,"unanswered_FP_service_status.csv"),row.names=FALSE)
audit<-list(linked_facilities=sum(linked),free_undefined=sum(linked&free_missing),
 popular_stock_undefined=sum(linked&stock_missing),free_counts_reproduced=TRUE,
 zero_denominator_free_undefined=sum(linked&free_missing&ny+nn==0),
 no_popular_product=sum(linked&stock_missing&no_pop),
 no_valid_volume=sum(linked&stock_missing&no_pop&nv==0),
 no_raw_numeric_volume=sum(linked&stock_missing&no_pop&rowSums(is.finite(M0))==0),
 only_special_volume=sum(linked&stock_missing&no_pop&nv==0&rowSums(is.finite(M0))>0),
 male_condom_omission="Not present in Lagos: s6_16a is included by the original selector.",
 interpretation="Valid-response averages are conditional on usable replies. Unidentified popular product does not establish no FP service or no commodity stock. Original indicators and classifications remain unchanged.")
jsonlite::write_json(audit,file.path(out,"missingness_audit.json"),auto_unbox=TRUE,pretty=TRUE)
print(audit);print(reason_table);print(by_fp);print(by_lga[by_lga$in_analysis,])
