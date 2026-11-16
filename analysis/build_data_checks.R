# Recalculate the cumulative checks from the input bundle and current report frames.
args <- commandArgs(trailingOnly=TRUE)
arg <- function(k) {i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
if("--help"%in%args) {
 cat("Rscript analysis/build_data_checks.R --cache-dir CACHE --facility-input FACILITIES --lagos-dir ANALYSIS --senegal-dir ANALYSIS --cotedivoire-dir ANALYSIS --sindh-dir ANALYSIS --output-json JSON\n")
 quit(status=0)
}
nv <- function(x) as.numeric(haven::zap_labels(x))
countries <- c("lagos","senegal","sindh","cotedivoire")
cache <- arg("--cache-dir")
fac <- readRDS(arg("--facility-input"))
frames <- setNames(lapply(countries,function(k)readRDS(file.path(arg(paste0("--",k,"-dir")),"analysis_frame.rds"))),countries)
num <- function(x) format(x,big.mark=",",scientific=FALSE,trim=TRUE)
pct <- function(x) sprintf("%.2f%%",x)
count_pct <- function(n,p) paste0(num(n),' <span class="pct">(',pct(p),')</span>')
values <- list()
put <- function(table,row,col,value) values[[paste(table,row,col,sep="_")]] <<- as.character(value)
raw <- list()
for(i in seq_along(countries)) {
 k<-countries[i];folder<-if(k=="cotedivoire")"cdi"else k
 v<-switch(k,senegal="s5_6",cotedivoire="s2_5_sp","q506")
 fields<-c(v,if(k=="cotedivoire")c("fid","s5_1","s5_6","s5_3a","s5_3c","s5_3f","s5_3l"))
 raw[[k]]<-haven::read_dta(file.path(cache,"inputs/uom",folder,paste0("uom_",folder,"_facility_pooled.dta")),
  col_select=tidyselect::all_of(fields))
 x<-nv(raw[[k]][[v]]);n<-length(x);counts<-c(sum(x%in%1),sum(x%in%c(0,2)),sum(!x%in%c(0,1,2)))
 put(1,i,3,num(n))
 for(j in 1:3)put(1,i,j+3,count_pct(counts[j],100*counts[j]/n))
 d<-frames[[k]]$women;use<-d$need
 stopifnot(sum(use)==c(lagos=2159,senegal=1905,sindh=3274,cotedivoire=4292)[[k]])
 for(j in 1:7) {
  fields<-c("agency","stigma","health","fertility","confidence","opposes")
  missing<-if(j==7)!complete.cases(d[fields])else !is.finite(d[[fields[j]]])
  put(3,j,i+1,count_pct(sum(use&missing),100*weighted.mean(missing[use],d$weight[use])))
 }
}
d<-frames$cotedivoire$women;supply<-frames$cotedivoire$supply
unknown<-!complete.cases(supply);positive<-d$positive&is.finite(d$weight)&d$weight>0
keys<-c("BONON","FACOBLY","GOHITAFLA","TAABO","DAOUKRO")
for(i in 1:7) {
 selected<-if(i<=5)d$geo%in%keys[i]else if(i==6)!d$geo%in%unique(fac$assigned_join_area[fac$country=="cotedivoire"])&unknown else unknown
 if(i==6)selected<-unknown&!d$geo%in%keys
 n<-sum(selected);wp<-100*sum(d$weight[selected&positive])/sum(d$weight[positive])
 put(4,i,2,num(n));put(4,i,3,pct(100*n/nrow(d)));put(4,i,4,if(i==6)"—"else pct(wp))
}
r<-raw$cotedivoire;f<-fac[fac$country=="cotedivoire",]
ix<-match(as.character(r$fid),as.character(f$fid));stopifnot(!anyNA(ix))
geo<-toupper(gsub("[^A-Za-z0-9]","",as.character(f$assigned_join_area[ix])))
selected<-geo%in%"DAOUKRO"&nv(r$s2_5_sp)%in%1
stopifnot(sum(selected)==19)
for(i in 1:3) {
 x<-nv(r[[c("s2_5_sp","s5_1","s5_6")[i]]][selected]);yes<-sum(x%in%1);no<-sum(x%in%c(0,2))
 text<-if(!no)paste(num(yes),"Yes")else if(!yes)paste(num(no),"No")else paste0(num(yes)," Yes; ",num(no)," No")
 put(5,i,2,paste0("<strong>",text,"</strong>"))
}
for(i in 1:4) {
 x<-nv(r[[c("s5_3a","s5_3c","s5_3f","s5_3l")[i]]][selected])
 # This questionnaire uses -9 for not applicable; this is distinct from blanks.
 counts<-c(sum(x%in%1),sum(x%in%2),sum(x%in%(-9)),sum(is.na(x)))
 stopifnot(sum(counts)==sum(selected))
 for(j in 1:4)put(6,i,j+1,num(counts[j]))
}
yes<-nv(r$s2_5_sp)%in%1;onsite<-nv(r$s5_1)
counts<-c(sum(yes&onsite%in%1),sum(yes&onsite%in%2),sum(yes))
for(i in 1:3){put(8,i,2,num(counts[i]));put(8,i,3,pct(100*counts[i]/sum(yes)))}
out<-arg("--output-json");dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(values,out,auto_unbox=TRUE,pretty=TRUE)
cat("Recalculated facility eligibility, six-domain missingness, supply nonmatches and Daoukro routing.\n")
