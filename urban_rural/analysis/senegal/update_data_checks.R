# Append reproducible, aggregate-only Senegal fallback tables to an existing report.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop("Supply ",k);return(default)};if(i==length(args))stop("Supply ",k);args[i+1L]}
base_html<-arg("--base-html");data_dir<-arg("--data-dir");output<-arg("--output")
audit_json<-arg("--audit-json","")
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
coverage<-read.csv(file.path(data_dir,"residence_supply_profiles_and_coverage.csv"),stringsAsFactors=FALSE)
stopifnot(nrow(d)==7169,sum(d$need)==1905,all(d$residence%in%c("Rural","Urban")),
 sum(d$fallback)==1815,sum(d$fallback&d$need)==522,
 !any(d$fallback&d$residence=="Urban"))
for(i in seq_len(nrow(coverage))){
 ii<-paste(d$geo,d$residence,sep="|")==coverage$context[i]
 stopifnot(sum(ii)==coverage$women[i],sum(ii&d$need)==coverage$women_in_need[i],
  all(d$fallback[ii]==coverage$fallback[i]),coverage$fallback[i]==(coverage$fp_facilities[i]==0))
}
positive<-d$positive&is.finite(d$weight)&d$weight>0
stopifnot(sum(positive)==7158,all(positive[d$need]))
wm_share<-function(numerator,denominator){
 ii<-denominator&positive
 if(!any(ii))return(NA_real_)
 100*sum(d$weight[numerator&ii])/sum(d$weight[ii])
}
stats<-function(pop,residence,geo=NULL){
 residence_group<-pop&d$residence==residence
 selected<-residence_group&d$fallback
 if(!is.null(geo))selected<-selected&d$geo==geo
 list(n=sum(selected),n_residence=sum(residence_group),n_all=sum(pop),
  sample_pct_residence=100*sum(selected)/sum(residence_group),
  sample_pct_all=100*sum(selected)/sum(pop),
  weighted_pct_residence=wm_share(selected,residence_group),
  weighted_pct_all=wm_share(selected,pop))
}
esc<-function(x){x<-gsub("&","&amp;",as.character(x),fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
name<-function(g){
 a<-tools::toTitleCase(tolower(unique(d$district[d$geo==g])))
 special<-c(KEDOUGOU="Kédougou",LINGUERE="Linguère",MBACKE="Mbacké",SEDHIOU="Sédhiou",THIES="Thiès")
 if(g%in%names(special))unname(special[g])else a
}
num<-function(v)format(v,big.mark=",",scientific=FALSE,trim=TRUE)
pct<-function(v)sprintf("%.2f%%",v)
table_html<-function(id,headers,rows,total=NULL){
 row_html<-function(x,cls="")paste0('<tr',if(nzchar(cls))paste0(' class="',cls,'"')else'',"><td>",paste(esc(x),collapse="</td><td>"),"</td></tr>")
 paste0('<div class="table-wrap"><table class="supply-coverage" id="',id,'"><thead><tr><th>',
  paste(esc(headers),collapse="</th><th>"),"</th></tr></thead><tbody>",
  paste(vapply(rows,row_html,character(1)),collapse="\n"),
  if(!is.null(total))row_html(total,"total-row")else'',"</tbody></table></div>")
}
rural_geos<-sort(unique(d$geo[d$fallback&d$residence=="Rural"]))
stopifnot(length(rural_geos)==16)
populations<-list(all=rep(TRUE,nrow(d)),need=d$need)
results<-lapply(populations,function(pop)list(rural_total=stats(pop,"Rural"),
 urban_total=stats(pop,"Urban"),rural_rows=setNames(lapply(rural_geos,function(g)stats(pop,"Rural",g)),rural_geos)))
rows<-function(x)c(num(x$n),pct(x$sample_pct_residence),pct(x$sample_pct_all),pct(x$weighted_pct_residence),pct(x$weighted_pct_all))
section<-paste0('\n<!-- BEGIN SENEGAL RESIDENCE CHECKS -->\n<section id="senegal-residence-fallbacks">',
 '<h2>Senegal: département–Rural/Urban supply assignment and fallback</h2>',
 '<p>Supply means use only facilities reporting <code>s5_6 = 1</code> (FP services), with valid answers separately for each measure. A woman receives her own département–Rural/Urban FP-provider profile. If that exact group has zero eligible FP facilities, she receives her own département-wide FP-provider averages with an explicit fallback flag. Her recorded residence is unchanged.</p>',
 '<div class="decision"><strong>Observed coverage</strong><p>Across 33 départements, the 49 observed residence groups have 33 exact assignments and 16 rural département-wide fallbacks. The fallback affects <strong>1,815 source women</strong>, including <strong>522 women in need</strong>. No observed Urban group requires fallback.</p></div>',
 '<p class="note">Facility Rural/Urban is a health-district lookup proxy, not a site-level geographic measurement. Nine facility records have unknown proxy residence. A zero rural match does not establish absence of rural services. The flagged fallback represents broader département evidence, not measured rural conditions.</p>')
for(k in names(populations)){
 pop<-populations[[k]];r<-results[[k]];all_label<-if(k=="all")"all women"else"all women in need"
 heading<-if(k=="all")"All women: rural groups receiving département-wide averages"else"Women in need of FP: rural groups receiving département-wide averages"
 section<-paste0(section,'<h3 style="font-size:17px;color:#185664">',heading,"</h3>",
  '<p class="note">Population: ',num(sum(pop)),if(k=="all")' original women'else' women with original SURGO FPN2 = 1, consent and positive WEIGHT',
  '; ',num(r$rural_total$n_residence),' Rural and ',num(r$urban_total$n_residence),' Urban.</p>',
  table_html(paste0("senegal-rural-fallback-",k),
   c("Département","Women receiving fallback, n","Sample % of Rural women",paste0("Sample % of ",all_label),"Weighted % of Rural women",paste0("Weighted % of ",all_label)),
   lapply(rural_geos,function(g)c(name(g),rows(r$rural_rows[[g]]))),c("Total",rows(r$rural_total))))
 stopifnot(sum(vapply(r$rural_rows,function(a)a$n,numeric(1)))==r$rural_total$n,
  abs(sum(vapply(r$rural_rows,function(a)a$weighted_pct_all,numeric(1)))-r$rural_total$weighted_pct_all)<1e-10)
}
section<-paste0(section,'<h3 style="font-size:17px;color:#185664">Urban groups receiving département-wide averages</h3>',
 '<p>None. All 24 observed Urban département groups have at least one eligible FP provider, so their women receive the matching Urban profile. No Urban assignment is created for départements without Urban women in SURGO.</p>',
 table_html("senegal-urban-fallback-summary",c("Population","Urban women, n","Women receiving fallback, n","Sample % of Urban women","Sample % of population","Weighted % of Urban women","Weighted % of population"),
  lapply(names(populations),function(k){r<-results[[k]]$urban_total;c(if(k=="all")"All women"else"Women in need of FP",num(r$n_residence),rows(r))})),
 '<p class="note"><strong>How to read the percentages.</strong> Sample percentages divide unweighted respondent counts. Weighted percentages use original SURGO <code>WEIGHT</code> and the corresponding residence/population denominator. For all-women weighted estimates, use 7,158 consenting women with positive weights; 11 source records remain in raw counts but are outside weighted estimation, including one fallback record. All 1,905 women in need have valid positive weights. No facility weights are applied. Percentages use valid denominators, not fitted-model complete cases.</p>',
 '<p class="note">The source-group need denominator is 837 Rural women and 1,068 Urban women. These coverage tables concern the full need group, not only the 1,558 women with complete model variables. The LGA/département planning classifications and fitted models are unchanged by this reporting addition.</p>',
 '</section>\n<!-- END SENEGAL RESIDENCE CHECKS -->\n')
base<-paste(readLines(base_html,warn=FALSE,encoding="UTF-8"),collapse="\n")
base<-gsub('(?s)\n<!-- BEGIN SENEGAL RESIDENCE CHECKS -->.*?<!-- END SENEGAL RESIDENCE CHECKS -->\n?',"\n",base,perl=TRUE)
stopifnot(length(gregexpr("<footer",base,fixed=TRUE)[[1]])==1,grepl("<footer",base,fixed=TRUE))
updated<-sub("<footer",paste0(section,"\n<footer"),base,fixed=TRUE)
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE)
writeLines(updated,output,useBytes=TRUE)
if(nzchar(audit_json)){
 dir.create(dirname(audit_json),recursive=TRUE,showWarnings=FALSE)
 jsonlite::write_json(list(populations=results,source_rows=nrow(d),positive_weight_rows=sum(positive),
  need_n=sum(d$need),rural_fallback_groups=16,urban_fallback_groups=0,urban_observed_groups=24,
  policy="Department-wide fallback only for zero exact-cell FP providers; no input or analysis changes."),
  audit_json,auto_unbox=TRUE,pretty=TRUE,na="null",digits=12)
}
cat("Updated Data checks: two rural fallback tables, all/need Urban checks, sample and final-weight percentages.\n")
