# Append aggregate-only Côte d’Ivoire residence fallback checks; preserve prior sections.
args<-commandArgs(trailingOnly=TRUE)
arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop("Supply ",k);return(default)};if(i==length(args))stop("Supply ",k);args[i+1L]}
base_html<-arg("--base-html");data_dir<-arg("--data-dir");output<-arg("--output");audit_json<-arg("--audit-json","")
frame<-readRDS(file.path(data_dir,"analysis_frame.rds"));d<-frame$women
coverage<-read.csv(file.path(data_dir,"residence_supply_profiles_and_coverage.csv"),stringsAsFactors=FALSE)
need<-d$need;total<-sum(need)
stopifnot(nrow(d)==10859,total==4292,all(d$residence[need]%in%c("Rural","Urban")),
 all(is.finite(d$weight[need])&d$weight[need]>0),!anyNA(d$fallback),
 sum(need&!d$has_fp_supply)==49,sum(need&!complete.cases(frame$supply))==92)
for(i in seq_len(nrow(coverage))){
 selected<-d$geo%in%coverage$geo[i]&d$residence%in%coverage$residence[i]
 expected_fallback<-coverage$fp_facilities[i]==0&&coverage$assigned_fp_facilities[i]>0
 stopifnot(sum(selected)==coverage$women[i],sum(selected&need)==coverage$women_in_need[i],
  all(d$fallback[selected]==coverage$fallback[i]),coverage$fallback[i]==expected_fallback)
}
# Counts are unweighted; percentages use the specified need-group denominator.
percent<-function(selected,pop)if(sum(pop))100*sum(selected&pop)/sum(pop)else NA_real_
wpercent<-function(selected,pop)if(sum(pop))100*sum(d$weight[selected&pop])/sum(d$weight[pop])else NA_real_
stat<-function(selected,pop)list(n=sum(selected&pop),denominator=sum(pop),
 sample_pct=percent(selected,pop),weighted_pct=wpercent(selected,pop),
 sample_pct_all=percent(selected,need),weighted_pct_all=wpercent(selected,need))
name<-function(g)tools::toTitleCase(tolower(unique(d$district[d$geo%in%g])))
esc<-function(x){x<-gsub("&","&amp;",as.character(x),fixed=TRUE);x<-gsub("<","&lt;",x,fixed=TRUE);gsub(">","&gt;",x,fixed=TRUE)}
num<-function(v)format(v,big.mark=",",scientific=FALSE,trim=TRUE)
pct<-function(v)if(is.na(v))"—"else sprintf("%.2f%%",v)
tab<-function(id,headers,rows){
 paste0('<div class="table-wrap"><table class="supply-coverage" id="',id,'"><thead><tr><th>',
 paste(esc(headers),collapse="</th><th>"),"</th></tr></thead><tbody>",
 paste(vapply(rows,function(x)paste0("<tr><td>",paste(esc(x),collapse="</td><td>"),"</td></tr>"),character(1)),collapse="\n"),
 "</tbody></table></div>")
}
section<-paste0('\n<!-- BEGIN CDI RESIDENCE CHECKS -->\n<section id="cdi-residence-fallbacks">',
 '<h2>Côte d’Ivoire: women in need and residence-specific supply fallback</h2>',
 '<p>Population: <strong>4,292 women in need of FP</strong>, using original SURGO <code>FPN2 = 1</code>, consent and positive <code>WEIGHT</code>. Facility summaries include general FP providers (<code>s2_5_sp = 1</code>), with valid answers separately for each measure. Each woman receives her own département–Rural/Urban profile. Only a cell with zero eligible FP providers can trigger a flagged whole-département fallback when that broader profile exists.</p>',
 '<div class="decision"><strong>No fallback was required</strong><p>All observed residence groups in linked départements have eligible FP facilities. No women in need received a département-wide default. The unmatched cases below lack a département profile too, so no fallback was available.</p></div>')
summary_rows<-lapply(c("Rural","Urban"),function(res){
 pop<-need&d$residence%in%res;v<-stat(pop&d$fallback,pop)
 c(res,num(sum(pop)),num(v$n),pct(v$weighted_pct))
})
summary_total<-stat(need&d$fallback,need)
summary_rows<-c(summary_rows,list(c("Total",num(total),num(summary_total$n),pct(summary_total$weighted_pct))))
section<-paste0(section,'<h3 style="font-size:17px;color:#185664">Rural/Urban fallback summary</h3>',
 tab("cdi-fallback-need-summary",c("Residence","Women in need, n","Received département-wide fallback, n","Weighted % receiving fallback"),summary_rows),
 '<p class="note">Counts are unweighted. Each residence percentage uses the corresponding Rural or Urban need-group denominator; the total uses all 4,292 women in need.</p>')
results<-list()
for(res in c("Rural","Urban")){
 pop<-need&d$residence%in%res;selected<-pop&d$fallback
 geos<-sort(unique(d$geo[selected]));s<-stat(selected,pop)
 rr<-lapply(geos,function(g){v<-stat(selected&d$geo%in%g,pop);c(name(g),num(v$n),pct(v$sample_pct),pct(v$weighted_pct),pct(v$sample_pct_all),pct(v$weighted_pct_all))})
 if(!length(rr))rr<-list(c("None — no fallback required","0","0.00%","0.00%","0.00%","0.00%"))
 rr<-c(rr,list(c("Total",num(s$n),pct(s$sample_pct),pct(s$weighted_pct),pct(s$sample_pct_all),pct(s$weighted_pct_all))))
 section<-paste0(section,'<h3 style="font-size:17px;color:#185664">',res,' women in need receiving département-wide averages</h3>',
 '<p class="note">Residence denominator: ',num(sum(pop)),' ',tolower(res),' women in need; overall denominator: ',num(total),' women in need.</p>',
 tab(paste0("cdi-",tolower(res),"-fallback-need"),c("Département","Women receiving fallback, n",paste0("Sample % of ",res," women in need"),paste0("Weighted % of ",res," women in need"),"Sample % of all women in need","Weighted % of all women in need"),rr))
 results[[res]]<-s
}
unmatched<-need&!d$has_fp_supply;geos<-sort(unique(d$geo[unmatched]))
unmatched_rows<-lapply(geos,function(g){ii<-unmatched&d$geo%in%g;s<-stat(ii,need)
 data.frame(departement=name(g),rural_n=sum(ii&d$residence%in%"Rural"),urban_n=sum(ii&d$residence%in%"Urban"),n=s$n,sample_pct=s$sample_pct,weighted_pct=s$weighted_pct)})
u<-do.call(rbind,unmatched_rows);stopifnot(sum(u$n)==49,sum(u$rural_n)==34,sum(u$urban_n)==15)
ut<-stat(unmatched,need)
rows<-lapply(seq_len(nrow(u)),function(i)c(u$departement[i],num(u$rural_n[i]),num(u$urban_n[i]),num(u$n[i]),pct(u$sample_pct[i]),pct(u$weighted_pct[i])))
rows<-c(rows,list(c("Total",num(sum(u$rural_n)),num(sum(u$urban_n)),num(ut$n),pct(ut$sample_pct),pct(ut$weighted_pct))))
section<-paste0(section,'<h3 style="font-size:17px;color:#185664">Separate issue: unmatched women, not fallback recipients</h3>',
 '<p>These départements have no linked facility profile at either resolution. Supply remains unknown; zero or another département’s averages are not substituted.</p>',
 tab("cdi-unmatched-need-residence",c("Département","Rural women in need, n","Urban women in need, n","Total women in need, n","Sample % of all women in need","Weighted % of all women in need"),rows),
 '<p class="note">An additional 43 women in need in Daoukro (22 Rural, 21 Urban) receive their exact residence profile, but free-product and daily-provision values are undefined. Existing FP facilities with an undefined component do not trigger an empty-cell fallback. Thus 49 unmatched plus 43 partially defined assignments give 92 women without all six supply measures. All remain in the joined dataset and need-group overview.</p>',
 '<p class="note"><strong>Denominators.</strong> Need-group counts are 1,715 Rural and 2,577 Urban. Sample percentages divide respondent counts; weighted percentages use original SURGO WEIGHT. These are full need-group denominators, not fitted-model complete cases. The zero fallback percentages are identical under both conventions. This addition does not change supply assignments, scores, classifications or fitted models.</p>',
 '</section>\n<!-- END CDI RESIDENCE CHECKS -->\n')
base<-paste(readLines(base_html,warn=FALSE,encoding="UTF-8"),collapse="\n")
base<-gsub('(?s)\n<!-- BEGIN CDI RESIDENCE CHECKS -->.*?<!-- END CDI RESIDENCE CHECKS -->\n?',"\n",base,perl=TRUE)
stopifnot(length(gregexpr("<footer",base,fixed=TRUE)[[1]])==1,grepl("<footer",base,fixed=TRUE))
base<-sub("\\s*<footer","<footer",base,perl=TRUE)
updated<-sub("<footer",paste0(section,"\n<footer"),base,fixed=TRUE)
dir.create(dirname(output),recursive=TRUE,showWarnings=FALSE);writeLines(updated,output,useBytes=TRUE)
if(nzchar(audit_json)){
 dir.create(dirname(audit_json),recursive=TRUE,showWarnings=FALSE)
 jsonlite::write_json(list(need_n=total,residence=results,unmatched=u,unmatched_total=ut,
  exact_profile_but_incomplete_need=43,all_incomplete_supply_need=92,
  policy="Flagged whole-département fallback only for zero exact-cell FP providers when a whole-département profile exists; none needed here."),
  audit_json,auto_unbox=TRUE,pretty=TRUE,dataframe="rows",na="null",digits=12)
}
cat("Added Côte d’Ivoire need-group Rural/Urban fallback tables (zero cases) and separate unmatched-area coverage.\n")
