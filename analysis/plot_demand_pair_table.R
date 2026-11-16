# Presentation table from saved aggregate profiles; no model refitting.
plot_demand_pair_table <- function(input,country,area_a,area_b,output_dir,html_output='') {
  for(p in c('svglite','jsonlite'))if(!requireNamespace(p,quietly=TRUE))stop('Install ',p)
  x<-readRDS(input)
  pick<-function(g)x$profiles[x$profiles$country==country&x$profiles$side=='Demand'&x$profiles$geo==g,]
  a<-pick(area_a);b<-pick(area_b);stopifnot(nrow(a)==1,nrow(b)==1)
  v<-x$components[x$components$country==country&x$components$side=='Demand',]
  fields<-c('agency','stigma','health','fertility','confidence','opposes')
  aa<-v[v$geo==area_a,];aa<-aa[match(fields,aa$variable),]
  bb<-v[v$geo==area_b,];bb<-bb[match(fields,bb$variable),]
  stopifnot(nrow(aa)==6,nrow(bb)==6,all(is.finite(aa$risk_z)),all(is.finite(bb$risk_z)),
    max(abs(aa$reference_raw_mean-bb$reference_raw_mean))<1e-9,
    abs(mean(aa$risk_z)-a$score)<1e-9,abs(mean(bb$risk_z)-b$score)<1e-9)
  labels<-c('Healthcare permission barriers','Anticipated social stigma','Health / side-effect beliefs',
    'Future-fertility concerns','Low method knowledge','Opposition to family planning')
  difference<-abs(aa$raw_pct-bb$raw_pct)
  higher<-ifelse(aa$raw_pct>bb$raw_pct,a$area,ifelse(bb$raw_pct>aa$raw_pct,b$area,'Equal'))
  shade<-function(z)if(z<=1e-10)'#F5F7F9'else grDevices::colorRampPalette(c('#FFF0D9','#C46024'))(301)[1+round(100*min(z,3))]
  text<-function(label,x,y,size=12,bold=FALSE,col='#244652',just='left')grid::grid.text(label,x=x,y=y,just=just,
    gp=grid::gpar(fontfamily='sans',fontsize=size,fontface=if(bold)'bold'else'plain',col=col))
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  stem<-paste(country,tolower(area_a),tolower(area_b),'table',sep='_');svg<-file.path(output_dir,paste0(stem,'.svg'))
  svglite::svglite(svg,width=15,height=8.5,bg='white');grid::grid.newpage()
  text('Similar demand scores, different reported concerns',.025,.955,24,TRUE,col='#173E50')
  for(i in 1:2){q<-list(a,b)[[i]];left<-c(.025,.515)[i]
    grid::grid.roundrect(x=left+.23,y=.815,width=.46,height=.105,r=grid::unit(.012,'npc'),gp=grid::gpar(fill='#EEF4F6',col='#D8E3E8'))
    text(sprintf('%s  |  demand score %+.3f',q$area,q$score),left+.016,.839,17,TRUE)
    text(sprintf('%d of 6 components above average · %s women in need',q$unfavorable_components,format(q$n_need,big.mark=',')),left+.016,.795,11,col='#55707D')
  }
  text('Bold = above reference average',.025,.723,11,TRUE)
  text('Shading: farther above average',.315,.723,11)
  for(j in 1:40)grid::grid.rect(x=.570+j*.0021,y=.724,width=.0022,height=.021,gp=grid::gpar(fill=shade(j*3/40),col=NA))
  text('Blue bars: difference between areas',.705,.723,11,col='#246D95')
  grid::grid.rect(x=.5,y=.664,width=.95,height=.058,gp=grid::gpar(fill='#173E50',col=NA))
  text('Demand component',.038,.664,12,TRUE,col='white')
  text('Reference\naverage',.397,.664,11,TRUE,col='white',just='centre')
  text(a$area,.528,.664,12,TRUE,col='white',just='centre');text(b$area,.655,.664,12,TRUE,col='white',just='centre')
  text('Difference · percentage points',.735,.664,11,TRUE,col='white')
  y<-.594-(0:5)*.082
  for(i in seq_along(fields)) {
    grid::grid.rect(x=.5,y=y[i],width=.95,height=.079,gp=grid::gpar(fill=if(i%%2)'#FFFFFF'else'#FAFCFD',col=NA))
    text(labels[i],.038,y[i],12);text(sprintf('%.1f%%',aa$reference_raw_mean[i]),.397,y[i],13,col='#6A818C',just='centre')
    for(j in 1:2){z<-list(aa,bb)[[j]]$risk_z[i];pct<-list(aa,bb)[[j]]$raw_pct[i];cx<-c(.528,.655)[j]
      grid::grid.roundrect(x=cx,y=y[i],width=.112,height=.062,r=grid::unit(.007,'npc'),gp=grid::gpar(fill=shade(z),col=NA))
      text(sprintf('%.1f%%',pct),cx,y[i],15,bold=z>1e-10,col=if(z>=2.4)'white'else'#244652',just='centre')
    }
    start<-.735;width<-.221
    grid::grid.roundrect(x=start+width/2,y=y[i]-.019,width=width,height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#E8F0F5',col=NA))
    if(difference[i]>0)grid::grid.roundrect(x=start+width*difference[i]/200,y=y[i]-.019,width=width*difference[i]/100,
      height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#3F86AC',col=NA))
    text(sprintf('%.1f pp · higher in %s',difference[i],higher[i]),start,y[i]+.009,11,col='#276D92')
  }
  grDevices::dev.off()
  vals<-data.frame(component=fields,label=labels,reference_pct=aa$reference_raw_mean,
    area_a=a$area,pct_a=aa$raw_pct,z_a=aa$risk_z,above_a=aa$risk_z>1e-10,n_a=aa$n_valid,
    area_b=b$area,pct_b=bb$raw_pct,z_b=bb$risk_z,above_b=bb$risk_z>1e-10,n_b=bb$n_valid,
    difference_pp=difference,higher_area=higher)
  write.csv(vals,file.path(output_dir,paste0(stem,'_values.csv')),row.names=FALSE)
  if(nzchar(html_output)){
    esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
    rows<-vapply(seq_len(nrow(vals)),function(i)paste0('<tr><td>',esc(vals$label[i]),'</td><td>',sprintf('%.1f%%',vals$reference_pct[i]),
      '</td><td>',if(vals$above_a[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_a[i]),if(vals$above_a[i])'</strong>'else'',
      '</td><td>',if(vals$above_b[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_b[i]),if(vals$above_b[i])'</strong>'else'',
      '</td><td>',sprintf('%.1f pp',vals$difference_pp[i]),' · higher in ',esc(vals$higher_area[i]),'</td></tr>'),character(1))
    html<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Demand comparison table example</title><style>body{margin:0;background:#F2F6F8;color:#244652;font:16px/1.6 Arial,sans-serif}main{max-width:1500px;margin:24px auto;padding:0 20px}figure{margin:0;background:white;border:1px solid #DBE5EA;border-radius:8px}img{display:block;width:100%;height:auto}.note{background:white;padding:18px 24px;margin:16px 0;border-left:4px solid #3F86AC}.table-wrap{overflow-x:auto;background:white}table{width:100%;border-collapse:collapse;font-size:14px}th,td{padding:12px;text-align:left;border-bottom:1px solid #DFE8ED}th{background:#EEF4F6}summary{cursor:pointer;font-weight:bold;padding:12px}@media print{body{background:white}}</style></head><body><main><figure><img alt="Raw demand percentages with above-reference bolding, relative-elevation shading and percentage-point differences for ',esc(a$area),' and ',esc(b$area),'" src="data:image/svg+xml;base64,',jsonlite::base64_enc(readBin(svg,'raw',n=file.info(svg)$size)),
      '"></figure><details><summary>Definitions and accessible values</summary><p>Bold means above the component reference mean (z &gt; 0). Orange becomes darker as the positive component z-score increases; shade saturates at z = 3. Values at or below average are neutral. Blue bars show absolute percentage-point differences on a fixed 0–100 scale, with text naming the higher area. No new threshold changes scores or classifications.</p><p>Reference averages give each area equal weight within the geography. Individual area percentages are survey-weighted among women in need, using valid responses per component. These are descriptive point estimates, not tests of differences. The retained label “Low method knowledge” refers to MC5 confidence/self-efficacy, not an objective knowledge test.</p><div class="table-wrap"><table><thead><tr><th>Component</th><th>Reference average</th><th>',esc(a$area),'</th><th>',esc(b$area),'</th><th>Difference</th></tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div></details></main></body></html>')
    dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(html,html_output,useBytes=TRUE)
  }
  invisible(svg)
}

plot_all_demand_pair_tables <- function(input,output_dir,html_output='') {
  cases<-data.frame(country=c('cotedivoire','senegal','lagos','sindh'),
    label=c('Côte d’Ivoire','Senegal','Lagos','Sindh'),
    a=c('MEAGUI','MBACKE','AJEROMIIFELODUN','DISTRICTKEMARI'),
    b=c('YAMOUSSOUKRO','BIGNONA','IFAKOIJAYE','KHAIRPUR'))
  sections<-character(nrow(cases));style<-''
  for(i in seq_len(nrow(cases))){
    cc<-cases[i,];child<-file.path(output_dir,paste0(cc$country,'_demand_comparison.html'))
    plot_demand_pair_table(input,cc$country,cc$a,cc$b,output_dir,child)
    h<-paste(readLines(child,warn=FALSE),collapse='\n')
    fragment<-regmatches(h,regexpr('(?s)<main>.*?</main>',h,perl=TRUE))
    fragment<-sub('^<main>','',sub('</main>$','',fragment))
    sections[i]<-paste0('<section id="',cc$country,'" aria-label="',cc$label,'">',fragment,'</section>')
    if(i==1)style<-regmatches(h,regexpr('(?s)<style>.*?</style>',h,perl=TRUE))
  }
  if(nzchar(html_output)){
    nav<-paste0('<a href="#',cases$country,'">',cases$label,'</a>',collapse='')
    h<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Demand pair comparisons</title>',style,
      '<style>header{max-width:1500px;margin:24px auto;padding:0 20px}h1{font-size:28px;color:#173E50}nav{display:flex;gap:12px;flex-wrap:wrap}nav a{color:#246D95;text-decoration:none;padding:6px 14px;background:white;border:1px solid #DBE5EA;border-radius:18px}section{margin:0 0 32px}details{background:white;padding:10px 20px;margin-top:10px;border-radius:6px}section:target{scroll-margin-top:18px}details p{max-width:1100px}@media print{header,details{display:none}section{break-inside:avoid;break-after:page}}</style></head><body><header><h1>Demand pair comparisons</h1><nav>',nav,'</nav></header><main>',paste(sections,collapse=''),'</main></body></html>')
    dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(h,html_output,useBytes=TRUE)
  }
  invisible(cases)
}

plot_regional_comparisons <- function(input,output_dir,html_output) {
  cases<-data.frame(
    id=c('cotedivoire','senegal','lagos','sindh-kemari-khairpur','sindh-hyderabad-sanghar'),
    country=c('cotedivoire','senegal','lagos','sindh','sindh'),
    label=c('Côte d’Ivoire','Senegal','Lagos','Sindh','Sindh'),
    a=c('MEAGUI','MBACKE','AJEROMIIFELODUN','DISTRICTKEMARI','HYDERABAD'),
    b=c('YAMOUSSOUKRO','BIGNONA','IFAKOIJAYE','KHAIRPUR','SANGHAR'),
    comparison_type=c('Both — different types of challenge and different breadth and severity',
      'Different breadth and severity','Different types of challenge',
      'Different types of challenge','Both — different types of challenge and different breadth and severity'),
    takeaway=c(
      'Méagui exceeds the reference average on five components, compared with three in Yamoussoukro. Health/side-effect concerns are higher in Méagui (70.6% versus 57.5%), while low method knowledge is much higher in Yamoussoukro (76.5% versus 17.5%). The similar scores combine a broader set of above-average concerns in Méagui with a particularly large confidence-related concern in Yamoussoukro.',
      'Mbacké has four components above average, including one marginal elevation, compared with two in Bignona. Bignona’s standout concern is low method knowledge (86.5% versus 33.3% in Mbacké). Mbacké has higher health/side-effect concerns (68.1% versus 31.7%) and opposition to family planning (34.7% versus 18.2%).',
      'Both LGAs have five components above average, but the mix differs. Ajeromi-Ifelodun has more social stigma (23.4% versus 7.8%) and opposition to family planning (36.0% versus 27.2%). Ifako-Ijaye has more healthcare permission barriers (24.2% versus 11.9%) and health/side-effect concerns (57.3% versus 49.0%).',
      'Kemari and Khairpur have almost identical scores, but low method knowledge is 85.2% in Kemari and 10.0% in Khairpur. Khairpur has more future-fertility concerns (89.5% versus 63.1%) and social stigma (50.3% versus 23.8%). Three components are above average in Kemari, compared with four in Khairpur.',
      'Hyderabad’s profile features high health/side-effect and future-fertility concerns (92.0% and 89.0%). Sanghar has more social stigma (65.5% versus 30.4%), low method knowledge (40.9% versus 11.0%) and opposition to family planning (25.2% versus 11.1%). Sanghar has five components above average, compared with three in Hyderabad, although its permission barriers and health concerns are only slightly above their respective averages.'),
    stringsAsFactors=FALSE)
  x<-readRDS(input)
  sections<-nav<-character(nrow(cases));style<-''
  esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
  for(i in seq_len(nrow(cases))){
    cc<-cases[i,]
    q<-x$profiles[x$profiles$country==cc$country&x$profiles$side=='Demand'&x$profiles$geo%in%c(cc$a,cc$b),]
    stopifnot(nrow(q)==2,all(is.finite(q$score)),all(q$score>0))
    a<-q[q$geo==cc$a,];b<-q[q$geo==cc$b,]
    child<-file.path(output_dir,paste0(cc$id,'_demand_comparison.html'))
    plot_demand_pair_table(input,cc$country,cc$a,cc$b,output_dir,child)
    h<-paste(readLines(child,warn=FALSE),collapse='\n')
    fragment<-regmatches(h,regexpr('(?s)<main>.*?</main>',h,perl=TRUE))
    fragment<-sub('^<main>','',sub('</main>$','',fragment))
    finding<-paste0('<div class="finding"><p class="comparison-label"><strong>What this comparison shows:</strong> ',esc(cc$comparison_type),'</p><p><strong>Key finding.</strong> ',esc(cc$takeaway),'</p></div>')
    fragment<-sub('</figure>',paste0('</figure>',finding),fragment,fixed=TRUE)
    title<-paste0(cc$label,' · ',a$area,' and ',b$area)
    sections[i]<-paste0('<section id="',cc$id,'" aria-labelledby="',cc$id,'-title"><h2 id="',cc$id,'-title">',i,'. ',esc(title),'</h2>',fragment,'</section>')
    nav[i]<-paste0('<a href="#',cc$id,'">',i,'. ',esc(a$area),' / ',esc(b$area),'</a>')
    if(i==1)style<-regmatches(h,regexpr('(?s)<style>.*?</style>',h,perl=TRUE))
  }
  h<-paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Composite score comparisons</title>',style,
    '<style>header{max-width:1500px;margin:32px auto 20px;padding:0 20px}h1{margin:0 0 16px;font-size:32px;line-height:1.2;color:#173E50}header p{max-width:1120px;margin:10px 0}.intro{font-size:18px}.context{font-size:14px;color:#55707D}nav{display:flex;gap:8px;flex-wrap:wrap;margin-top:22px}nav a{color:#246D95;text-decoration:none;padding:6px 12px;background:white;border:1px solid #DBE5EA;border-radius:18px;font-size:14px}nav a:hover,nav a:focus{background:#E5EFF4}h2{font-size:22px;line-height:1.35;color:#173E50;margin:0 0 14px}section{margin:0 0 48px;scroll-margin-top:20px}.finding{background:#EAF2F6;border-left:4px solid #3F86AC;border-radius:0 6px 6px 0;padding:16px 22px;margin:14px 0}.finding p{margin:0;max-width:1260px}details{background:white;padding:8px 18px;margin-top:10px;border:1px solid #DBE5EA;border-radius:6px}details p{max-width:1100px}summary{font-size:14px}main{margin-top:28px}@media(max-width:640px){header{margin-top:22px}h1{font-size:27px}h2{font-size:20px}.intro{font-size:17px}.finding{padding:14px 16px}section{margin-bottom:36px}}@media print{body{background:white}header{margin:0;padding:0}header nav,details{display:none}main{padding:0}section{break-before:page;break-inside:avoid}.finding{background:#F2F6F8}h1{font-size:26px}}</style></head><body><header><h1>Composite score comparisons</h1>',
    '<p class="intro">These five comparisons show areas flagged for <strong>“Demand” concerns</strong> that have similar overall demand risk scores but face different challenges. <strong>Similar overall scores can reflect different barriers; the component profile shows what the score alone conceals.</strong></p>',
    '<p class="context">All percentages describe women in need of family planning. “Demand” means an above-average overall demand score within the geography; some areas are also flagged for supply concerns. Reference averages give each surveyed area equal weight. An above-average component is not necessarily far above average.</p><nav aria-label="Comparisons">',paste(nav,collapse=''),'</nav></header><main>',paste(sections,collapse=''),'</main></body></html>')
  h<-sub('</style></head>','</style><style>.finding p.comparison-label{margin:0 0 8px;font-size:14px;font-weight:600;color:#246D95}</style></head>',h,fixed=TRUE)
  dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(h,html_output,useBytes=TRUE)
  invisible(cases)
}

if(sys.nframe()==0L){
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
  if('--help'%in%args){cat('Rscript analysis/plot_demand_pair_table.R --input AGGREGATE_PROFILES [--regional-comparisons | --all-pairs | --country COUNTRY --area-a KEY --area-b KEY] [--output-dir output/demand_pair_tables] [--html-output REPORT]\n');quit(status=0)}
  if('--regional-comparisons'%in%args)plot_regional_comparisons(arg('--input'),arg('--output-dir','output/demand_pair_tables'),arg('--html-output'))else
  if('--all-pairs'%in%args)plot_all_demand_pair_tables(arg('--input'),arg('--output-dir','output/demand_pair_tables'),arg('--html-output',''))else
    plot_demand_pair_table(arg('--input'),arg('--country','senegal'),arg('--area-a','MBACKE'),arg('--area-b','BIGNONA'),arg('--output-dir','output/demand_pair_tables'),arg('--html-output',''))
}
