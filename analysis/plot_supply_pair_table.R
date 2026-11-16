# Supply-readiness comparisons from saved aggregate profiles; no model refitting.
plot_supply_pair_table <- function(input,country,area_a,area_b,output_dir,html_output='') {
  for(p in c('svglite','jsonlite'))if(!requireNamespace(p,quietly=TRUE))stop('Install ',p)
  x<-readRDS(input)
  pick<-function(g)x$profiles[x$profiles$country==country&x$profiles$side=='Supply'&x$profiles$geo==g,]
  a<-pick(area_a);b<-pick(area_b);stopifnot(nrow(a)==1,nrow(b)==1,a$score>0,b$score>0)
  if(area_a=='MBAHIAKRO')a$area<-'M’Bahiakro'
  if(area_b=='MBAHIAKRO')b$area<-'M’Bahiakro'
  v<-x$components[x$components$country==country&x$components$side=='Supply',]
  fields<-c('offering','free','training','daily_provision','larc_stock','stockout_burden')
  aa<-v[v$geo==area_a,];aa<-aa[match(fields,aa$variable),]
  bb<-v[v$geo==area_b,];bb<-bb[match(fields,bb$variable),]
  stopifnot(nrow(aa)==6,nrow(bb)==6,all(is.finite(aa$risk_z)),all(is.finite(bb$risk_z)),
    abs(mean(aa$risk_z)-a$score)<1e-9,abs(mean(bb$risk_z)-b$score)<1e-9)
  # The diagnostic input stores negative readiness as a challenge score.
  # Restore the original readiness direction and transform only the stockout row.
  for(n in c('aa','bb')){
    q<-get(n);q$readiness_z<--q$risk_z
    q$display_pct<-ifelse(q$variable=='stockout_burden',100-q$raw_pct,q$raw_pct)
    q$display_reference<-ifelse(q$variable=='stockout_burden',100-q$reference_raw_mean,q$reference_raw_mean)
    assign(n,q)
  }
  stopifnot(max(abs(aa$display_reference-bb$display_reference))<1e-9,
    min(aa$n_valid,bb$n_valid)>=20,
    all(aa$display_pct>0&aa$display_pct<100),all(bb$display_pct>0&bb$display_pct<100))
  labels<-c('Short + long-acting offering','Free-product provision','Trained-provider share',
    'Daily provision of offered modern methods','At least one LARC in stock','Stock continuity')
  difference<-abs(aa$display_pct-bb$display_pct)
  lower<-ifelse(aa$display_pct<bb$display_pct,a$area,ifelse(bb$display_pct<aa$display_pct,b$area,'Equal'))
  shade<-function(z)if(z>=-1e-10)'#F5F7F9'else grDevices::colorRampPalette(c('#FFF0D9','#C46024'))(301)[1+round(100*min(-z,3))]
  text<-function(label,x,y,size=12,bold=FALSE,col='#244652',just='left')grid::grid.text(label,x=x,y=y,just=just,
    gp=grid::gpar(fontfamily='sans',fontsize=size,fontface=if(bold)'bold'else'plain',col=col))
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  stem<-paste(country,tolower(area_a),tolower(area_b),'supply_table',sep='_');svg<-file.path(output_dir,paste0(stem,'.svg'))
  svglite::svglite(svg,width=15,height=8.5,bg='white');grid::grid.newpage()
  text('Similar readiness scores, different supply weaknesses',.025,.955,24,TRUE,col='#173E50')
  for(i in 1:2){q<-list(a,b)[[i]];left<-c(.025,.515)[i]
    grid::grid.roundrect(x=left+.23,y=.815,width=.46,height=.105,r=grid::unit(.012,'npc'),gp=grid::gpar(fill='#EEF4F6',col='#D8E3E8'))
    text(sprintf('%s  |  readiness score %+.3f',q$area,-q$score),left+.016,.839,17,TRUE)
    text(sprintf('%d of 6 components below average · %s FP facilities',q$unfavorable_components,format(q$fp_facilities,big.mark=',')),left+.016,.795,11,col='#55707D')
  }
  text('Bold = below reference average',.025,.723,11,TRUE)
  text('Shading: farther below average',.315,.723,11)
  for(j in 1:40)grid::grid.rect(x=.570+j*.0021,y=.724,width=.0022,height=.021,gp=grid::gpar(fill=shade(-j*3/40),col=NA))
  text('Blue bars: difference between areas',.705,.723,11,col='#246D95')
  grid::grid.rect(x=.5,y=.664,width=.95,height=.058,gp=grid::gpar(fill='#173E50',col=NA))
  text('Supply component · higher is better',.038,.664,12,TRUE,col='white')
  text('Reference\naverage',.397,.664,11,TRUE,col='white',just='centre')
  text(a$area,.528,.664,12,TRUE,col='white',just='centre');text(b$area,.655,.664,12,TRUE,col='white',just='centre')
  text('Difference · percentage points',.735,.664,11,TRUE,col='white')
  y<-.594-(0:5)*.082
  for(i in seq_along(fields)) {
    grid::grid.rect(x=.5,y=y[i],width=.95,height=.079,gp=grid::gpar(fill=if(i%%2)'#FFFFFF'else'#FAFCFD',col=NA))
    text(labels[i],.038,y[i],12);text(sprintf('%.1f%%',aa$display_reference[i]),.397,y[i],13,col='#6A818C',just='centre')
    for(j in 1:2){q<-list(aa,bb)[[j]];z<-q$readiness_z[i];cx<-c(.528,.655)[j]
      grid::grid.roundrect(x=cx,y=y[i],width=.112,height=.062,r=grid::unit(.007,'npc'),gp=grid::gpar(fill=shade(z),col=NA))
      text(sprintf('%.1f%%',q$display_pct[i]),cx,y[i],15,bold=z< -1e-10,col=if(z<=-2.4)'white'else'#244652',just='centre')
    }
    start<-.735;width<-.221
    grid::grid.roundrect(x=start+width/2,y=y[i]-.019,width=width,height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#E8F0F5',col=NA))
    if(difference[i]>0)grid::grid.roundrect(x=start+width*difference[i]/200,y=y[i]-.019,width=width*difference[i]/100,
      height=.010,r=grid::unit(.004,'npc'),gp=grid::gpar(fill='#3F86AC',col=NA))
    text(sprintf('%.1f pp · lower in %s',difference[i],lower[i]),start,y[i]+.009,11,col='#276D92')
  }
  grDevices::dev.off()
  vals<-data.frame(component=fields,label=labels,reference_pct=aa$display_reference,
    area_a=a$area,pct_a=aa$display_pct,z_a=aa$readiness_z,below_a=aa$readiness_z< -1e-10,n_a=aa$n_valid,
    area_b=b$area,pct_b=bb$display_pct,z_b=bb$readiness_z,below_b=bb$readiness_z< -1e-10,n_b=bb$n_valid,
    difference_pp=difference,lower_area=lower)
  write.csv(vals,file.path(output_dir,paste0(stem,'_values.csv')),row.names=FALSE)
  if(nzchar(html_output)){
    esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
    rows<-vapply(seq_len(nrow(vals)),function(i)paste0('<tr><td>',esc(vals$label[i]),'</td><td>',sprintf('%.1f%%',vals$reference_pct[i]),
      '</td><td>',if(vals$below_a[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_a[i]),if(vals$below_a[i])'</strong>'else'',
      '</td><td>',if(vals$below_b[i])'<strong>'else'',sprintf('%.1f%%',vals$pct_b[i]),if(vals$below_b[i])'</strong>'else'',
      '</td><td>',sprintf('%.1f pp',vals$difference_pp[i]),' · lower in ',esc(vals$lower_area[i]),'</td><td>',vals$n_a[i],' / ',vals$n_b[i],'</td></tr>'),character(1))
    h<-paste0('<figure><img alt="Supply-readiness percentages, below-reference bolding and percentage-point differences for ',esc(a$area),' and ',esc(b$area),'" src="data:image/svg+xml;base64,',jsonlite::base64_enc(readBin(svg,'raw',n=file.info(svg)$size)),
      '"></figure><details><summary>Definitions and accessible values</summary><p>Higher values mean better readiness in all six rows. Stock continuity is 100 minus the original stockout burden; it retains the underlying UoM product-group denominator and is not the percentage of facilities without any stockout. Bold means below the component reference mean (z &lt; 0). Orange becomes darker as the shortfall increases; shading saturates at z = −3. Blue bars show absolute percentage-point differences on a fixed 0–100 scale, with text naming the lower area.</p><p>Supply summaries include confirmed FP-providing facilities with valid responses separately for each measure. Each facility contributes equally; valid zero/FALSE values are retained, and missing/undefined values are excluded. Method offering and LARC stock are facility percentages; free provision, training, daily provision and stock continuity summarize within-facility proportions. Reference averages give each assessed area equal weight within its geography. Scores are the mean of the six favorable component z-scores; negative means below-average overall readiness. All figures use whole-area profiles, as in the area heatmaps, rather than residence-specific profiles assigned in woman-level models. These descriptive comparisons do not establish causal effects or statistically significant differences.</p><div class="table-wrap"><table><thead><tr><th>Component</th><th>Reference average</th><th>',esc(a$area),'</th><th>',esc(b$area),'</th><th>Difference</th><th>Valid facilities: first / second area</th></tr></thead><tbody>',paste(rows,collapse=''),'</tbody></table></div></details>')
    writeLines(h,html_output,useBytes=TRUE)
  }
  invisible(svg)
}

build_supply_regional_comparisons <- function(input,output_dir,html_output,project_root) {
  source(file.path(project_root,'analysis/plot_demand_pair_table.R'),local=TRUE)
  cases<-data.frame(
    id=c('supply-lagos-types','supply-senegal-concentrated','supply-cdi-concentrated','supply-cdi-types','supply-lagos-breadth','supply-cdi-service-contrast','supply-cdi-offsetting-strengths','supply-cdi-broad-versus-deep'),
    country=c('lagos','senegal','cotedivoire','cotedivoire','lagos','cotedivoire','cotedivoire','cotedivoire'),
    label=c('Lagos','Senegal','Côte d’Ivoire','Côte d’Ivoire','Lagos','Côte d’Ivoire','Côte d’Ivoire','Côte d’Ivoire'),
    a=c('IBEJULEKKI','MATAM','ABIDJAN','BONGOUANOU','AJEROMIIFELODUN','BONGOUANOU','BIANKOUMA','DIVO'),
    b=c('IKORODU','GOUDIRY','GAGNOA','MBAHIAKRO','OJO','TANDA','MEAGUI','BONGOUANOU'),
    comparison_type=c('Different types of challenge','Different breadth and severity',
      'Both — different types of challenge and different breadth and severity',
      'Different types of challenge','Different breadth and severity',
      'Different types of challenge','Different breadth and severity','Different breadth and severity'),
    takeaway=c(
      'Ibeju-Lekki and Ikorodu have nearly identical readiness scores but different weaknesses. Ibeju-Lekki has lower short- and long-acting offering and LARC stock (both 51.4%, versus 74.1% in Ikorodu). Ikorodu has lower free provision (43.9% versus 70.6%) and trained-provider share (53.9% versus 72.4%). Similar totals conceal different service profiles.',
      'Matam’s main shortfall is daily provision: 29.8%, compared with the 76.2% reference average. Its free provision and training are below average by less than one percentage point each. Goudiry has four components below average, including method offering (67.6%) and daily provision (48.6%). This pair shows one pronounced shortfall in Matam alongside broader weaknesses in Goudiry.',
      'Abidjan has five components below average; Gagnoa has three. Gagnoa’s largest shortfall is free provision (62.8%, versus 78.3% in Abidjan), while its other two below-average components are closer to their references. Abidjan has a much lower share of facilities with a LARC in stock (49.5% versus 69.7%). Abidjan’s daily provision is only marginally below average, so five below-average components does not mean five large shortfalls.',
      'Bongouanou and M’Bahiakro have almost identical scores, but their service profiles differ sharply. Bongouanou has lower LARC stock (37.0% versus 85.0%) and stock continuity (16.4% versus 42.1%). M’Bahiakro has lower free provision (62.8% versus 90.9%) and trained-provider share (16.9% versus 45.9%). The same overall readiness level combines different weaknesses.',
      'Ajeromi-Ifelodun is below average across all six components; Ojo is below average on four, with method offering and LARC stock above average. Ojo’s daily provision is lower (17.2% versus 25.1%), while Ajeromi-Ifelodun has lower free provision (34.3% versus 42.9%) and LARC stock (68.9% versus 76.8%). Similar negative scores can reflect weaknesses across all components or a mix of strengths and shortfalls.',
      'Bongouanou’s largest shortfalls are LARC stock (37.0%) and stock continuity (16.4%). Tanda has much lower free provision (62.7% versus 90.9%) and daily provision (36.0% versus 73.0%). Their similar readiness scores conceal different types of service weakness, with three components below average in Bongouanou and five in Tanda.',
      'Biankouma is below average on four components, with modest-to-moderate shortfalls. Méagui has two pronounced shortfalls: method offering is 51.3% against an 80.3% reference average, and daily provision is 35.6% against 66.2%. Its other four components are above average and offset these weaknesses, bringing the overall score close to zero. The relatively favorable total therefore conceals substantial weaknesses in two services.',
      'Divo has five components below average, with modest-to-moderate shortfalls. Bongouanou has two pronounced weaknesses: LARC stock is 37.0% against a 72.4% reference average, and stock continuity is 16.4% against 35.1%. Method offering is also below average, while its other three components are above average and partly offset these weaknesses. Similar overall readiness scores can therefore represent broader moderate weaknesses or fewer, deeper weaknesses balanced by strengths elsewhere.'),stringsAsFactors=FALSE)
  # Explicit display names retain accents and punctuation without altering keys.
  aliases<-c(MBAHIAKRO='M’Bahiakro')
  x<-readRDS(input)
  negative<-x$profiles[x$profiles$side=='Supply'&is.finite(x$profiles$score)&x$profiles$score>1e-10,]
  stopifnot(!any(negative$unfavorable_components==1))
  candidates<-list()
  for(country in unique(negative$country)){
    pp<-negative[negative$country==country,]
    if(nrow(pp)<2)next
    for(pair in combn(seq_len(nrow(pp)),2,simplify=FALSE)){
      a<-pp[pair[1],];b<-pp[pair[2],]
      if(abs(a$score-b$score)>.1+1e-10)next
      aa<-x$components[x$components$side=='Supply'&x$components$country==country&x$components$geo==a$geo,]
      bb<-x$components[x$components$side=='Supply'&x$components$country==country&x$components$geo==b$geo,]
      bb<-bb[match(aa$variable,bb$variable),]
      pa<-ifelse(aa$variable=='stockout_burden',100-aa$raw_pct,aa$raw_pct)
      pb<-ifelse(bb$variable=='stockout_burden',100-bb$raw_pct,bb$raw_pct)
      eligible<-min(a$min_valid,b$min_valid)>=20&all(pa>0&pa<100)&all(pb>0&pb<100)
      candidates[[length(candidates)+1]]<-data.frame(country=country,geo_a=a$geo,geo_b=b$geo,
        readiness_a=-a$score,readiness_b=-b$score,score_gap=abs(a$score-b$score),
        below_average_a=a$unfavorable_components,below_average_b=b$unfavorable_components,
        minimum_valid=min(a$min_valid,b$min_valid),mean_absolute_pp=mean(abs(pa-pb)),
        maximum_pp=max(abs(pa-pb)),illustration_screen=eligible)
    }
  }
  candidates<-do.call(rbind,candidates)
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  write.csv(candidates,file.path(output_dir,'supply_candidate_pairs.csv'),row.names=FALSE)
  sections<-nav<-character(nrow(cases))
  esc<-function(s){s<-gsub('&','&amp;',s,fixed=TRUE);s<-gsub('<','&lt;',s,fixed=TRUE);gsub('>','&gt;',s,fixed=TRUE)}
  for(i in seq_len(nrow(cases))){
    cc<-cases[i,];q<-x$profiles[x$profiles$country==cc$country&x$profiles$side=='Supply'&x$profiles$geo%in%c(cc$a,cc$b),]
    stopifnot(nrow(q)==2,all(q$score>0),abs(diff(q$score))<=.1+1e-10)
    a<-q[q$geo==cc$a,];b<-q[q$geo==cc$b,]
    # Display-name corrections leave geographic keys unchanged.
    if(cc$a%in%names(aliases))a$area<-unname(aliases[cc$a])
    if(cc$b%in%names(aliases))b$area<-unname(aliases[cc$b])
    child<-file.path(output_dir,paste0(cc$id,'.html'))
    plot_supply_pair_table(input,cc$country,cc$a,cc$b,output_dir,child)
    h<-paste(readLines(child,warn=FALSE),collapse='\n')
    finding<-paste0('<div class="finding"><p class="comparison-label"><strong>What this comparison shows:</strong> ',esc(cc$comparison_type),'</p><p><strong>Key finding.</strong> ',esc(cc$takeaway),'</p></div>')
    h<-sub('</figure>',paste0('</figure>',finding),h,fixed=TRUE)
    sections[i]<-paste0('<section id="',cc$id,'" aria-labelledby="',cc$id,'-title"><h2 id="',cc$id,'-title">',i,'. ',esc(cc$label),' · ',esc(a$area),' and ',esc(b$area),'</h2>',h,'</section>')
    nav[i]<-paste0('<a href="#',cc$id,'">Supply ',i,': ',esc(a$area),' / ',esc(b$area),'</a>')
  }
  demand_html<-file.path(output_dir,'demand_comparisons.html')
  plot_regional_comparisons(input,output_dir,demand_html)
  h<-paste(readLines(demand_html,warn=FALSE),collapse='\n')
  h<-sub('<p class="intro">.*?</p>',paste0('<p class="intro">These comparisons show areas with similar overall scores but different challenges. Five demand comparisons pair areas with above-average demand concerns; eight supply comparisons pair areas with below-average supply readiness. <strong>Similar overall scores can reflect different barriers; the component profile shows what the score alone conceals.</strong></p>'),h,perl=TRUE)
  h<-sub('<p class="context">.*?</p>','<p class="context">Demand percentages describe women in need of family planning and use survey weights. Supply percentages summarize confirmed FP-providing facilities, with valid responses separately for each measure and no facility weights. Reference averages give each assessed area equal weight within its geography. Areas may be flagged for both demand and supply concerns.</p>',h,perl=TRUE)
  h<-sub('</nav>',paste0(paste(nav,collapse=''),'</nav>'),h,fixed=TRUE)
  h<-sub('<main>','<main><h2 class="group-title">Demand concerns</h2>',h,fixed=TRUE)
  introduction<-paste0('<div class="group-intro"><h2 class="group-title">Supply readiness</h2><p>Every pair below has negative supply-readiness scores. Higher percentages mean better readiness in all six rows; the stockout row is shown as <strong>stock continuity = 100% − stockout burden</strong>. Bold and orange shading identify values below the component reference average.</p><p>These examples distinguish different types of weakness, broader shortfalls and profiles dominated by one large shortfall.</p></div>')
  h<-sub('</main>',paste0(introduction,paste(sections,collapse=''),'<details><summary>How the supply examples were selected</summary><p>All four geographies were searched using the saved whole-area scores and component values. The eight illustrated pairs have negative readiness scores, an absolute score gap no larger than 0.10 average-z points, at least 20 valid FP facilities per component in each area, and no displayed 0% or 100% component. These screening rules select readable descriptive examples; they are not statistical adequacy thresholds. The pairs illustrate different contrasts rather than a representative sample of areas. No negative-readiness area had exactly one component below average. Sindh did not supply a pair meeting the illustration screen because some component denominators were smaller. The original scores, thresholds, classifications and models are unchanged.</p></details></main>'),h,fixed=TRUE)
  h<-sub('</style></head>','</style><style>.group-title{font-size:27px;margin:34px 0 20px;border-bottom:2px solid #D8E3E8;padding-bottom:10px}.group-intro{margin:45px 0 30px;max-width:1150px}.group-intro p{margin:10px 0}@media print{.group-intro{break-before:page}}</style></head>',h,fixed=TRUE)
  # The presentation report keeps its opening paragraph and comparison findings.
  # Detailed methods and value tables remain in standalone previews and source docs.
  h<-sub('(?s)<p class="context">.*?</header>','</header>',h,perl=TRUE)
  h<-gsub('(?s)<details><summary>Definitions and accessible values</summary>.*?</details>','',h,perl=TRUE)
  dir.create(dirname(html_output),recursive=TRUE,showWarnings=FALSE);writeLines(h,html_output,useBytes=TRUE)
  write.csv(cases[c('id','country','a','b','comparison_type')],file.path(output_dir,'selected_supply_pairs.csv'),row.names=FALSE)
  invisible(cases)
}

if(sys.nframe()==0L){
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
  if('--help'%in%args){cat('Rscript analysis/plot_supply_pair_table.R --input AGGREGATE_PROFILES [--output-dir output/supply_pair_tables] --html-output REPORT\n');quit(status=0)}
  script<-gsub('~+~',' ',sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]),fixed=TRUE)
  root<-normalizePath(file.path(dirname(script),'..'),mustWork=TRUE)
  build_supply_regional_comparisons(arg('--input'),arg('--output-dir','output/supply_pair_tables'),arg('--html-output'),root)
}
