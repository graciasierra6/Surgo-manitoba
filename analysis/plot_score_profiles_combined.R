# Plot aligned component percentages beside their contributions to an existing score.
# Input is the aggregate-only diagnostic produced by score_profile_examples.R.
plot_score_profiles_combined <- function(input, case_id, output_dir) {
  for(pkg in c('ggplot2','svglite'))if(!requireNamespace(pkg,quietly=TRUE))stop('Install ',pkg)
  x<-readRDS(input);q<-x$selected[[case_id]]
  if(is.null(q))stop('Unknown case: ',case_id)
  v<-q$components;areas<-c(q$a$geo,q$b$geo);area_names<-c(q$a$area,q$b$area)
  fields<-v$variable[v$geo==areas[1]];side<-as.character(q$case$side)
  stopifnot(length(fields)==6,nrow(v)==12,all(is.finite(v$raw_pct)),all(is.finite(v$contribution)))
  for(i in seq_along(areas))stopifnot(abs(sum(v$contribution[v$geo==areas[i]])-c(q$a$score,q$b$score)[i])<1e-10)
  v$percentage<-ifelse(side=='Supply'&!v$higher_raw_is_worse,100-v$raw_pct,v$raw_pct)
  v$reference<-ifelse(side=='Supply'&!v$higher_raw_is_worse,100-v$reference_raw_mean,v$reference_raw_mean)
  v$row<-7-match(v$variable,fields);v$area<-factor(v$geo,levels=areas,labels=area_names)
  v$y<-v$row+ifelse(v$geo==areas[1],.15,-.15)
  v$percentage_label<-sprintf('%.1f%%',v$percentage)
  v$contribution_label<-sprintf('%+.3f',v$contribution)
  reference<-v[v$geo==areas[1],]
  labels<-if(side=='Supply')c(offering='Short + long-acting offering\nGap: facilities not meeting standard (%)',
    free='Free-product provision\nGap: facility-average non-free share (%)',
    training='Trained-provider share\nGap: facility-average untrained share (%)',
    daily_provision='Daily provision of offered modern methods\nGap: facility-average non-daily share (%)',
    larc_stock='At least one LARC in stock\nGap: facilities without a stocked LARC (%)',
    stockout_burden='Stockout burden\nOriginal facility-average burden (%)')else setNames(v$label[match(fields,v$variable)],fields)
  if(side=='Demand')labels<-c(agency='Healthcare permission barriers',stigma='Anticipated social stigma',
    health='Health / side-effect beliefs',fertility='Future-fertility concerns',
    confidence='Low confidence choosing a method\nor managing side effects',opposes='Opposes family planning')
  colors<-setNames(c('#16768A','#C2782B'),area_names)
  theme<-ggplot2::theme_minimal(base_family='sans',base_size=11)+ggplot2::theme(
    panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor=ggplot2::element_blank(),
    panel.grid.major.x=ggplot2::element_line(color='#E5ECEF',linewidth=.4),
    plot.title=ggplot2::element_text(face='bold',size=14,color='#173E50'),
    plot.subtitle=ggplot2::element_text(size=10.5,color='#526D79'),
    axis.title=ggplot2::element_text(size=11,color='#294D5D'),
    axis.text=ggplot2::element_text(color='#375866',size=10.5),
    axis.text.y=ggplot2::element_text(lineheight=1.1),legend.position='none',
    plot.margin=ggplot2::margin(8,18,8,8))
  left<-ggplot2::ggplot(v,ggplot2::aes(percentage,y,color=area,shape=area))+
    ggplot2::geom_segment(data=reference,ggplot2::aes(x=reference,xend=reference,y=row-.34,yend=row+.34),
      inherit.aes=FALSE,color='#687E88',linewidth=.75)+
    ggplot2::geom_point(size=3.1)+
    ggplot2::geom_text(ggplot2::aes(label=percentage_label),hjust=0,nudge_x=2.2,size=3.2,show.legend=FALSE)+
    ggplot2::scale_color_manual(values=colors)+ggplot2::scale_shape_manual(values=c(16,18))+
    ggplot2::scale_y_continuous(limits=c(.4,6.6),breaks=6:1,labels=unname(labels[fields]),expand=c(0,0))+
    ggplot2::scale_x_continuous(limits=c(0,100),breaks=seq(0,100,25),expand=c(0,0))+
    ggplot2::labs(title=if(side=='Demand')'A. What women report'else'A. The underlying percentages',
      subtitle=if(side=='Demand')'Weighted % of women in need; higher = more concern'else'Higher = more challenging; grey ticks show the reference',
      x=if(side=='Demand')'Women reporting the concern (%)'else'Challenge measure (%)',y=NULL)+theme
  extent<-range(c(0,v$contribution));padding<-diff(extent)*.20
  right<-ggplot2::ggplot(v)+
    ggplot2::geom_vline(xintercept=0,color='#728A94',linewidth=.6)+
    ggplot2::geom_rect(ggplot2::aes(xmin=pmin(0,contribution),xmax=pmax(0,contribution),ymin=y-.075,ymax=y+.075,fill=area),color=NA)+
    ggplot2::geom_text(ggplot2::aes(x=contribution,y=y,label=contribution_label,
      hjust=ifelse(contribution>=0,-.13,1.13),color=area),size=3.1,show.legend=FALSE)+
    ggplot2::scale_fill_manual(values=colors)+ggplot2::scale_color_manual(values=colors)+
    ggplot2::scale_y_continuous(limits=c(.4,6.6),breaks=6:1,labels=NULL,expand=c(0,0))+
    ggplot2::scale_x_continuous(limits=extent+c(-padding,padding),breaks=c(-.2,0,.2,.4),expand=c(0,0))+
    ggplot2::labs(title=if(side=='Demand')'B. Why the overall scores look similar'else'B. How each measure enters the score',
      subtitle='Left of zero reduces the total; right of zero increases it',
      x=if(side=='Demand')'Contribution to the overall concern score'else'Contribution to challenge score (z / 6)',y=NULL)+theme+
    ggplot2::theme(axis.ticks.y=ggplot2::element_blank(),plot.margin=ggplot2::margin(8,20,8,12))
  gl<-ggplot2::ggplotGrob(left);gr<-ggplot2::ggplotGrob(right)
  stopifnot(length(gl$heights)==length(gr$heights));height<-grid::unit.pmax(gl$heights,gr$heights)
  gl$heights<-height;gr$heights<-height
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  svg<-file.path(output_dir,paste0(case_id,'_percentages_and_contributions.svg'))
  svglite::svglite(svg,width=16,height=7.6,bg='white')
  grid::grid.newpage();grid::pushViewport(grid::viewport(layout=grid::grid.layout(3,2,
    widths=grid::unit(c(.60,.40),'null'),heights=grid::unit(c(.14,.74,.12),'null'))))
  grid::pushViewport(grid::viewport(layout.pos.row=1,layout.pos.col=1:2))
  grid::grid.text(if(side=='Demand')paste0(q$a$country_label,': similar overall scores conceal different barriers')else paste0(q$a$country_label,': similar ',tolower(side),' scores, different component patterns'),
    x=.015,y=.79,just='left',gp=grid::gpar(fontsize=22,fontface='bold',col='#173E50'))
  for(i in 1:2) {
    xx<-c(.02,.255)[i]
    grid::grid.points(x=xx,y=.30,pch=c(16,18)[i],size=grid::unit(3,'mm'),gp=grid::gpar(col=colors[i],fill=colors[i]))
    grid::grid.text(sprintf(if(side=='Demand')'%s  |  overall concern score %+.3f'else'%s  |  total challenge score %+.3f',area_names[i],c(q$a$score,q$b$score)[i]),
      x=xx+.012,y=.30,just='left',gp=grid::gpar(fontsize=11.5,col='#294D5D'))
  }
  grid::grid.lines(x=c(.52,.52),y=c(.22,.38),gp=grid::gpar(col='#687E88',lwd=1.1))
  grid::grid.text(paste0('Reference: mean across ',q$a$reference_areas,' surveyed areas'),x=.535,y=.30,
    just='left',gp=grid::gpar(fontsize=11,col='#526D79'))
  grid::popViewport()
  grid::pushViewport(grid::viewport(layout.pos.row=2,layout.pos.col=1));grid::grid.draw(gl);grid::popViewport()
  grid::pushViewport(grid::viewport(layout.pos.row=2,layout.pos.col=2));grid::grid.draw(gr);grid::popViewport()
  grid::pushViewport(grid::viewport(layout.pos.row=3,layout.pos.col=1:2))
  caption<-if(side=='Supply')c(
    'First five rows show gaps: 100 minus the heatmap percentage. Stockout burden keeps its original percentage; higher = worse throughout.',
    'Offering/LARC gaps are percentages of FP facilities; other gaps/burden are averages of within-facility shares, using valid responses.',
    'Reference averages give each surveyed area equal weight. Challenge = negative readiness; the original scores and classifications are unchanged.')else c(
    sprintf('Women in need: %s n=%s; %s n=%s. Percentages use survey weights and valid responses separately for each domain.',area_names[1],q$a$n_need,area_names[2],q$b$n_need),
    'Grey ticks show equal-area averages across the surveyed départements. Each bar is the standardized component divided by six.',
    'The confidence measure is unchanged. These figures describe reported concerns and index composition, not causal effects on contraceptive use.')
  for(i in seq_along(caption))grid::grid.text(caption[i],x=.015,y=.78-(i-1)*.27,just='left',
    gp=grid::gpar(fontsize=10,col='#526D79'))
  grid::popViewport(2);grDevices::dev.off()
  stopifnot(all(v$percentage>=0&v$percentage<=100),all(v$reference>=0&v$reference<=100))
  write.csv(v[c('geo','area','variable','raw_pct','percentage','reference','risk_z','contribution','n_valid')],
    file.path(output_dir,paste0(case_id,'_combined_plot_values.csv')),row.names=FALSE)
  cat('Saved combined percentage and contribution figure for ',case_id,'\n',sep='')
  invisible(svg)
}

plot_score_profile_percentages <- function(input,case_id,output_dir,supply_score='challenge',supply_percentages='gaps') {
  supply_score<-match.arg(supply_score,c('challenge','readiness'))
  supply_percentages<-match.arg(supply_percentages,c('gaps','original'))
  for(pkg in c('ggplot2','svglite'))if(!requireNamespace(pkg,quietly=TRUE))stop('Install ',pkg)
  x<-readRDS(input);q<-x$selected[[case_id]]
  if(is.null(q)||!q$case$side%in%c('Demand','Supply'))stop('Select a saved demand or supply example')
  side<-as.character(q$case$side)
  display_scores<-c(q$a$score,q$b$score)*if(side=='Supply'&&supply_score=='readiness')-1 else 1
  v<-q$components;areas<-c(q$a$geo,q$b$geo);area_names<-c(q$a$area,q$b$area)
  fields<-v$variable[v$geo==areas[1]]
  stopifnot(length(fields)==6,nrow(v)==12,all(is.finite(v$raw_pct)),all(v$raw_pct>=0&v$raw_pct<=100))
  v$percentage<-ifelse(side=='Supply'&supply_percentages=='gaps'&!v$higher_raw_is_worse,100-v$raw_pct,v$raw_pct)
  v$row<-7-match(v$variable,fields);v$area<-factor(v$geo,levels=areas,labels=area_names)
  v$y<-v$row+ifelse(v$geo==areas[1],.16,-.16)
  labels<-c(agency='Healthcare permission barriers',stigma='Anticipated social stigma',
    health='Health / side-effect beliefs',fertility='Future-fertility concerns',
    confidence='Low confidence choosing a method\nor managing side effects',opposes='Opposes family planning')
  if(side=='Supply')labels<-c(offering='Short + long-acting offering\nGap: facilities not meeting standard',
    free='Free-product provision\nGap: average non-free product share',training='Trained-provider share\nGap: average untrained share',
    daily_provision='Daily provision of offered modern methods\nGap: average non-daily share',
    larc_stock='At least one LARC in stock\nGap: facilities without a stocked LARC',stockout_burden='Stockout burden')
  if(side=='Supply'&&supply_percentages=='original')labels<-c(
    offering='Short + long-acting offering\nHigher is better',free='Free-product provision\nHigher is better',
    training='Trained-provider share\nHigher is better',daily_provision='Daily provision of offered modern methods\nHigher is better',
    larc_stock='At least one LARC in stock\nHigher is better',stockout_burden='Stockout burden\nLower is better')
  colors<-setNames(c('#16768A','#C2782B'),area_names)
  v$text_color<-ifelse(v$percentage>92,'#FFFFFF',unname(colors[as.character(v$area)]))
  g<-ggplot2::ggplot(v)+
    ggplot2::geom_rect(ggplot2::aes(xmin=0,xmax=percentage,ymin=y-.115,ymax=y+.115,fill=area),color=NA)+
    ggplot2::geom_text(ggplot2::aes(x=pmin(percentage+1.3,98.5),y=y,label=sprintf('%.1f%%',percentage),color=text_color,
      hjust=ifelse(percentage>92,1,0)),
      size=4.1,show.legend=FALSE)+
    ggplot2::scale_fill_manual(values=colors)+ggplot2::scale_color_identity()+
    ggplot2::scale_y_continuous(limits=c(.45,6.55),breaks=6:1,labels=unname(labels[fields]),expand=c(0,0))+
    ggplot2::scale_x_continuous(limits=c(0,100),breaks=seq(0,100,25),labels=function(v)paste0(v,'%'),expand=c(0,0))+
    ggplot2::labs(x=if(side=='Demand')'Women reporting the concern · weighted %'else if(supply_percentages=='original')'Original heatmap percentages (%)'else'Service gaps or stockout burden · higher = worse (%)',y=NULL)+
    ggplot2::theme_minimal(base_family='sans',base_size=13)+ggplot2::theme(
      panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor=ggplot2::element_blank(),
      panel.grid.major.x=ggplot2::element_line(color='#E3EAED',linewidth=.4),
      axis.text=ggplot2::element_text(color='#31515F',size=12.5),
      axis.text.y=ggplot2::element_text(lineheight=1.1),axis.title.x=ggplot2::element_text(color='#31515F',size=12.5),
      legend.position='none',plot.margin=ggplot2::margin(5,35,8,15))
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  svg<-file.path(output_dir,paste0(case_id,'_presentation.svg'))
  svglite::svglite(svg,width=14,height=8,bg='white');grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout=grid::grid.layout(2,1,heights=grid::unit(c(.20,.80),'null'))))
  grid::pushViewport(grid::viewport(layout.pos.row=1))
  title<-if(!is.null(q$plot_title))q$plot_title else if(side=='Demand')'Similar overall scores, different reported concerns'else if(supply_score=='readiness'&&supply_percentages=='original')'Similar readiness scores, different supply profiles'else if(supply_score=='readiness')'Similar readiness scores, different service gaps'else'Similar supply scores, different service weaknesses'
  grid::grid.text(title,x=.025,y=.82,just='left',
    gp=grid::gpar(fontsize=23,fontface='bold',col='#173E50'))
  grid::grid.text(paste0(q$a$country_label,if(side=='Demand')' · women in need of family planning'else' · confirmed FP-provider facility summaries'),x=.025,y=.52,just='left',
    gp=grid::gpar(fontsize=13,col='#526D79'))
  for(i in 1:2) {
    xx<-c(.03,.49)[i]
    grid::grid.rect(x=xx,y=.17,width=.012,height=.11,gp=grid::gpar(fill=colors[i],col=NA))
    grid::grid.text(sprintf(if(side=='Demand')'%s  |  overall concern score %+.3f'else if(supply_score=='readiness')'%s  |  supply readiness score %+.3f'else'%s  |  supply constraint score %+.3f',area_names[i],display_scores[i]),
      x=xx+.014,y=.17,just='left',gp=grid::gpar(fontsize=13,col='#294D5D'))
  }
  grid::popViewport();grid::pushViewport(grid::viewport(layout.pos.row=2));grid::grid.draw(ggplot2::ggplotGrob(g));grid::popViewport()
  grid::popViewport();grDevices::dev.off()
  write.csv(v[c('geo','area','variable','raw_pct','percentage','n_valid')],file.path(output_dir,paste0(case_id,'_presentation_values.csv')),row.names=FALSE)
  cat('Saved percentage-only presentation figure for ',case_id,'\n',sep='')
  invisible(svg)
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
  if('--help'%in%args){cat('Rscript analysis/plot_score_profiles_combined.R --input AGGREGATE_EXAMPLES --case CASE [--view combined|percentages] [--supply-score challenge|readiness] [--supply-percentages gaps|original] [--output-dir output/combined_score_profiles]\n');quit(status=0)}
  view<-arg('--view','combined')
  if(!view%in%c('combined','percentages'))stop('Use --view combined or percentages')
  if(view=='percentages')plot_score_profile_percentages(arg('--input'),arg('--case','senegal_supply'),
    arg('--output-dir','output/combined_score_profiles'),arg('--supply-score','challenge'),arg('--supply-percentages','gaps'))else {
    if(arg('--supply-score','challenge')!='challenge'||arg('--supply-percentages','gaps')!='gaps')stop('Supply display options are available in the percentages view')
    plot_score_profiles_combined(arg('--input'),arg('--case','senegal_supply'),arg('--output-dir','output/combined_score_profiles'))
  }
}
