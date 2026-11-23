# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: jsonlite, rmarkdown.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Profile diagnostic entry.
# Inputs: Saved main country analysis directories, including Sindh district results.
# Outputs: score_profile_contrasts.rds and aggregate profile comparison HTML.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Extend the saved-profile comparison to all four geographies.
# Only aggregate report inputs are read. No scores, models or source reports change.
build_score_profile_contrasts <- function(data_dirs, work_dir, project_root) {
  source(file.path(project_root, 'analysis/score_profile_examples.R'))
  source(file.path(project_root, 'analysis/plot_score_profiles_combined.R'))
  build_score_profile_examples(data_dirs, file.path(work_dir, 'profile_audit'))
  x <- readRDS(file.path(work_dir, 'profile_audit/score_profile_examples.rds'))
  pairs <- list()
  for (country in names(data_dirs)) for (side in c('Demand','Supply')) {
    p <- x$profiles[x$profiles$country==country & x$profiles$side==side & x$profiles$score>1e-10,]
    if(nrow(p)<2) next
    for(ii in combn(seq_len(nrow(p)),2,simplify=FALSE)) {
      a<-p[ii[1],];b<-p[ii[2],]
      ca<-x$components[x$components$country==country & x$components$side==side & x$components$geo==a$geo,]
      cb<-x$components[x$components$country==country & x$components$side==side & x$components$geo==b$geo,]
      cb<-cb[match(ca$variable,cb$variable),]
      difference<-abs(ca$raw_pct-cb$raw_pct);j<-which.max(difference)
      pairs[[length(pairs)+1]]<-data.frame(country=country,country_label=a$country_label,side=side,
        geo_a=a$geo,area_a=a$area,score_a=a$score,geo_b=b$geo,area_b=b$area,score_b=b$score,
        score_gap=abs(a$score-b$score),breadth_a=a$unfavorable_components,breadth_b=b$unfavorable_components,
        concentration_a=a$largest_positive_share,concentration_b=b$largest_positive_share,
        minimum_valid=min(a$min_valid,b$min_valid),mean_absolute_pp=mean(difference),maximum_pp=max(difference),
        maximum_gap_component=ca$variable[j],maximum_gap_label=ca$label[j],
        maximum_gap_pct_a=ca$raw_pct[j],maximum_gap_pct_b=cb$raw_pct[j],
        near_equal=abs(a$score-b$score)<=.1+1e-10,
        illustration_n=min(a$min_valid,b$min_valid)>=(if(side=='Demand')50 else 20),
        broad_vs_concentrated=(a$unfavorable_components>=5 & b$largest_positive_share>=.7) |
          (b$unfavorable_components>=5 & a$largest_positive_share>=.7))
    }
  }
  pairs<-do.call(rbind,pairs)
  pairs<-pairs[order(pairs$country,pairs$side,-pairs$mean_absolute_pp,pairs$score_gap),]
  overview<-do.call(rbind,lapply(names(data_dirs),function(country) {
    p<-pairs[pairs$country==country & pairs$side=='Demand' & pairs$near_equal & pairs$illustration_n,]
    stopifnot(nrow(p)>0);p[which.max(p$mean_absolute_pp),]
  }))
  stopifnot(sum(pairs$side=='Demand' & pairs$near_equal & pairs$broad_vs_concentrated)==0)
  cases<-data.frame(case_id=c('sindh_sharp_demand','cdi_broader_demand','senegal_broad_demand','cdi_concentrated_supply'),
    country=c('sindh','cotedivoire','senegal','cotedivoire'),side=c('Demand','Demand','Demand','Supply'),
    geo_a=c('DISTRICTKEMARI','DIVO','DAGANA','ABIDJAN'),geo_b=c('KHAIRPUR','DABOU','BIGNONA','GAGNOA'))
  selected<-list()
  for(i in seq_len(nrow(cases))) {
    cc<-cases[i,]
    a<-x$profiles[x$profiles$country==cc$country & x$profiles$side==cc$side & x$profiles$geo==cc$geo_a,]
    b<-x$profiles[x$profiles$country==cc$country & x$profiles$side==cc$side & x$profiles$geo==cc$geo_b,]
    stopifnot(nrow(a)==1,nrow(b)==1,a$score>0,b$score>0)
    v<-x$components[x$components$country==cc$country & x$components$side==cc$side &
      x$components$geo%in%c(cc$geo_a,cc$geo_b),]
    for(gg in c(cc$geo_a,cc$geo_b)) stopifnot(abs(sum(v$contribution[v$geo==gg])-
      x$profiles$score[x$profiles$country==cc$country & x$profiles$side==cc$side & x$profiles$geo==gg])<1e-9)
    selected[[cc$case_id]]<-list(case=cc,a=a,b=b,components=v,score_gap=abs(a$score-b$score))
  }
  selected$senegal_broad_demand$plot_title<-'Broader concerns versus a concentrated confidence concern'
  x$selected<-selected;x$pairs<-pairs;x$overview<-overview
  x$labels['confidence']<-'Low confidence choosing a method or managing side effects'
  x$audit$selection<-paste('Four-geography search within country; complete profiles and positive totals.',
    'Near-equal means absolute score gap <= 0.10 in the original average-z units.',
    'Demand overview maximizes mean absolute percentage-point contrast among pairs with at least 50 valid women per domain.',
    'Broad means at least five positive component z-values; concentrated means one supplies at least 70% of positive component z-points.',
    'The separate Dagana/Bignona example relaxes score similarity and the 50-response screening rule and is labelled accordingly.')
  x$audit$demand_profiles<-sum(x$profiles$side=='Demand')
  x$audit$supply_profiles<-sum(x$profiles$side=='Supply')
  x$audit$strict_near_equal_demand_pairs<-sum(pairs$side=='Demand' & pairs$near_equal & pairs$broad_vs_concentrated)
  x$audit$strict_near_equal_supply_pairs<-sum(pairs$side=='Supply' & pairs$near_equal & pairs$broad_vs_concentrated)
  x$audit$selected_cases<-nrow(cases)
  saveRDS(x,file.path(work_dir,'score_profile_contrasts.rds'))
  write.csv(pairs,file.path(work_dir,'all_positive_profile_pairs.csv'),row.names=FALSE,na='')
  write.csv(overview,file.path(work_dir,'four_geography_demand_overview.csv'),row.names=FALSE,na='')
  for(id in names(selected))plot_score_profile_percentages(file.path(work_dir,'score_profile_contrasts.rds'),id,work_dir,
    supply_score='challenge',
    supply_percentages=if(selected[[id]]$case$side=='Supply')'original'else'gaps')
  jsonlite::write_json(x$audit,file.path(work_dir,'verification.json'),pretty=TRUE,auto_unbox=TRUE,dataframe='rows',digits=12)
  cat('Verified four-geography contrasts without refitting or changing source reports.\n')
  invisible(x)
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
  if('--help'%in%args){cat('Rscript analysis/score_profile_contrasts.R --lagos-dir ANALYSIS --senegal-dir ANALYSIS --cotedivoire-dir ANALYSIS --sindh-dir ANALYSIS [--work-dir output/score_profile_contrasts] [--output-dir reports] [--pandoc-dir DIRECTORY]\n');quit(status=0)}
  script<-gsub('~+~',' ',sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]),fixed=TRUE)
  root<-normalizePath(file.path(dirname(script),'..'),mustWork=TRUE)
  source(file.path(root,'analysis/common/pipeline.R'));use_project_library(root)
  resolve<-function(p)project_path(p,root)
  dirs<-lapply(c(lagos=arg('--lagos-dir'),senegal=arg('--senegal-dir'),cotedivoire=arg('--cotedivoire-dir'),sindh=arg('--sindh-dir')),resolve)
  work<-resolve(arg('--work-dir','output/score_profile_contrasts'));dir.create(work,recursive=TRUE,showWarnings=FALSE)
  build_score_profile_contrasts(dirs,work,root)
  pandoc<-arg('--pandoc-dir',Sys.getenv('RSTUDIO_PANDOC',''));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
  out<-resolve(arg('--output-dir','reports'));dir.create(out,recursive=TRUE,showWarnings=FALSE)
  rmarkdown::render(file.path(root,'analysis/score_profile_contrasts.Rmd'),output_file='Score_profile_contrasts.html',
    output_dir=normalizePath(out),params=list(prepared_dir=work),quiet=TRUE,envir=new.env(parent=globalenv()))
}
