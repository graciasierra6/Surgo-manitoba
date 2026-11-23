# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: ggplot2, grid, jsonlite, rmarkdown, svglite, tools.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Profile diagnostic entry.
# Inputs: Saved Lagos, Senegal and Côte d’Ivoire analysis directories.
# Outputs: Score_profile_examples.html plus aggregate comparison tables/figures.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Compare similar administrative-area scores with different component profiles.
# Reads saved aggregate report inputs only; never changes the working reports.
build_score_profile_examples<-function(data_dirs,work_dir){
 for(pkg in c("jsonlite","ggplot2","svglite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
 dir.create(work_dir,recursive=TRUE,showWarnings=FALSE)
 country_names<-c(lagos="Lagos",senegal="Senegal",cotedivoire="Côte d’Ivoire",sindh="Sindh")
 aliases<-c(IFAKOIJAYE="Ifako-Ijaye",IBEJULEKKI="Ibeju-Lekki",AJEROMIIFELODUN="Ajeromi-Ifelodun",AMUWOODOFIN="Amuwo-Odofin",LAGOSISLAND="Lagos Island",LAGOSMAINLAND="Lagos Mainland",ETIOSA="Eti-Osa",MBACKE="Mbacké",KOUNFAO="Koun-Fao",YAMOUSSOUKRO="Yamoussoukro",DISTRICTKEMARI="Kemari",QAMBARSHAHDADKOT="Qambar Shahdadkot",MEAGUI="Méagui")
 pretty<-function(g,p){if(g%in%names(aliases))return(unname(aliases[g]));q<-p$coverage[p$coverage$geo==g,,drop=FALSE];tools::toTitleCase(tolower(if(nrow(q)&&"name"%in%names(q))q$name[1]else g))}
 component_labels<-c(agency="Healthcare permission",stigma="Anticipated social stigma",health="Health / side-effect beliefs",fertility="Future-fertility concerns",confidence="Low method knowledge*",opposes="Opposes family planning",offering="Short + long-acting offering",free="Free-product provision",training="Trained-provider share",daily_provision="Daily method provision",larc_stock="At least one LARC in stock",stockout_burden="Stockout burden")
 profiles<-components<-candidates<-list();hashes<-list();source_checks<-list()
 for(country in names(data_dirs)){
  input<-file.path(data_dirs[[country]],"report_data.json");p<-jsonlite::fromJSON(input)
  hashes[[country]]<-unname(tools::md5sum(input));sc<-p$scores[p$scores$method=="zscore",]
  stopifnot(length(p$demand_variables)==6,length(p$supply_variables)==6)
  for(side in c("Demand","Supply")){
   fields<-if(side=="Demand")p$demand_variables else p$supply_variables
   for(i in seq_len(nrow(sc))){
    g<-sc$geo[i];tr<-p$component_trace[p$component_trace$side==side&p$component_trace$geo==g,];tr<-tr[match(fields,tr$variable),]
    stopifnot(nrow(tr)==6,identical(as.character(tr$variable),fields))
    if(any(!is.finite(tr$zscore)))next
    direction<-if(side=="Demand" || identical(p$audit$supply_score_orientation,"constraints"))1 else -1
    z<-tr$zscore*direction
    expected<-if(side=="Demand")sc$demand_score[i]else direction*sc$supply_score[i]
    stopifnot(abs(mean(z)-expected)<1e-9)
    hm<-p$heatmap[p$heatmap$geo==g&p$heatmap$side==side,];hm<-hm[match(fields,hm$variable),]
    stopifnot(max(abs(hm$value-tr$original_pct))<1e-8)
    if(side=="Demand")stopifnot(length(unique(hm$n_total))==1)
    coverage<-p$coverage[p$coverage$geo==g,];nr<-p$rates[p$rates$geo==g,]
    n_need<-if(side=="Demand")unique(hm$n_total)else if(nrow(nr)&&"n"%in%names(nr))nr$n[1]else NA_real_
    n_fac<-if("fp_facilities"%in%names(coverage))coverage$fp_facilities[1]else max(hm$n_total)
    pos<-pmax(z,0);neg<-pmin(z,0);breadth<-sum(z>1e-10);top<-which.max(z)
    concentration<-if(sum(pos)>0)max(pos)/sum(pos)else 0
    key<-paste(country,side,g,sep="|")
    profiles[[key]]<-data.frame(country=country,country_label=unname(country_names[country]),side=side,geo=g,area=pretty(g,p),score=expected,unfavorable_components=breadth,components_z_at_least_half=sum(z>=.5),largest_positive_component=fields[top],largest_positive_share=concentration,positive_total=sum(pos)/6,negative_total=sum(neg)/6,n_need=n_need,fp_facilities=n_fac,min_valid=min(hm$n_valid),max_valid=max(hm$n_valid),reference_areas=if("reference_n"%in%names(tr))unique(tr$reference_n)else if(side=="Demand")sum(is.finite(sc$demand_score))else sum(is.finite(sc$supply_score)))
    components[[key]]<-data.frame(country=country,side=side,geo=g,area=pretty(g,p),variable=fields,label=unname(component_labels[fields]),raw_pct=tr$original_pct,reference_raw_mean=if(side=="Supply" && identical(p$audit$supply_score_orientation,"constraints"))ifelse(fields=="stockout_burden",tr$indicator_mean,100-tr$indicator_mean)else ifelse(fields=="stockout_burden",100-tr$indicator_mean,tr$indicator_mean),risk_z=z,contribution=z/6,n_valid=hm$n_valid,n_total=hm$n_total,higher_raw_is_worse=side=="Demand"|fields=="stockout_burden")
   }
  }
  source_checks[[country]]<-data.frame(country=country,source_rows=p$audit$source_rows,need_n=p$audit$need_n,source_report_md5=hashes[[country]])
 }
 profiles<-do.call(rbind,profiles);components<-do.call(rbind,components)
 # Search complete profiles with an above-average challenge score on the same side.
 for(country in names(data_dirs))for(side in c("Demand","Supply")){
  a<-profiles[profiles$country==country&profiles$side==side&profiles$score>1e-10,]
  if(nrow(a)<2)next
  for(pair in combn(seq_len(nrow(a)),2,simplify=FALSE)){
   aa<-a[pair[1],];bb<-a[pair[2],];gap<-abs(aa$score-bb$score)
   if(gap>.1+1e-10)next
   x<-components[components$country==country&components$side==side&components$geo==aa$geo,]
   y<-components[components$country==country&components$side==side&components$geo==bb$geo,];y<-y[match(x$variable,y$variable),]
   candidates[[length(candidates)+1]]<-data.frame(country=country,side=side,area_a=aa$area,geo_a=aa$geo,score_a=aa$score,area_b=bb$area,geo_b=bb$geo,score_b=bb$score,score_gap=gap,breadth_a=aa$unfavorable_components,breadth_b=bb$unfavorable_components,largest_positive_share_a=aa$largest_positive_share,largest_positive_share_b=bb$largest_positive_share,mean_absolute_component_difference=mean(abs(x$risk_z-y$risk_z)),adequate_for_illustration=if(side=="Demand")min(aa$min_valid,bb$min_valid)>=50 else min(aa$min_valid,bb$min_valid)>=20)
  }
 }
 candidates<-do.call(rbind,candidates)
 # Illustrative choices from the candidate search, not estimates of causal drivers.
 cases<-data.frame(country=c("lagos","lagos","senegal","senegal","cotedivoire","cotedivoire"),side=rep(c("Demand","Supply"),3),geo_a=c("IFAKOIJAYE","IKORODU","MBACKE","GOUDIRY","DIVO","KOUNFAO"),geo_b=c("APAPA","IBEJULEKKI","BIGNONA","MATAM","YAMOUSSOUKRO","GAGNOA"),stringsAsFactors=FALSE)
 cases$case_id<-paste(cases$country,tolower(cases$side),sep="_")
 selected<-list();plots<-list()
 for(i in seq_len(nrow(cases))){
  cc<-cases[i,];a<-profiles[profiles$country==cc$country&profiles$side==cc$side&profiles$geo==cc$geo_a,];b<-profiles[profiles$country==cc$country&profiles$side==cc$side&profiles$geo==cc$geo_b,]
  stopifnot(nrow(a)==1,nrow(b)==1,a$score>0,b$score>0,abs(a$score-b$score)<=.1+1e-10)
  stopifnot(if(cc$side=="Demand")min(a$min_valid,b$min_valid)>=50 else min(a$min_valid,b$min_valid)>=20)
  v<-components[components$country==cc$country&components$side==cc$side&components$geo%in%c(cc$geo_a,cc$geo_b),]
  fields<-v$variable[v$geo==cc$geo_a];v$row<-factor(v$variable,levels=rev(fields))
  panel_labels<-sprintf("%s  |  total %+.3f\n%d of 6 components above-average challenges",c(a$area,b$area),c(a$score,b$score),c(a$unfavorable_components,b$unfavorable_components))
  v$panel<-factor(v$geo,levels=c(cc$geo_a,cc$geo_b),labels=panel_labels)
  v$direction<-ifelse(v$risk_z>0,"More challenging than reference","More favorable than reference")
  hue<-if(cc$side=="Demand")"#087f8a"else"#c7772c"
  labels<-setNames(v$label[match(fields,v$variable)],fields)
  g<-ggplot2::ggplot(v,ggplot2::aes(contribution,row,fill=direction))+
   ggplot2::geom_vline(xintercept=0,color="#6c8490",linewidth=.55)+ggplot2::geom_col(width=.58)+
   ggplot2::geom_text(ggplot2::aes(label=sprintf("%.1f%%",raw_pct),hjust=ifelse(contribution>=0,-.12,1.12)),size=3.4,color="#254252")+
   ggplot2::facet_wrap(~panel,nrow=1)+ggplot2::scale_y_discrete(labels=labels)+
   ggplot2::scale_fill_manual(values=c("More challenging than reference"=hue,"More favorable than reference"="#b7c7ce"))+
   ggplot2::scale_x_continuous(expand=ggplot2::expansion(mult=c(.22,.24)))+
   ggplot2::labs(title=paste(unname(country_names[cc$country]),tolower(cc$side),"profiles: similar totals, different components"),
    subtitle="Bars are signed contributions to the total score (component z / 6). Labels show the underlying percentages.",
    x="Contribution to the overall challenge score",y=NULL,fill=NULL,
    caption=paste0(if(cc$side=="Demand")"Percentages: survey-weighted women in need, valid answers separately by domain."else"Percentages: unweighted FP-provider summaries, valid facilities separately by measure; higher stockout burden is worse.",
     "\nArea-level planning profiles from the Rural/Urban reports. Positive contributions add challenges; negative contributions offset them.",
     if(cc$side=="Demand")"\n*Retained knowledge label measures confidence choosing a method or managing side effects. These are score components, not proven causes of nonuse."else"\nThese are contributions to an index, not estimated causal effects on contraceptive use."))+
   ggplot2::theme_minimal(base_family="sans",base_size=11)+ggplot2::theme(panel.grid.major.y=ggplot2::element_blank(),panel.grid.minor=ggplot2::element_blank(),strip.background=ggplot2::element_rect(fill="#edf4f6",color=NA),strip.text=ggplot2::element_text(face="bold",color="#17374a",lineheight=1.2),plot.title=ggplot2::element_text(face="bold",size=17,color="#17374a"),plot.subtitle=ggplot2::element_text(size=10,color="#617b88"),plot.caption=ggplot2::element_text(hjust=0,size=9,color="#617b88",lineheight=1.2),legend.position="bottom",legend.text=ggplot2::element_text(size=9),panel.spacing=grid::unit(1.5,"lines"),plot.margin=ggplot2::margin(15,20,12,12))
  svg<-paste0(cc$case_id,".svg");png<-paste0(cc$case_id,".png")
  ggplot2::ggsave(file.path(work_dir,svg),g,width=14,height=6.8,device=svglite::svglite,bg="white")
  ggplot2::ggsave(file.path(work_dir,png),g,width=14,height=6.8,dpi=170,bg="white")
  selected[[cc$case_id]]<-list(case=cc,a=a,b=b,components=v,svg=svg,png=png,score_gap=abs(a$score-b$score))
 }
 write.csv(profiles,file.path(work_dir,"all_area_profile_metrics.csv"),row.names=FALSE,na="")
 write.csv(components,file.path(work_dir,"all_component_contributions.csv"),row.names=FALSE,na="")
 write.csv(candidates,file.path(work_dir,"similar_score_candidate_pairs.csv"),row.names=FALSE,na="")
 write.csv(cases,file.path(work_dir,"selected_examples.csv"),row.names=FALSE)
 audit<-list(countries=names(data_dirs),selected_cases=nrow(cases),maximum_selected_score_gap=max(vapply(selected,function(v)v$score_gap,numeric(1))),source_checks=do.call(rbind,source_checks),source_report_md5=hashes,
  method="Compare above-average area planning challenge scores with absolute gap <=0.10. Demand uses original risk z; supply uses negative readiness z for a common higher=worse display. Existing scores and classifications are unchanged.",
  selection="Illustrative pairs chosen from the search for clear profile differences and valid denominators (at least 50 per demand domain or 20 per supply measure). These are exploratory rules, not clinical thresholds or tests of regional differences.",
  concentration="Largest positive z / sum of positive z, excluding favorable negative components. This is an arithmetic profile statistic, not a share of the net score or of nonuse.",
  precision="No inference about profile differences is made. Raw demand percentages use survey weights; area standardization references and facility means are unweighted across their respective records. Small area/item Ns and overlapping indicators limit interpretation.")
 result<-list(selected=selected,profiles=profiles,components=components,candidates=candidates,audit=audit,labels=component_labels)
 saveRDS(result,file.path(work_dir,"score_profile_examples.rds"))
 jsonlite::write_json(audit,file.path(work_dir,"verification.json"),auto_unbox=TRUE,pretty=TRUE,dataframe="rows",digits=12)
 for(c in names(data_dirs))stopifnot(identical(hashes[[c]],unname(tools::md5sum(file.path(data_dirs[[c]],"report_data.json")))))
 cat("Verified component sums for",nrow(profiles),"area-side profiles; selected six pairs without modifying source reports.\n")
 normalizePath(work_dir,mustWork=TRUE)
}

if(sys.nframe()==0L){
 args<-commandArgs(trailingOnly=TRUE)
 arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop("Supply ",k);return(default)};if(i==length(args))stop("Supply ",k);args[i+1L]}
 if("--help"%in%args){cat("Rscript analysis/score_profile_examples.R --lagos-dir ANALYSIS --senegal-dir ANALYSIS --cotedivoire-dir ANALYSIS [--work-dir output/score_profiles] [--output-dir reports] [--pandoc-dir DIRECTORY]\n");quit(status=0)}
 script<-gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE)
 root<-normalizePath(file.path(dirname(script),".."),mustWork=TRUE)
 source(file.path(root,"analysis/common/pipeline.R"));use_project_library(root)
 resolve<-function(p)project_path(p,root)
 dirs<-lapply(c(lagos=arg("--lagos-dir"),senegal=arg("--senegal-dir"),cotedivoire=arg("--cotedivoire-dir")),resolve)
 work<-build_score_profile_examples(dirs,resolve(arg("--work-dir","output/score_profiles")))
 pandoc<-arg("--pandoc-dir",Sys.getenv("RSTUDIO_PANDOC",""));if(nzchar(pandoc))Sys.setenv(RSTUDIO_PANDOC=pandoc)
 out<-resolve(arg("--output-dir","reports"));dir.create(out,recursive=TRUE,showWarnings=FALSE)
 rmarkdown::render(file.path(root,"analysis/score_profile_examples.Rmd"),output_file="Score_profile_examples.html",output_dir=normalizePath(out),params=list(prepared_dir=work),quiet=TRUE,envir=new.env(parent=globalenv()))
 cat("Saved Score_profile_examples.html\n")
}
