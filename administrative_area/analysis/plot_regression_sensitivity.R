# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: ggplot2, jsonlite, svglite.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Figure stage.
# Inputs: Saved aggregate regression sensitivity results.
# Outputs: Coefficient-comparison figures; no model refitting.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Plot aggregate coefficient comparisons using the existing report palette.
args<-commandArgs(TRUE);arg<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
out<-arg("--data-dir");p<-jsonlite::fromJSON(file.path(out,"sensitivity_results.json"))
D<-p$demand_variables;labels<-setNames(p$demand_labels,D)
labels<-c(labels,supply_risk="Supply risk score",demand_score="Demand risk score")
wrap<-function(x)vapply(x,function(v)paste(strwrap(v,31),collapse="\n"),character(1))
library(ggplot2)
theme_report<-theme_minimal(base_size=12,base_family="Arial")+
 theme(panel.grid.minor=element_blank(),panel.grid.major.y=element_blank(),
  axis.title.y=element_blank(),axis.text=element_text(color="#244657"),
  plot.title=element_text(face="bold",color="#17374a",size=16),
  legend.position="top",legend.title=element_blank(),
  strip.text.y=element_text(face="bold",color="#17374a"),
  plot.margin=margin(12,18,12,12))
save_plot<-function(plot,name,height){
 file<-file.path(out,name);ggsave(file,plot,width=10.2,height=height,device=svglite::svglite,bg="white")
 ggsave(sub("[.]svg$",".png",file),plot,width=10.2,height=height,dpi=144,bg="white")
 text<-readLines(file,warn=FALSE)
 text<-text[!grepl("<!--.*Created with",text)]
 writeLines(text,file,useBytes=TRUE)
}
a<-p$available_coefficients
a<-a[(a$model=="supply"&a$term=="supply_risk")|
 (startsWith(a$model,"single_")&a$term==sub("^single_","",a$model)),]
a$component<-factor(a$term,levels=rev(c("supply_risk",D)))
a$sample<-factor(a$sample,levels=c("common","available"),labels=c("Common sample","Available cases"))
pos<-position_dodge(width=.58)
g<-ggplot(a,aes(x=odds_ratio,y=component,color=sample))+
 geom_vline(xintercept=1,linetype="dashed",color="#aabcc6",linewidth=.5)+
 geom_errorbar(aes(xmin=or_lo,xmax=or_hi),orientation="y",width=.17,position=pos,linewidth=.6)+
 geom_point(position=pos,size=2.7)+scale_x_log10()+
 scale_y_discrete(labels=function(x)wrap(labels[x]))+
 scale_color_manual(values=c("Common sample"="#74838d","Available cases"="#047f8b"))+
 labs(title="Adjusted associations in common and broader eligible samples",x="Adjusted odds ratio · 95% confidence interval")+theme_report
save_plot(g,"available_case_comparison.svg",6.3)
b<-p$joint_coefficients
b<-b[(b$model=="score_joint"&b$term%in%c("demand_score","supply_risk"))|
 (b$model=="components_joint"&b$term%in%c(D,"supply_risk")),]
b$key<-paste(b$model,b$term)
keys<-c(paste("score_joint",c("demand_score","supply_risk")),paste("components_joint",c(D,"supply_risk")))
b$key<-factor(b$key,levels=rev(keys))
key_labels<-setNames(labels[sub("^.* ","",keys)],keys)
b$model_label<-factor(b$model,levels=c("score_joint","components_joint"),
 labels=c("Demand + supply\nscores","Six demand indicators\n+ supply score"))
b$weighting<-factor(b$weighting,levels=c("survey_only","response_weighted"),
 labels=c("Survey weights","Survey + response weights"))
g<-ggplot(b,aes(x=odds_ratio,y=key,color=weighting))+
 geom_vline(xintercept=1,linetype="dashed",color="#aabcc6",linewidth=.5)+
 geom_errorbar(aes(xmin=or_lo,xmax=or_hi),orientation="y",width=.17,position=pos,linewidth=.6)+
 geom_point(position=pos,size=2.7)+scale_x_log10()+
 scale_y_discrete(labels=function(x)wrap(key_labels[x]))+
 scale_color_manual(values=c("Survey weights"="#74838d","Survey + response weights"="#047f8b"))+
 facet_grid(model_label~.,scales="free_y",space="free_y")+
 labs(title="Joint associations with original and response-adjusted weights",x="Adjusted odds ratio · 95% area-jackknife confidence interval")+
 theme_report+theme(strip.text.y=element_text(angle=0,size=10),strip.placement="outside")
save_plot(g,"response_weighting_comparison.svg",7.7)
cat("Saved two aggregate sensitivity coefficient figures.\n")
