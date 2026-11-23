# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: haven, jsonlite, readr, tools.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Diagnostic/helper.
# Inputs: Caller-supplied files/objects and runtime arguments listed in this script.
# Outputs: Requested diagnostic tables/figures or helper results; see adjacent module README.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Shared survey-weighted estimation and geographic regression inference.
.survey_entry <- grep("^--file=", commandArgs(), value = TRUE)
.survey_root <- if (length(.survey_entry)) dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", .survey_entry[1]), fixed = TRUE), mustWork = TRUE)) else getwd()
while (!file.exists(file.path(.survey_root, "analysis/common/survey_methods.R"))) {
  .survey_parent <- dirname(.survey_root)
  if (identical(.survey_parent, .survey_root)) stop("Run this script within the complete project")
  .survey_root <- .survey_parent
}
source(file.path(.survey_root, "analysis/common/survey_methods.R"), local = TRUE)

# Read-only comparison of the authorized Sindh need rule with supplied FPN2.
# Output contains aggregate cross-tabs and agreement statistics only.
read_need_input <- function(path) {
  if(!file.exists(path))stop('Input does not exist')
  ext<-tolower(tools::file_ext(path))
  x<-switch(ext,rds=readRDS(path),dta=haven::read_dta(path),
    csv=readr::read_csv(path,col_types=readr::cols(.default=readr::col_character()),show_col_types=FALSE,progress=FALSE),stop('Use CSV, DTA or RDS input'))
  if(ext=='csv'&&nrow(readr::problems(x)))stop('Resolve CSV parsing problems before comparison')
  names(x)<-tolower(names(x));stopifnot(!anyDuplicated(names(x)))
  x
}

reconstruct_need_rule <- function(x) {
  required<-c('sf0b','sf0c','sf1a','sf1b','sf1c','sa0','sa1','pc0','sf2','pc18','pc19')
  if(!all(required%in%names(x)))stop('Missing need-definition fields')
  nv<-function(k)as.numeric(x[[k]])
  clauses<-data.frame(SF0B_1=nv('sf0b')%in%1,SF0C_1=nv('sf0c')%in%1,
    SF1_cannot_become_pregnant=nv('sf1a')%in%3|nv('sf1b')%in%3|nv('sf1c')%in%3,
    SA0_never_had_sex=nv('sa0')%in%2,SA1_over_12_months=nv('sa1')%in%c(5,6),
    SA1_special_95_97=nv('sa1')%in%c(95,97),PC0_currently_pregnant=nv('pc0')%in%1,
    SF2_wants_child_within_year=nv('sf2')%in%1,PC18_1=nv('pc18')%in%1,PC19_2=nv('pc19')%in%2)
  list(flag=as.integer(rowSums(clauses)==0),clauses=clauses)
}

check_need_concordance <- function(inputs,sindh_need_file,output_dir) {
  stopifnot(identical(names(inputs),c('lagos','senegal','cotedivoire','sindh')))
  for(p in c('haven','readr','jsonlite'))if(!requireNamespace(p,quietly=TRUE))stop('Install ',p)
  all_inputs<-c(inputs,sindh_need_file);hash_before<-unname(tools::md5sum(all_inputs))
  names_country<-c(lagos='Lagos',senegal='Senegal',cotedivoire='Côte d’Ivoire',sindh='Sindh')
  summaries<-cross_tabs<-reasons<-policy<-list()
  for(country in names(inputs)) {
    x<-read_need_input(inputs[[country]]);nv<-function(k)as.numeric(x[[k]])
    w<-nv(if(country=='sindh')'normalizedweights'else'weight')
    eligible<-nv('irb1')%in%1&is.finite(w)&w>0
    calc<-reconstruct_need_rule(x);orig<-nv('fpn2')
    valid<-eligible&orig%in%0:1
    if(any(valid)) {
      same<-orig==calc$flag;both<-valid&orig%in%1&calc$flag%in%1
      summaries[[country]]<-data.frame(geography=unname(names_country[country]),eligible_n=sum(eligible),
        supplied_binary_n=sum(valid),original_need_n=sum(valid&orig%in%1),reconstructed_need_n=sum(valid&calc$flag%in%1),
        both_need_n=sum(both),disagreement_n=sum(valid&!same),
        agreement_pct=100*mean(same[valid]),weighted_agreement_pct=100*survey_weighted_mean(same[valid],w[valid]),
        original_need_retained_pct=100*sum(both)/sum(valid&orig%in%1),
        weighted_original_need_retained_pct=100*survey_weighted_mean(as.numeric(both[valid&orig%in%1]),w[valid&orig%in%1]),
        reconstructed_only_need_n=sum(valid&orig%in%0&calc$flag%in%1))
      tab<-as.data.frame(table(original=orig[valid],reconstructed=calc$flag[valid]))
      tab$geography<-unname(names_country[country]);cross_tabs[[country]]<-tab
      bad<-valid&!same
      if(any(bad)) {
        keys<-apply(calc$clauses[bad,,drop=FALSE],1,function(v)paste(names(calc$clauses)[v],collapse=' + '))
        t<-as.data.frame(table(exclusion_pattern=keys));t$geography<-unname(names_country[country]);reasons[[country]]<-t
      }
    } else if(country!='sindh')stop('No valid supplied classification for ',country)
    if(country=='sindh') {
      ref<-read_need_input(sindh_need_file)
      stopifnot('col_id'%in%names(x),'col_id'%in%names(ref),'fpn2_derived'%in%names(ref),!anyDuplicated(x$col_id),!anyDuplicated(ref$col_id))
      ii<-match(as.character(x$col_id),as.character(ref$col_id));stopifnot(!anyNA(ii[eligible]))
      approved<-as.numeric(ref$fpn2_derived[ii])
      stopifnot(all(approved[eligible]==calc$flag[eligible]),sum(eligible&approved%in%1)==3274,all(orig==95))
      other<-rowSums(calc$clauses[,names(calc$clauses)!='SA1_special_95_97',drop=FALSE])==0
      affected<-eligible&other&calc$clauses$SA1_special_95_97
      policy<-data.frame(SA1_code=c(95,97),otherwise_eligible_excluded_n=c(sum(affected&nv('sa1')%in%95),sum(affected&nv('sa1')%in%97)))
      sindh<-list(source_n=nrow(x),eligible_n=sum(eligible),original_fpn2_all_95=TRUE,
        reconstructed_need_n=sum(eligible&calc$flag%in%1),matches_authorized_flag=TRUE,
        otherwise_eligible_SA1_special_exclusions=sum(affected))
    }
  }
  summary<-do.call(rbind,summaries);tab<-do.call(rbind,cross_tabs);reason<-do.call(rbind,reasons)
  stopifnot(identical(hash_before,unname(tools::md5sum(all_inputs))))
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  write.csv(summary,file.path(output_dir,'need_definition_agreement.csv'),row.names=FALSE)
  write.csv(tab,file.path(output_dir,'need_definition_cross_tabs.csv'),row.names=FALSE)
  write.csv(reason,file.path(output_dir,'need_definition_discrepancies.csv'),row.names=FALSE)
  write.csv(policy,file.path(output_dir,'sindh_sexual_activity_special_codes.csv'),row.names=FALSE)
  audit<-list(source_hashes_md5=setNames(hash_before,c(names(inputs),'authorized_sindh_need')),
    inputs_unchanged=TRUE,sindh=sindh,
    rule='Reproduce the authorized Sindh instrument-based exclusion rule, including SA1 codes 95/97. Expand FPN1 from hysterectomy, menopause and SF1A/B/C cannot-become-pregnant responses. Preserve the other pregnancy, fertility, sexual-activity and postpartum exclusions.',
    population='Consenting respondents with finite positive final survey weights. Original FPN2 must be binary for a concordance comparison. All original fields are read only.',
    interpretation='Agreement is descriptive, not external validation of an unavailable Sindh official classification. Original SURGO flags in the other countries remain authoritative for current analyses; no classifications or reports change.',
    missing_policy='Faithfully reproduces the authorized rule: only listed exclusion codes trigger exclusions. Special codes are not asserted to establish sexual inactivity. Read the disagreement breakdown before describing the definitions as exactly identical.')
  jsonlite::write_json(audit,file.path(output_dir,'verification.json'),auto_unbox=TRUE,pretty=TRUE,digits=12)
  print(summary,row.names=FALSE);print(reason,row.names=FALSE);print(policy,row.names=FALSE)
  cat('Sindh reconstruction exactly matches the authorized flag; no supplied classifications changed.\n')
  invisible(list(summary=summary,cross_tabs=tab,reasons=reason,sindh=sindh))
}

if(sys.nframe()==0L) {
  args<-commandArgs(trailingOnly=TRUE)
  arg<-function(k,default=NULL){i<-match(k,args);if(is.na(i)){if(is.null(default))stop('Supply ',k);return(default)};if(i==length(args))stop('Supply ',k);args[i+1L]}
  if('--help'%in%args){cat('Rscript analysis/check_need_definition_concordance.R --lagos-data DATA --senegal-data DATA --cotedivoire-data DATA --sindh-data DATA --sindh-need-file DATA [--output-dir output/need_definition_concordance]\n');quit(status=0)}
  inputs<-c(lagos=arg('--lagos-data'),senegal=arg('--senegal-data'),cotedivoire=arg('--cotedivoire-data'),sindh=arg('--sindh-data'))
  check_need_concordance(inputs,arg('--sindh-need-file'),arg('--output-dir','output/need_definition_concordance'))
}
