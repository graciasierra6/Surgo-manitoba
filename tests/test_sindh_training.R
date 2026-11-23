# Dependencies and input/output reference
# Setup once from repository root: Rscript R/install_dependencies.R
# Direct R packages: bundled R only; dependencies may be supplied by the caller.
# Pipeline packages/versions: DESCRIPTION and renv.lock; see docs/SCRIPT_REFERENCE.md.
# Role: Test/validation utility.
# Inputs: Test fixtures or caller-supplied earlier/current saved results, as specified in the
#   code.
# Outputs: Assertions, validation records or aggregate before/after comparisons.
# Source dataset names/roles: docs/COLLEAGUE_RUN_GUIDE.md (D01-D14 and supporting files).
# Run instructions and runtime input/output locations: docs/COLLEAGUE_RUN_GUIDE.md.
# External data locations are caller parameters; keep working data out of shared files.

# Exercise substantive statuses, both encodings, conflicts and facility ratios.
entry<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
root<-normalizePath(file.path(dirname(entry),".."),mustWork=TRUE)
source(file.path(root,"administrative_area/analysis/sindh/training_reconstruction.R"))
cases<-c("trained_text","none_text","dont_know_text","excluded_topic_text",
 "blank","malformed","trained_binary","no_binary","partial_no",
 "dont_know_binary","none_binary","text_binary_conflict","none_and_topic",
 "dont_know_and_topic","old_no_with_dont_know","second_person_unknown")
r<-data.frame(q406_1=rep("",length(cases)),q406_2=rep("",length(cases)))
for(slot in 1:2)for(k in c(letters[1:8],"y","z"))r[[paste0("q406_",k,"_",slot)]]<-NA_real_
rownames(r)<-cases
r$q406_1<-c("A C","Z","Y","B","","not recorded","","","","","","A","A Z","A Y","Y","A")
r["trained_binary","q406_a_1"]<-1
r["no_binary",paste0("q406_",c("a","c","d","g","h"),"_1")]<-0
r["partial_no","q406_a_1"]<-0
r["dont_know_binary","q406_y_1"]<-1
r["none_binary","q406_z_1"]<-1
r["text_binary_conflict","q406_a_1"]<-0
r["old_no_with_dont_know",paste0("q406_",c("a","c","d","g","h"),"_1")]<-0
r["second_person_unknown","q406_2"]<-"Y"
t<-sindh_training(r)
expected<-c(1,0,NA,0,NA,NA,1,0,NA,NA,0,NA,NA,NA,NA,1)
stopifnot(isTRUE(all.equal(t$proportion,expected,check.attributes=FALSE)),
 t$n_known[16]==1,t$n_trained[16]==1,t$n_dont_know[16]==1,
 all(t$n_conflict[c(12,13,14)]==1),t$n_original_known[15]==1,t$n_known[15]==0)
cat("Passed 16 training-status scenarios and denominator/conflict checks.\n")
