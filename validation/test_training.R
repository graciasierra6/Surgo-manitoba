# Sindh training-status and denominator tests
#
# Required R packages (restore once: Rscript setup.R):
#   No additional non-bundled packages are used directly.
# Exact package versions: renv.lock. Project dependencies: DESCRIPTION.
# setup.R uses renv to restore the recorded project package environment.
# stats, utils, tools, grid, graphics and grDevices are bundled with R where used.
#
# Test data / definitions:
#   Synthetic Q406 checkbox/text fixtures generated inside this script; no external dataset
#   Training functions in aggregate/sindh/Sindh_analysis.R (code only; report build is not executed)
#
# No additional non-bundled package functions are used directly by these fixtures. Restore the
#   project environment with setup.R for consistent module loading.
# Run: Rscript validation/test_training.R. Checks text/checkbox encodings, explicit unknowns,
#   contradictions, known-status denominators and facility proportions.
# Full filename inventory: docs/DATASET_FILENAMES.md; roles/checksums: docs/INPUTS.md.
# Input locations and credentials are runtime parameters, never saved personal paths.

entry<-sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1])
root<-normalizePath(file.path(dirname(entry),".."),mustWork=TRUE)
module<-new.env(parent=globalenv())
source(file.path(root,"aggregate/sindh/Sindh_analysis.R"),local=module)
module$.report_definitions[["training_reconstruction.R"]](globalenv())
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
