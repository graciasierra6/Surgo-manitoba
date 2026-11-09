# Department boundaries are for display; facility assignments use the audited lookup.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
boundary<-opt("--boundary");data_dir<-opt("--data-dir")
for(pkg in c("sf","jsonlite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
here<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
source(file.path(here,"..","..","..","analysis","senegal","senegal_definitions.R"))
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"))
b<-sf::st_read(boundary,quiet=TRUE);stopifnot(nrow(b)==46)
b$geo<-senegal_key(b$adm2_name)
aliases<-c(NIORODURIP="NIORO",MEDINAYOROFOULA="MEDINAYOROFOULAH",RANEROU="RANEROUFERLO")
ii<-match(b$geo,names(aliases));b$geo[!is.na(ii)]<-aliases[ii[!is.na(ii)]]
coverage<-p$coverage[p$coverage$geo!="SENEGAL",]
stopifnot(!anyDuplicated(b$geo),setequal(b$geo,coverage$geo),length(intersect(b$geo,p$profiles$geo))==33)
write.csv(data.frame(boundary_name=b$adm2_name,join_geo=b$geo,join_name=coverage$name[match(b$geo,coverage$geo)],
 boundary_pcode=b$adm2_pcode,exact_normalized_name=senegal_key(b$adm2_name)==b$geo),
 file.path(data_dir,"map_name_crosswalk.csv"),row.names=FALSE)
utm<-sf::st_transform(sf::st_make_valid(b),32628)
utm<-sf::st_simplify(utm,dTolerance=120,preserveTopology=TRUE)
ct<-sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(utm)))
features<-lapply(seq_len(nrow(utm)),function(i){g<-sf::st_geometry(utm)[[i]]
 rings<-if(inherits(g,"POLYGON"))unclass(g) else unlist(unclass(g),recursive=FALSE)
 rings<-lapply(rings,function(z)unname(as.matrix(z)))
 list(geo=utm$geo[i],name=utm$adm2_name[i],rings=unname(rings),center=unname(ct[i,1:2]),
  inset=utm$geo[i]%in%c("DAKAR","GUEDIAWAYE","PIKINE","KEURMASSAR","RUFISQUE"))})
jsonlite::write_json(list(crs="EPSG:32628",features=features),file.path(data_dir,"map_layout.json"),auto_unbox=TRUE,digits=10)
dir.create(file.path(data_dir,"assets"),showWarnings=FALSE)
file.copy(boundary,file.path(data_dir,"assets/senegal_departments.geojson"),overwrite=TRUE)
jsonlite::write_json(list(source="https://data.humdata.org/dataset/cod-ab-sen",source_name="Government of Senegal / OCHA administrative boundaries",
 snapshot_md5=unname(tools::md5sum(boundary)),departments=46,display_simplification_metres=120,
 qualification="Boundary snapshot is display geography; no facility point-in-polygon assignment or GPS inference."),
 file.path(data_dir,"assets/boundary_metadata.json"),auto_unbox=TRUE,pretty=TRUE)
cat('Verified all 46 department polygons: 33 both-source and 13 facility-only departments.\n')
