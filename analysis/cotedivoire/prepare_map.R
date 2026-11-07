# Display geometry: CNTIG/OCHA 2018 boundaries with documented 2020 splits.
# No facility or woman is assigned geographically using these polygons.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
boundary<-opt("--boundary");subpref<-opt("--subprefecture-boundary");data_dir<-opt("--data-dir")
for(pkg in c("sf","jsonlite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
here<-dirname(gsub("~+~", " ", sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]), fixed=TRUE))
source(file.path(here,"cdi_definitions.R"))
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"));key<-cdi_key
b<-sf::st_read(boundary,quiet=TRUE);s<-sf::st_read(subpref,quiet=TRUE)
stopifnot(nrow(b)==108,nrow(s)==510)
b<-sf::st_transform(sf::st_make_valid(b),32630);s<-sf::st_transform(sf::st_make_valid(s),32630)
b<-b[c("adm2_name","adm2_pcode")];b$geo<-key(b$adm2_name);b$derived_2020<-FALSE
# Supplied facility naming omits one 'n' in the full Niakaramandougou name.
b$geo[b$geo=="NIAKARAMANDOUGOU"]<-"NIAKARAMADOUGOU"
updates<-read.csv(file.path(here,"boundary_updates.csv"),stringsAsFactors=FALSE)
before_area<-sum(as.numeric(sf::st_area(b)));geometry_checks<-list()
for(new in unique(updates$new_department)){
 u<-updates[updates$new_department==new,];parent<-unique(u$parent_department);stopifnot(length(parent)==1)
 bi<-which(b$geo==key(parent));si<-which(key(s$adm2_name)==key(parent)&key(s$adm3_name)%in%key(u$subprefecture))
 stopifnot(length(bi)==1,length(si)==nrow(u),setequal(key(s$adm3_name[si]),key(u$subprefecture)))
 old<-sf::st_geometry(b)[bi];child<-sf::st_union(sf::st_geometry(s)[si])
 new_geom<-suppressWarnings(sf::st_intersection(old,child))
 old_area<-as.numeric(sf::st_area(old));new_area<-as.numeric(sf::st_area(new_geom))
 sf::st_geometry(b)[bi]<-sf::st_difference(old,new_geom)
 remainder<-as.numeric(sf::st_area(b[bi,]))
 stopifnot(abs(remainder+new_area-old_area)/old_area<1e-8,new_area>0,remainder>0)
 nb<-b[bi,];nb$adm2_name<-new;nb$adm2_pcode<-paste0("DERIVED2020_",key(new));nb$geo<-key(new);nb$derived_2020<-TRUE
 sf::st_geometry(nb)<-new_geom;b<-rbind(b,nb)
 geometry_checks[[new]]<-data.frame(new_department=new,parent_department=parent,
  subprefectures=paste(s$adm3_name[si],collapse="; "),parent_area_before=old_area,
  new_department_area=new_area,parent_area_after=remainder,area_conserved=TRUE,source=unique(u$source))
}
stopifnot(nrow(b)==111,!anyDuplicated(b$geo),abs(sum(as.numeric(sf::st_area(b)))-before_area)/before_area<1e-8)
coverage<-p$coverage[p$coverage$geo!="CDI",]
stopifnot(all(coverage$geo%in%b$geo),sum(coverage$all_source_women)==10856,
 sum(coverage$facilities)==3078)
write.csv(do.call(rbind,geometry_checks),file.path(data_dir,"boundary_update_checks.csv"),row.names=FALSE)
write.csv(data.frame(boundary_name=b$adm2_name,join_geo=b$geo,boundary_pcode=b$adm2_pcode,
 derived_2020=b$derived_2020,has_source_records=b$geo%in%coverage$geo),file.path(data_dir,"map_name_crosswalk.csv"),row.names=FALSE)
original_area<-as.numeric(sf::st_area(b));b<-sf::st_simplify(b,dTolerance=110,preserveTopology=TRUE)
ct<-sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(b)))
important<-c("ABIDJAN","BOUAKE","KORHOGO","YAMOUSSOUKRO","BONON","GOHITAFLA","FACOBLY","TAABO",
 "DAOUKRO","DUEKOUE","OUANGOLODOUGOU","GUEYO")
largest<-b$geo[order(original_area,decreasing=TRUE)[1:27]]
features<-lapply(seq_len(nrow(b)),function(i){g<-sf::st_geometry(b)[[i]]
 rings<-if(inherits(g,"POLYGON"))unclass(g)else unlist(unclass(g),recursive=FALSE)
 list(geo=b$geo[i],name=b$adm2_name[i],rings=lapply(rings,function(z)unname(as.matrix(z))),
  center=unname(ct[i,1:2]),label=b$geo[i]%in%union(important,largest),derived_2020=b$derived_2020[i])})
jsonlite::write_json(list(crs="EPSG:32630",features=features),file.path(data_dir,"map_layout.json"),auto_unbox=TRUE,digits=10)
assets<-file.path(data_dir,"assets");dir.create(assets,showWarnings=FALSE)
invisible(file.copy(boundary,file.path(assets,"civ_departments_2018.geojson"),overwrite=TRUE))
invisible(file.copy(subpref,file.path(assets,"civ_subprefectures_2018.geojson"),overwrite=TRUE))
sf::st_write(sf::st_transform(b,4326),file.path(assets,"civ_display_departments.geojson"),delete_dsn=TRUE,quiet=TRUE)
jsonlite::write_json(list(source="https://data.humdata.org/dataset/cod-ab-civ",source_name="CNTIG / OCHA archived 2018 administrative boundaries",
 department_file_md5=unname(tools::md5sum(boundary)),subprefecture_file_md5=unname(tools::md5sum(subpref)),
 original_departments=108,display_departments=111,legal_updates=unique(updates$source),
 interpretation="Bonon, Gohitafla and Ouelle display polygons reconstructed from named 2018 subprefectures following official 2020 decisions. Parent polygons partitioned with area conservation. Display geometry only; no point-in-polygon facility linkage or later border corrections assumed."),
 file.path(assets,"boundary_metadata.json"),auto_unbox=TRUE,pretty=TRUE)
cat('Verified 111 display departments, all 104 source-covered areas and three area-conserving 2020 updates.\n')
