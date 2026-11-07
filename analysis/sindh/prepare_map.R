# Verify a one-to-one named district boundary crosswalk and project for display.
args<-commandArgs(trailingOnly=TRUE)
opt<-function(k){i<-match(k,args);if(is.na(i)||i==length(args))stop("Supply ",k);args[i+1L]}
boundary<-opt("--boundary");data_dir<-opt("--data-dir")
for(pkg in c("sf","jsonlite"))if(!requireNamespace(pkg,quietly=TRUE))stop("Install ",pkg)
p<-jsonlite::fromJSON(file.path(data_dir,"report_data.json"))
b<-sf::st_read(boundary,quiet=TRUE);stopifnot(nrow(b)==30)
key<-function(v)toupper(gsub("[^A-Za-z0-9]","",v))
aliases<-c(KAMBARSHAHDADKOT="QAMBARSHAHDADKOT",CENTRALKARACHI="KARACHICENTRALDISTRICT",EASTKARACHI="KARACHIEAST",
 KORANGIKARACHI="DISTRICTKORANGI",MALIRKARACHI="DISTRICTMALIR",WESTKARACHI="KARACHIWEST",KEAMARI="DISTRICTKEMARI",SOUTHKARACHI="KARACHISOUTH")
b$geo<-key(b$districts);ii<-match(b$geo,names(aliases));b$geo[!is.na(ii)]<-aliases[ii[!is.na(ii)]]
stopifnot(!anyDuplicated(b$geo),setequal(b$geo,p$profiles$geo))
b$name<-p$profiles$district[match(b$geo,p$profiles$geo)]
write.csv(data.frame(boundary_name=b$districts,join_geo=b$geo,join_name=b$name,exact_normalized_name=key(b$districts)==b$geo),
 file.path(data_dir,"map_name_crosswalk.csv"),row.names=FALSE)
utm<-sf::st_transform(sf::st_make_valid(b),32642)
ct<-sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(utm)))
features<-lapply(seq_len(nrow(utm)),function(i){g<-sf::st_geometry(utm)[[i]]
 rings<-if(inherits(g,"POLYGON"))g else unlist(g,recursive=FALSE)
 list(geo=utm$geo[i],name=utm$name[i],rings=unname(rings),center=unname(ct[i,1:2]),
  karachi=grepl("KARACHI|MALIR|KORANGI|KEMARI",utm$geo[i]))})
jsonlite::write_json(list(crs="EPSG:32642",features=features),file.path(data_dir,"map_layout.json"),
 auto_unbox=TRUE,digits=10)
cat('Verified 30 district polygons and explicit name aliases; no supply assignments changed.\n')
