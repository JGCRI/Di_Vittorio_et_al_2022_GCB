# proc_moirai_land_distribution.r

# determine the GCAM land distribution
# available land includes only known types
# GCAM style: percent of known non- crop, pasture, urban
# total land style: percent of total land, including unknown

# assume that this script is in the moirai diagnostics directory and that this is the working directory

# arguments:

#	    land_area_fname: the area data (path is indir below); moirai output
#		land_type_map_fname: land type mapping for data (path is indir below); moirai output
#		ctry_glu_map_fname: country and glu mapping for data (path is indir below); moirai output
#		ctry2gcam_reg_fname: country to gcam region mapping (path is indir below); moirai input
#		gcam_reg_fname: moirai input file containing the gcam region names (path is indir below); moirai input
#		gcam_reg_glu_rast_fname: raster of valid region and glu pixels; a spatial diag output
#		gcam_region_shape_lname: name of valid region shapefile without extension; a spaital diag output
#		country_shape_lname: name of valid country shapefile without extension; a spaital diag output
#		gcam_diag_area_name: name of complete initial gcam land area output (path is indir below); gcam output
#		gcam_glu_name_map: name of mapping between full glu names and gcam abbreviated glu names (path is indir below); diagnostic input
#		rast_dir: path to gcam_reg_glu_rast_fname
#		shape_dir: path to gcam_region_shape_lname and country_shape_lname

#		indir: path to the moirai files that are input to this function
#		outdir: path to the directory for the outputs from this function
#		year: the year of data to process

# outputs:

# notes:
#
#	unknown land types are excluded from gcam
#	unknown status is considererd unsuitable and protected during pre-rpocessing - there should not be any unknown status here
#		there are not any unknown status - i checked

# make sure that this scripts directory is the working directory
setwd("./")

library(raster)
library(rasterVis)
library(rgdal)
library(RColorBrewer)
library(sp)

land_area_fname = "Land_type_area_ha_all.csv"
land_type_map_fname = "MOIRAI_land_types.csv"
ctry_glu_map_fname = "MOIRAI_ctry_GLU.csv"
ctry2gcam_reg_fname = "iso_GCAM_regID_32reg.csv"
gcam_reg_fname = "GCAM_region_names_32reg.csv"
gcam_reg_glu_rast_fname = "gcam_reg_glu_boundaries_moirai_land_cells_3p1_0p5arcmin.tif"
gcam_region_shape_lname = "region_boundaries_moirai_combined_3p1_0p5arcmin"
country_shape_lname = "country_boundaries_moirai_combined_3p1_0p5arcmin"
gcam_diag_area_name = "Detailed_land_allocation_2015_june2022.csv"
gcam_glu_name_map = "glu_to_gcamglu_mapping_c2021.csv"
rast_dir = "../other_data/raster_files"
shape_dir = "../other_data/gcam_boundaries_moirai_3p1_0p5arcmin_wgs84"
indir = "../other_data"
outdir = "../outputs/distribution_threshold"
year = 2015

proc_moirai_land_distribution <- function(	land_area_fname = "Land_type_area_ha_all.csv",
											land_type_map_fname = "MOIRAI_land_types.csv",
											ctry_glu_map_fname = "MOIRAI_ctry_GLU.csv",
											ctry2gcam_reg_fname = "iso_GCAM_regID_32reg.csv",
											gcam_reg_fname = "GCAM_region_names_32reg.csv",
											gcam_reg_glu_rast_fname = "gcam_reg_glu_boundaries_moirai_land_cells_3p1_0p5arcmin.tif",
											gcam_region_shape_lname = "region_boundaries_moirai_combined_3p1_0p5arcmin",
											country_shape_lname = "country_boundaries_moirai_combined_3p1_0p5arcmin",
											gcam_diag_area_name = "Detailed_land_allocation_2015_june2022.csv",
											gcam_glu_name_map = "glu_to_gcamglu_mapping_c2021.csv",
											rast_dir = "../other_data/raster_files",
											shape_dir = "../other_data/gcam_boundaries_moirai_3p1_0p5arcmin_wgs84",
											indir = "../other_data",
											outdir = "../outputs/distribution_threshold",
											year = 2015) {
	
	cat("start proc_moirai_land_distribution at", date(), "\n")
	
	# ensure that the paths end with "/"
	# except for the shape dir which cannot have the / for read OGR
  	if(substr(indir,nchar(indir), nchar(indir)) != "/") { indir = paste0(indir, "/") }
  	if(substr(outdir,nchar(outdir), nchar(outdir)) != "/") { outdir = paste0(outdir, "/") }
  	if(substr(rast_dir,nchar(rast_dir), nchar(rast_dir)) != "/") { rast_dir = paste0(rast_dir, "/") }
  	dir.create(outdir, recursive=TRUE)
  	
  	# make sure the shape dir does not end with "/"
	if(substr(shape_dir,nchar(shape_dir), nchar(shape_dir)) == "/") { shape_dir = substr(shape_dir,1, nchar(shape_dir)-1 ) }
	
	HA2KMSQ = 1 / 100
	HA2THOUSKMSQ = 1 / 100000
	KMSQ2HA = 100
	THOUSKMSQ2HA = 100000
	
	PROJ4_STRING = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

	# need only one Protection_constraint from gcam because at initial year 2015 the managed forest and pasture are the same across constraints
	# so the fractions will be calculated based on the total land type area to avoid differences in protected status
	# this can be done because initial managed area is considered non-convertible
	# take it out of which status? suitable and unprotected? unsuitable and unprotected?
	constraint = "Low Availability"
	
	# need only forest, grass and pasture from gcam; these are the first part of the landleaf names, the second part is the abbrev. glu
	# note that Forest is managed forest, and pasture is grazed pasture
	gcam_forest_names = c("Forest", "ProtectedUnmanagedForest", "UnmanagedForest")
	gcam_pasture_names = c("Pasture", "UnmanagedPasture", "ProtectedUnmanagedPasture")
	gcam_grassland_names = c("Grassland", "ProtectedGrassland")
	gcam_crop_names = c("biomassGrass", "RootTuber", "Rice", "OilCrop", "PalmFruit", "OtherGrain", "Wheat", "Corn",
		"biomassTree", "MiscCrop", "FiberCrop", "SugarCrop", "FodderHerb", "FodderGrass")
	gcam_other_arable_name = "OtherArableLand"		# this is considered a managed cropland type
	gcam_other_names = c("RockIceDesert", "Tundra")
	gcam_shrubland_names = c("Shrubland", "ProtectedShrubland")
	gcam_urban_name = "UrbanLand"
	
	
	# read in the files
	data_in = read.csv(paste0(indir,land_area_fname), header=TRUE, skip=5, stringsAsFactors=FALSE)
	land_types = read.csv(paste0(indir,land_type_map_fname), header=TRUE, skip=4, stringsAsFactors=FALSE)
	ctry_glus = read.csv(paste0(indir,ctry_glu_map_fname), header=TRUE, skip=4, stringsAsFactors=FALSE)
	ctry2gcam_reg = read.csv(paste0(indir, ctry2gcam_reg_fname), header=TRUE, skip=3, stringsAsFactors=FALSE)
	gcam_reg = read.csv(paste0(indir, gcam_reg_fname), header=TRUE, skip=3, stringsAsFactors=FALSE)
	glu_name_map = read.csv(paste0(indir, gcam_glu_name_map), header=TRUE, skip=7, stringsAsFactors=FALSE)
	gcam_init_area = read.csv(paste0(indir, gcam_diag_area_name), header=TRUE, skip=0, stringsAsFactors=FALSE)
	
	# subset data_in to desired year
	data_in = data_in[data_in$year == year,]
	
	# join the dfs together to align the data with the info
	data_in = merge(data_in, land_types, by.x = "land_type", by.y = "Category", all.x = TRUE, sort = FALSE)
	data_in = merge(data_in, ctry_glus, by.x = c("iso", "glu_code"), by.y = c("iso", "glu"), all.x = TRUE, sort = FALSE)
	data_in = merge(data_in, ctry2gcam_reg[, c("iso", "GCAM_region_ID")], by.x = c("iso"), by.y = c("iso"), all.x = TRUE, sort = FALSE)
	data_in = merge(data_in, gcam_reg, by.x = c("GCAM_region_ID"), by.y = c("GCAM_region_ID"), all.x = TRUE, sort = FALSE)
		
	##### need the gcam determinations of managed forest and pasture and grass to calculate the correct convertible land
	# this is because gcam convertible land consists of unmanaged forest, shrubland, and grassland
	#	where grassland is technically grass+unused pasture
	#	gcam uses very little (~1/15) of the input pasture area as grazed pasture - calc this
	# since the total forest and total grass/pasture do not match exactly between moirai and gcam outputs,
	#	calculate a fraction of the total that is managed in the gcam output, by gcam region and glu, and apply it to these moirai data
	# also note that unmanaged land with unknown land type is discarded by gcam

	##### the moirai data are scaled and split and merged so that they should be mutually exclusive
	# add records for a new land use type (LT_HYDE label == Adj_Unmanaged) that is adjusted for managed forest and unused pasture
	# add a new pasture record called Pasture_Grazed (type) Grassland (cover)
	# add Pasture_Ungrazed Grassland for tracking, but this will be added to Grassland in Adj_unmanaged
	# add a new managed forest record called Harvested_Forest (type) Forest (cover)
	
	# subset the gcam init area
	# the in units are thousand km^2, so convert them to ha for now
	gcam_init_area = gcam_init_area[gcam_init_area$Protection_constraint==constraint & gcam_init_area$year == year, c("region", "landleaf", "value","Units")]
	gcam_init_area$lt_gcam_area_ha = gcam_init_area$value * THOUSKMSQ2HA
	gcam_init_area$value = NULL
	gcam_init_area$Units = NULL
	
	# separate the land type and basin names and determine the glu code
	gcam_init_area$LT_GCAM = sapply(strsplit(gcam_init_area$landleaf,"_"),"[[",1)
	gcam_init_area$gcam_glu_name = sapply(strsplit(gcam_init_area$landleaf,"_"),"[[",2)
	gcam_init_area$water[gcam_init_area$LT_GCAM %in% gcam_crop_names] = 
		sapply(strsplit(gcam_init_area$landleaf[gcam_init_area$LT_GCAM %in% gcam_crop_names],"_"),"[[",3)
	gcam_init_area$fert[gcam_init_area$LT_GCAM %in% gcam_crop_names] =
		sapply(strsplit(gcam_init_area$landleaf[gcam_init_area$LT_GCAM %in% gcam_crop_names],"_"),"[[",4)
	gcam_init_area$landleaf = NULL
	gcam_init_area = merge(gcam_init_area, glu_name_map[,c("GCAM_basin_ID", "GLU_name")], by.x = c("gcam_glu_name"), by.y = c("GLU_name"), all.x = TRUE, sort = FALSE)
	
	# calculate the managed forest fraction of all forest
	gcam_forest = gcam_init_area[gcam_init_area$LT_GCAM %in% gcam_forest_names,]
	temp = aggregate(gcam_forest$lt_gcam_area_ha, by = list(gcam_forest$region, gcam_forest$GCAM_basin_ID), FUN = sum)
	colnames(temp) = c("region", "GCAM_basin_ID", "gcam_cover_area_ha")
	temp$gcam_cover = "gcam_forest"
	temp = temp[,c("region", "GCAM_basin_ID", "gcam_cover", "gcam_cover_area_ha")]
	gcam_forest = merge(gcam_forest, temp, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE, sort = FALSE)
	gcam_forest$lt_gcam_frac = gcam_forest$lt_gcam_area_ha / gcam_forest$gcam_cover_area_ha
	### need to select at least one for each regionXglu, if it exists
	# need the Forest record (managed)
	gcam_forest_sub = gcam_forest[gcam_forest$LT_GCAM == "Forest",]
	# if Forest doesn't exist, create a Forest record with zero leaf area and frac
	land_units = unique(gcam_forest[c("region", "GCAM_basin_ID")])
	if (nrow(gcam_forest_sub) == 0 | nrow(gcam_forest_sub) != nrow(land_units)) {
		temp = merge(land_units, gcam_forest_sub, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE)
	
		get_inds = which(is.na(temp$gcam_glu_name))
		for ( i in 1:length(get_inds)) {
			temp2 = gcam_forest[gcam_forest$region == temp$region[get_inds[i]] & gcam_forest$GCAM_basin_ID == temp$GCAM_basin_ID[get_inds[i]],]
			temp2[1,"lt_gcam_area_ha"] = 0.0
			temp2[1,"lt_gcam_frac"] = 0.0
			temp2[1,"LT_GCAM"] = "Forest"
			gcam_forest_sub = rbind(gcam_forest_sub, temp2[1,])
		}
	} # end if missing Forest record for present regionXglu
	# replace NaN an Inf with 0
	gcam_forest_sub[is.na(gcam_forest_sub)] = 0
	gcam_forest_sub[gcam_forest_sub == Inf] = 0
	
	# aggregate to gcam region for regional analysis
	gcam_forest_sub_reg = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ region + LT_GCAM + gcam_cover, data = gcam_forest_sub, FUN = sum)	
	
	# aggregate to globe and then add the global row
	temp_globe = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ LT_GCAM + gcam_cover, data = gcam_forest_sub_reg, FUN = sum)
	region = rep("Global", nrow(temp_globe))
	temp_globe = cbind(region, temp_globe, stringsAsFactors=FALSE)
	gcam_forest_sub_reg = rbind(gcam_forest_sub_reg, temp_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	# now recalculate the fraction
	gcam_forest_sub_reg$lt_gcam_frac = gcam_forest_sub_reg$lt_gcam_area_ha / gcam_forest_sub_reg$gcam_cover_area_ha
	gcam_forest_sub_reg[is.na(gcam_forest_sub_reg)] = 0
	gcam_forest_sub_reg[gcam_forest_sub_reg == Inf] = 0
	
	
	
	# calculate the grazed pasture fraction of all grassland and pasture
	gcam_pasture = gcam_init_area[gcam_init_area$LT_GCAM %in% gcam_pasture_names | gcam_init_area$LT_GCAM %in% gcam_grassland_names,]
	temp = aggregate(gcam_pasture$lt_gcam_area_ha, by = list(gcam_pasture$region, gcam_pasture$GCAM_basin_ID), FUN = sum)
	colnames(temp) = c("region", "GCAM_basin_ID", "gcam_cover_area_ha")
	temp$gcam_cover = "gcam_grass"
	temp = temp[,c("region", "GCAM_basin_ID", "gcam_cover", "gcam_cover_area_ha")]
	gcam_pasture = merge(gcam_pasture, temp, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE, sort = FALSE)
	gcam_pasture$lt_gcam_frac = gcam_pasture$lt_gcam_area_ha / gcam_pasture$gcam_cover_area_ha
	### need to select at least one for each regionXglu, if it exists
	# need the Pasture record (managed)
	gcam_pasture_sub = gcam_pasture[gcam_pasture$LT_GCAM == "Pasture",]
	# if Pasture doesn't exist, create a pasture record with zero leaf area and frac
	land_units = unique(gcam_pasture[c("region", "GCAM_basin_ID")])
	if (nrow(gcam_pasture_sub) == 0 | nrow(gcam_pasture_sub) != nrow(land_units)) {
		temp = merge(land_units, gcam_pasture_sub, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE)
	
		get_inds = which(is.na(temp$gcam_glu_name))
		for ( i in 1:length(get_inds)) {
			temp2 = gcam_pasture[gcam_pasture$region == temp$region[get_inds[i]] & gcam_pasture$GCAM_basin_ID == temp$GCAM_basin_ID[get_inds[i]],]
			temp2[1,"lt_gcam_area_ha"] = 0.0
			temp2[1,"lt_gcam_frac"] = 0.0
			temp2[1,"LT_GCAM"] = "Pasture"
			gcam_pasture_sub = rbind(gcam_pasture_sub, temp2[1,])
		}
	} # end if missing Forest record for present regionXglu
	# replace NaN an Inf with 0
	gcam_pasture_sub[is.na(gcam_pasture_sub)] = 0
	gcam_pasture_sub[gcam_pasture_sub == Inf] = 0
	
	# aggregate to gcam region for regional analysis
	gcam_pasture_sub_reg = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ region + LT_GCAM + gcam_cover, data = gcam_pasture_sub, FUN = sum)	
	
	# aggregate to globe and then add the global row
	temp_globe = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ LT_GCAM + gcam_cover, data = gcam_pasture_sub_reg, FUN = sum)
	region = rep("Global", nrow(temp_globe))
	temp_globe = cbind(region, temp_globe, stringsAsFactors=FALSE)
	gcam_pasture_sub_reg = rbind(gcam_pasture_sub_reg, temp_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	# now recalculate the fraction
	gcam_pasture_sub_reg$lt_gcam_frac = gcam_pasture_sub_reg$lt_gcam_area_ha / gcam_pasture_sub_reg$gcam_cover_area_ha
	gcam_pasture_sub_reg[is.na(gcam_pasture_sub_reg)] = 0
	gcam_pasture_sub_reg[gcam_pasture_sub_reg == Inf] = 0
	
	
	
	# calculate the total gcam cropland including other arable, and the fraction that is other arable
	# other arable is considered managed cropland in gcam
	gcam_crop = gcam_init_area[gcam_init_area$LT_GCAM %in% gcam_crop_names | gcam_init_area$LT_GCAM %in% gcam_other_arable_name,]
	temp = aggregate(gcam_crop$lt_gcam_area_ha, by = list(gcam_crop$region, gcam_crop$GCAM_basin_ID), FUN = sum)
	colnames(temp) = c("region", "GCAM_basin_ID", "gcam_cover_area_ha")
	temp$gcam_cover = "gcam_crop"
	temp = temp[,c("region", "GCAM_basin_ID", "gcam_cover", "gcam_cover_area_ha")]
	gcam_crop = merge(gcam_crop, temp, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE, sort = FALSE)
	gcam_crop$lt_gcam_frac = gcam_crop$lt_gcam_area_ha / gcam_crop$gcam_cover_area_ha
	### need to select at least one for each regionXglu, if it exists
	# want the OtherArableLand record record (managed, but in this case unused for crops)
	gcam_crop_sub = gcam_crop[gcam_crop$LT_GCAM == gcam_other_arable_name,]
	# if OtherArableLand doesn't exist, create a crop record with zero leaf area and frac
	land_units = unique(gcam_crop[c("region", "GCAM_basin_ID")])
	if (nrow(gcam_crop_sub) == 0 | nrow(gcam_crop_sub) != nrow(land_units)) {
		temp = merge(land_units, gcam_crop_sub, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE)
	
		get_inds = which(is.na(temp$gcam_glu_name))
		for ( i in 1:length(get_inds)) {
			temp2 = gcam_crop[gcam_crop$region == temp$region[get_inds[i]] & gcam_crop$GCAM_basin_ID == temp$GCAM_basin_ID[get_inds[i]],]
			temp2[1,"lt_gcam_area_ha"] = 0.0
			temp2[1,"lt_gcam_frac"] = 0.0
			temp2[1,"LT_GCAM"] = gcam_other_arable_name
			temp2[1,"water"] = 0
			temp2[1,"fert"] = 0
			gcam_crop_sub = rbind(gcam_crop_sub, temp2[1,])
		}
	} # end if missing Forest record for present regionXglu
	# replace NaN an Inf with 0
	gcam_crop_sub[is.na(gcam_crop_sub)] = 0
	gcam_crop_sub[gcam_crop_sub == Inf] = 0
	
	# aggregate to gcam region for regional analysis
	gcam_crop_sub_reg = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ region + LT_GCAM + gcam_cover, data = gcam_crop_sub, FUN = sum)	
	
	# aggregate to globe and then add the global row
	temp_globe = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ LT_GCAM + gcam_cover, data = gcam_crop_sub_reg, FUN = sum)
	region = rep("Global", nrow(temp_globe))
	temp_globe = cbind(region, temp_globe, stringsAsFactors=FALSE)
	gcam_crop_sub_reg = rbind(gcam_crop_sub_reg, temp_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	# now recalculate the fraction
	gcam_crop_sub_reg$lt_gcam_frac = gcam_crop_sub_reg$lt_gcam_area_ha / gcam_crop_sub_reg$gcam_cover_area_ha
	gcam_crop_sub_reg[is.na(gcam_crop_sub_reg)] = 0
	gcam_crop_sub_reg[gcam_crop_sub_reg == Inf] = 0
	
	
	
	# calculate the total gcam shrubland
	gcam_shrub = gcam_init_area[gcam_init_area$LT_GCAM %in% gcam_shrubland_names,]
	temp = aggregate(gcam_shrub$lt_gcam_area_ha, by = list(gcam_shrub$region, gcam_shrub$GCAM_basin_ID), FUN = sum)
	colnames(temp) = c("region", "GCAM_basin_ID", "gcam_cover_area_ha")
	temp$gcam_cover = "gcam_shrub"
	temp = temp[,c("region", "GCAM_basin_ID", "gcam_cover", "gcam_cover_area_ha")]
	gcam_shrub = merge(gcam_shrub, temp, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE, sort = FALSE)
	gcam_shrub$lt_gcam_frac = gcam_shrub$lt_gcam_area_ha / gcam_shrub$gcam_cover_area_ha
	### need to select at least one for each regionXglu, if it exists
	# want the Shrubland record (unprotected, unmanaged)
	gcam_shrub_sub = gcam_shrub[gcam_shrub$LT_GCAM == "Shrubland",]
	# if Shrubland doesn't exist, create a shrub record with zero leaf area and frac
	land_units = unique(gcam_shrub[c("region", "GCAM_basin_ID")])
	if (nrow(gcam_shrub_sub) == 0 | nrow(gcam_shrub_sub) != nrow(land_units)) {
		temp = merge(land_units, gcam_shrub_sub, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE)
	
		get_inds = which(is.na(temp$gcam_glu_name))
		for ( i in 1:length(get_inds)) {
			temp2 = gcam_shrub[gcam_shrub$region == temp$region[get_inds[i]] & gcam_shrub$GCAM_basin_ID == temp$GCAM_basin_ID[get_inds[i]],]
			temp2[1,"lt_gcam_area_ha"] = 0.0
			temp2[1,"lt_gcam_frac"] = 0.0
			temp2[1,"LT_GCAM"] = "Shrubland"
			gcam_shrub_sub = rbind(gcam_shrub_sub, temp2[1,])
		}
	} # end if missing Forest record for present regionXglu
	# replace NaN an Inf with 0
	gcam_shrub_sub[is.na(gcam_shrub_sub)] = 0
	gcam_shrub_sub[gcam_shrub_sub == Inf] = 0
	
	# aggregate to gcam region for regional analysis
	gcam_shrub_sub_reg = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ region + LT_GCAM + gcam_cover, data = gcam_shrub_sub, FUN = sum)	
	
	# aggregate to globe and then add the global row
	temp_globe = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ LT_GCAM + gcam_cover, data = gcam_shrub_sub_reg, FUN = sum)
	region = rep("Global", nrow(temp_globe))
	temp_globe = cbind(region, temp_globe, stringsAsFactors=FALSE)
	gcam_shrub_sub_reg = rbind(gcam_shrub_sub_reg, temp_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	# now recalculate the fraction
	gcam_shrub_sub_reg$lt_gcam_frac = gcam_shrub_sub_reg$lt_gcam_area_ha / gcam_shrub_sub_reg$gcam_cover_area_ha
	gcam_shrub_sub_reg[is.na(gcam_shrub_sub_reg)] = 0
	gcam_shrub_sub_reg[gcam_shrub_sub_reg == Inf] = 0
	
	
	
	# calculate the total gcam other
	gcam_other = gcam_init_area[gcam_init_area$LT_GCAM %in% gcam_other_names,]
	temp = aggregate(gcam_other$lt_gcam_area_ha, by = list(gcam_other$region, gcam_other$GCAM_basin_ID), FUN = sum)
	colnames(temp) = c("region", "GCAM_basin_ID", "gcam_cover_area_ha")
	temp$gcam_cover = "gcam_other"
	temp = temp[,c("region", "GCAM_basin_ID", "gcam_cover", "gcam_cover_area_ha")]
	gcam_other = merge(gcam_other, temp, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE, sort = FALSE)
	gcam_other$lt_gcam_frac = gcam_other$lt_gcam_area_ha / gcam_other$gcam_cover_area_ha
	### need to select at least one for each regionXglu, if it exists
	# want the RockIceDesert record (unmanaged, also constant; should minimize the loop length below)
	gcam_other_sub = gcam_other[gcam_other$LT_GCAM == "RockIceDesert",]
	# if RockIceDesert doesn't exist, create a other record with zero leaf area and frac
	land_units = unique(gcam_other[c("region", "GCAM_basin_ID")])
	if (nrow(gcam_other_sub) == 0 | nrow(gcam_other_sub) != nrow(land_units)) {
		temp = merge(land_units, gcam_other_sub, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE)
	
		get_inds = which(is.na(temp$gcam_glu_name))
		for ( i in 1:length(get_inds)) {
			temp2 = gcam_other[gcam_other$region == temp$region[get_inds[i]] & gcam_other$GCAM_basin_ID == temp$GCAM_basin_ID[get_inds[i]],]
			temp2[1,"lt_gcam_area_ha"] = 0.0
			temp2[1,"lt_gcam_frac"] = 0.0
			temp2[1,"LT_GCAM"] = "RockIceDesert"
			gcam_other_sub = rbind(gcam_other_sub, temp2[1,])
		}
	} # end if missing Forest record for present regionXglu
	# replace NaN an Inf with 0
	gcam_other_sub[is.na(gcam_other_sub)] = 0
	gcam_other_sub[gcam_other_sub == Inf] = 0
	
	# aggregate to gcam region for regional analysis
	gcam_other_sub_reg = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ region + LT_GCAM + gcam_cover, data = gcam_other_sub, FUN = sum)	
	
	# aggregate to globe and then add the global row
	temp_globe = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ LT_GCAM + gcam_cover, data = gcam_other_sub_reg, FUN = sum)
	region = rep("Global", nrow(temp_globe))
	temp_globe = cbind(region, temp_globe, stringsAsFactors=FALSE)
	gcam_other_sub_reg = rbind(gcam_other_sub_reg, temp_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	# now recalculate the fraction
	gcam_other_sub_reg$lt_gcam_frac = gcam_other_sub_reg$lt_gcam_area_ha / gcam_other_sub_reg$gcam_cover_area_ha
	gcam_other_sub_reg[is.na(gcam_other_sub_reg)] = 0
	gcam_other_sub_reg[gcam_other_sub_reg == Inf] = 0
	
	
	
	# calculate the total gcam urban
	gcam_urban = gcam_init_area[gcam_init_area$LT_GCAM %in% gcam_urban_name,]
	temp = aggregate(gcam_urban$lt_gcam_area_ha, by = list(gcam_urban$region, gcam_urban$GCAM_basin_ID), FUN = sum)
	colnames(temp) = c("region", "GCAM_basin_ID", "gcam_cover_area_ha")
	temp$gcam_cover = "gcam_urban"
	temp = temp[,c("region", "GCAM_basin_ID", "gcam_cover", "gcam_cover_area_ha")]
	gcam_urban = merge(gcam_urban, temp, by.x = c("region", "GCAM_basin_ID"), by.y = c("region", "GCAM_basin_ID"), all.x = TRUE, sort = FALSE)
	# want the UrbanLand record (the only type here, and constant)
	# don't need to create an UrbanLand record if no urban exists; also no need for a fraction here

	# aggregate to gcam region for regional analysis
	gcam_urban_reg = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ region + LT_GCAM + gcam_cover, data = gcam_urban, FUN = sum)	
	
	# aggregate to globe and then add the global row
	temp_globe = aggregate(cbind(lt_gcam_area_ha, gcam_cover_area_ha) ~ LT_GCAM + gcam_cover, data = gcam_urban_reg, FUN = sum)
	region = rep("Global", nrow(temp_globe))
	temp_globe = cbind(region, temp_globe, stringsAsFactors=FALSE)
	gcam_urban_reg = rbind(gcam_urban_reg, temp_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	
	# check the global area
	cat("Global in landleaf area is", sum(gcam_init_area$lt_gcam_area_ha), "ha\n")
	
	global_ha_sum = gcam_forest_sub_reg$gcam_cover_area_ha[gcam_forest_sub_reg$region=="Global"] +
		gcam_pasture_sub_reg$gcam_cover_area_ha[gcam_pasture_sub_reg$region=="Global"] +
		gcam_crop_sub_reg$gcam_cover_area_ha[gcam_crop_sub_reg$region=="Global"] +
		gcam_shrub_sub_reg$gcam_cover_area_ha[gcam_shrub_sub_reg$region=="Global"] +
		gcam_other_sub_reg$gcam_cover_area_ha[gcam_other_sub_reg$region=="Global"] +
		gcam_urban_reg$gcam_cover_area_ha[gcam_urban_reg$region=="Global"]
	cat("Global sum regional land area is", sum(gcam_init_area$lt_gcam_area_ha), "ha\n")
	
	###### aggregate the moirai in data to match gcam
	
	# now aggregate the data_in by gcam region
	# keep the original df in case also want to do this by country or by glu
	data_reg = aggregate(data_in$value, by = list(data_in$region, data_in$land_type, data_in$LT_SAGE, data_in$LT_HYDE, data_in$Status), FUN = sum)
	colnames(data_reg) = c("region", "land_type", "LT_SAGE", "LT_HYDE", "Status", "area_ha")
	
	# aggregate to globe and then add the global rows
	data_globe = aggregate(data_in$value, by = list(data_in$land_type, data_in$LT_SAGE, data_in$LT_HYDE, data_in$Status), FUN = sum)
	colnames(data_globe) = c("land_type", "LT_SAGE", "LT_HYDE", "Status", "area_ha")
	region = rep("Global", nrow(data_globe))
	data_globe = cbind(region, data_globe, stringsAsFactors=FALSE)
	data_reg = rbind(data_reg, data_globe, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	# get total land sums for general comparison and add as a new column
	total_land = aggregate(data_reg$area_ha, by = list(data_reg$region), FUN = sum)
	colnames(total_land) = c("region", "total_land_ha")
	data_reg = merge(data_reg, total_land, by.x = c("region"), by.y = c("region"), all.x = TRUE, sort = FALSE)
	 
	# do not exclude unknown land cover types - will want these later
	#data_reg_sub = data_reg[data_reg$LT_SAGE != "Unknown",]
	data_reg_sub = data_reg
	
	# now aggregate land cover to forest, grassland, shrubland, and other (tundra, desert, polar)
	# these "other" three are grouped as other because they do not change in gcam
	# track the unknown sage land covers but don't use them in main calcs because gcam ignores them
	# will fold other and unknown back in later for general comparison

	# forest
	temp = data_reg_sub[data_reg_sub$land_type >= 100 & data_reg_sub$land_type < 900,]
	forest_sums = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE, temp$Status), FUN = sum)
	colnames(forest_sums) = c("region", "LT_HYDE", "Status", "area_ha")
	forest_sums$cover = "Forest"
	forest_sums = forest_sums[,c("region", "cover", "LT_HYDE", "Status", "area_ha")]

	# savanna is mapped to grassland in gcam
	temp = data_reg_sub[data_reg_sub$land_type >= 900 & data_reg_sub$land_type < 1100,]
	grass_sums = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE, temp$Status), FUN = sum)
	colnames(grass_sums) = c("region", "LT_HYDE", "Status", "area_ha")
	grass_sums$cover = "Grassland"
	grass_sums = grass_sums[,c("region", "cover", "LT_HYDE", "Status", "area_ha")]

	# shrub
	temp = data_reg_sub[data_reg_sub$land_type >= 1100 & data_reg_sub$land_type < 1300,]
	shrub_sums = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE, temp$Status), FUN = sum)
	colnames(shrub_sums) = c("region", "LT_HYDE", "Status", "area_ha")
	shrub_sums$cover = "Shrubland"
	shrub_sums = shrub_sums[,c("region", "cover", "LT_HYDE", "Status", "area_ha")]

	# other - tundra, rock, ice, desert
	temp = data_reg_sub[data_reg_sub$land_type >= 1300 & data_reg_sub$land_type < 1600,]
	other_sums = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE, temp$Status), FUN = sum)
	colnames(other_sums) = c("region", "LT_HYDE", "Status", "area_ha")
	other_sums$cover = "Other"
	other_sums = other_sums[,c("region", "cover", "LT_HYDE", "Status", "area_ha")]
	
	# unknown
	temp = data_reg_sub[data_reg_sub$land_type < 100,]
	unknown_sums = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE, temp$Status), FUN = sum)
	colnames(unknown_sums) = c("region", "LT_HYDE", "Status", "area_ha")
	unknown_sums$cover = "Unknown"
	unknown_sums = unknown_sums[,c("region", "cover", "LT_HYDE", "Status", "area_ha")]

	data_reg_gcam = rbind(forest_sums, grass_sums, shrub_sums, other_sums, unknown_sums, stringsAsFactors=FALSE, make.row.names = FALSE)
	
	# get general land category areas regardless of status, as status groups will be added as columns
	data_out = aggregate(data_reg_gcam$area_ha, by = list(data_reg_gcam$region, data_reg_gcam$cover, data_reg_gcam$LT_HYDE), FUN = sum)
	colnames(data_out) = c("region", "cover", "LT_HYDE", "area_ha")
	
	# merge total land ha back into data frame
	data_out = merge(data_out, unique(data_reg_sub[,c("region", "total_land_ha")]), by = "region", all.x= TRUE, sort= FALSE)
	
	# get land use sums and the convertible land (convertible does not include other and unknown)
	# land use is the total of all covers for each LT_HYDE land use
		# this is to make comparisons with total land type values
	lu_sums = aggregate(data_reg_gcam$area_ha, by = list(data_reg_gcam$region, data_reg_gcam$LT_HYDE), FUN = sum)
	colnames(lu_sums) = c("region", "LT_HYDE", "land_use_ha")
	data_out = merge(data_out, lu_sums, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# gcam convertible land is the unmanaged land that does not include the other and unknown categories
	# this is to make direct comparisons with fixed restrictions that apply the same percentages across land types
	temp = data_reg_gcam[data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	conv_sums = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(conv_sums) = c("region", "LT_HYDE", "convertible_ha")
	data_out = merge(data_out, conv_sums, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)	
	
	# this is ref veg area and not relevant here
	# land cover
	#lc_sums = aggregate(data_reg_gcam$area_ha, by = list(data_reg_gcam$region, data_reg_gcam$cover), FUN = sum)
	#colnames(lc_sums)[colnames(lc_sums) == "x"] = "land_cover_ha"
	#data_out = merge(data_out, lc_sums, by.x = c("region", "cover"), by.y = c("region", "cover"), all.x = TRUE, sort = FALSE)
	
	# keep a non-sclaed one of data_reg_gcam and the initial data_out for comparison
	data_reg_gcam_moirai = data_reg_gcam
	data_out_initial_moirai = data_out
	
	# scale data_reg_gcam to the gcam output area and recalculate data_out 
	# this is because initial managed land is determined regardless of status
	#	and then the status amounts are applied to gcam unmanaged as fraction of moirai unmanaged
	# while unused croland is still managed cropland in gcam, but is available for expansion,
	# unused pasture is tracked but considered as unmanaged 	grassland for protection and expansion purposes in gcam
	# recall that unknown land types are discarded by gcam
	
	# forest
	temp = merge(data_out_initial_moirai[data_out_initial_moirai$LT_HYDE=="Unmanaged" & data_out_initial_moirai$cover=="Forest",],
		gcam_forest_sub_reg, by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$area_ha
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp$managed_forest = temp$area_ha_scaled * temp$lt_gcam_frac
	temp = merge(data_reg_gcam_moirai[data_reg_gcam_moirai$LT_HYDE=="Unmanaged" & data_reg_gcam_moirai$cover=="Forest",],
		temp[,c("region", "gcam_scalar", "lt_gcam_frac")], by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$area_ha = temp$area_ha * temp$gcam_scalar
	treg = unique(temp$region)
	nreg = length(treg)
	for (r in 1:nreg) {
		fractemp = temp[temp$region==treg[r],]
		fractemp$LT_HYDE = "Harvested"
		fractemp$Status = "SuitableUnprotected"
		fractemp$area_ha = sum(fractemp$area_ha) * fractemp$lt_gcam_frac
		temp = rbind(temp, fractemp[1,])
	}
	temp$area_ha[temp$LT_HYDE != "Harvested"] = temp$area_ha[temp$LT_HYDE != "Harvested"] * (1-temp$lt_gcam_frac[temp$LT_HYDE != "Harvested"])
	temp$gcam_scalar = NULL
	temp$lt_gcam_frac = NULL
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	data_reg_gcam = temp
	
	if ( sum(temp$area_ha[temp$region=="Global"]) != gcam_forest_sub_reg$gcam_cover_area_ha[gcam_forest_sub_reg$region=="Global"] ) {
		cat("Scaled global moirai forest ha", sum(temp$area_ha[temp$region=="Global"]), 
			"doesn't equal gcam global sum ha", gcam_forest_sub_reg$gcam_cover_area_ha[gcam_forest_sub_reg$region=="Global"], "\n")
	}
	
	
	# pasture/grass
	# need to combine all grass types first
	temp = data_out_initial_moirai[(data_out_initial_moirai$LT_HYDE=="Unmanaged" & data_out_initial_moirai$cover=="Grassland") | 
		(data_out_initial_moirai$LT_HYDE=="Pasture"),]
	# determine a scaled version of the original pasture
	moirai_pasture_scaled = temp[temp$LT_HYDE != "Unmanaged",]
	moirai_pasture_scaled = aggregate(area_ha ~ region + LT_HYDE, data = moirai_pasture_scaled, FUN = sum)	
	temp$LT_HYDE = "Unmanaged"
	temp$cover = "Grassland"
	temp = aggregate(area_ha ~ region + cover + LT_HYDE, data = temp, FUN = sum)
	temp = merge(temp,gcam_pasture_sub_reg, by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$area_ha
	moirai_pasture_scaled = merge(moirai_pasture_scaled, temp[,c("region", "gcam_scalar")], by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	moirai_pasture_scaled$area_ha = moirai_pasture_scaled$area_ha * moirai_pasture_scaled$gcam_scalar
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp$pasture = temp$area_ha_scaled * temp$lt_gcam_frac
	temp2 = data_reg_gcam_moirai[(data_reg_gcam_moirai$LT_HYDE=="Unmanaged" & data_reg_gcam_moirai$cover=="Grassland") |
		(data_reg_gcam_moirai $LT_HYDE=="Pasture"),]
	temp2$LT_HYDE = "Unmanaged"
	temp2$cover = "Grassland"
	temp2 = aggregate(area_ha ~ region + cover + LT_HYDE + Status, data = temp2, FUN = sum)
	temp = merge(temp2, temp[,c("region", "gcam_scalar", "lt_gcam_frac")], by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$area_ha = temp$area_ha * temp$gcam_scalar
	treg = unique(temp$region)
	nreg = length(treg)
	for (r in 1:nreg) {
		fractemp = temp[temp$region==treg[r],]
		fractemp$LT_HYDE = "Pasture"
		fractemp$Status = "SuitableUnprotected"
		fractemp$area_ha = sum(fractemp$area_ha) * fractemp$lt_gcam_frac
		temp = rbind(temp, fractemp[1,])
	}
	temp$area_ha[temp$LT_HYDE != "Pasture"] = temp$area_ha[temp$LT_HYDE != "Pasture"] * (1-temp$lt_gcam_frac[temp$LT_HYDE != "Pasture"])
	temp$gcam_scalar = NULL
	temp$lt_gcam_frac = NULL
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	data_reg_gcam = rbind(data_reg_gcam, temp)
	# calc the percentage of gcam pasture compared to the scaled moirai pasture
	gcam_pasture_reduction = merge(gcam_pasture_sub_reg[,c("region", "lt_gcam_area_ha")], moirai_pasture_scaled[,c("region", "area_ha")], by="region", all.x = TRUE)
	gcam_pasture_reduction$gcam_percent_moirai = gcam_pasture_reduction$lt_gcam_area_ha / gcam_pasture_reduction$area_ha * 100
	write.csv(gcam_pasture_reduction, file = paste0(outdir, "gcam_pasture_reduction_", year, ".csv"), row.names = FALSE, na="")
	
	if ( sum(temp$area_ha[temp$region=="Global"]) != gcam_pasture_sub_reg$gcam_cover_area_ha[gcam_pasture_sub_reg$region=="Global"] ) {
		cat("Scaled global moirai grass ha", sum(temp$area_ha[temp$region=="Global"]), 
			"doesn't equal gcam global sum ha", gcam_pasture_sub_reg$gcam_cover_area_ha[gcam_pasture_sub_reg$region=="Global"], "\n")
	}
	
	
	# cropland
	# need to combine all crop types first
	temp = data_out_initial_moirai[(data_out_initial_moirai$LT_HYDE=="Cropland"),]
	temp$cover = "Crops"
	temp = aggregate(area_ha ~ region + cover + LT_HYDE, data = temp, FUN = sum)
	temp = merge(temp,gcam_crop_sub_reg, by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$area_ha
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp$other_arable = temp$area_ha_scaled * temp$lt_gcam_frac
	temp2 = data_reg_gcam_moirai[(data_reg_gcam_moirai$LT_HYDE=="Cropland"),]
	temp2$cover = "Crops"
	temp2 = aggregate(area_ha ~ region + cover + LT_HYDE + Status, data = temp2, FUN = sum)
	temp = merge(temp2, temp[,c("region", "gcam_scalar", "lt_gcam_frac")], by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$area_ha = temp$area_ha * temp$gcam_scalar
	treg = unique(temp$region)
	nreg = length(treg)
	for (r in 1:nreg) {
		fractemp = temp[temp$region==treg[r],]
		fractemp$cover = "OtherArableLand"
		fractemp$Status = "SuitableUnprotected"
		fractemp$area_ha = sum(fractemp$area_ha) * fractemp$lt_gcam_frac
		temp = rbind(temp, fractemp[1,])
	}
	temp$area_ha[temp$cover != "OtherArableLand"] = temp$area_ha[temp$cover != "OtherArableLand"] * (1-temp$lt_gcam_frac[temp$cover != "OtherArableLand"])
	temp$gcam_scalar = NULL
	temp$lt_gcam_frac = NULL
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	data_reg_gcam = rbind(data_reg_gcam, temp)
	
	if ( sum(temp$area_ha[temp$region=="Global"]) != gcam_crop_sub_reg$gcam_cover_area_ha[gcam_crop_sub_reg$region=="Global"] ) {
		cat("Scaled global moirai crop ha", sum(temp$area_ha[temp$region=="Global"]), 
			"doesn't equal gcam global sum ha", gcam_crop_sub_reg$gcam_cover_area_ha[gcam_crop_sub_reg$region=="Global"], "\n")
	}	
	
	
	# shrubland, don't need the split
	temp = merge(data_out_initial_moirai[data_out_initial_moirai$LT_HYDE=="Unmanaged" & data_out_initial_moirai$cover=="Shrubland",],
		gcam_shrub_sub_reg, by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$area_ha
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp$shrubland = temp$area_ha_scaled * temp$lt_gcam_frac
	temp = merge(data_reg_gcam_moirai[data_reg_gcam_moirai$LT_HYDE=="Unmanaged" & data_reg_gcam_moirai$cover=="Shrubland",],
		temp[,c("region", "gcam_scalar", "lt_gcam_frac")], by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$area_ha = temp$area_ha * temp$gcam_scalar
	temp$gcam_scalar = NULL
	temp$lt_gcam_frac = NULL
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	data_reg_gcam = rbind(data_reg_gcam, temp)
	
	if ( sum(temp$area_ha[temp$region=="Global"]) != gcam_shrub_sub_reg$gcam_cover_area_ha[gcam_shrub_sub_reg$region=="Global"] ) {
		cat("Scaled global moirai shrub ha", sum(temp$area_ha[temp$region=="Global"]), 
			"doesn't equal gcam global sum ha", gcam_shrub_sub_reg$gcam_cover_area_ha[gcam_shrub_sub_reg$region=="Global"], "\n")
	}
	
	
	# other, don't need the split
	temp = merge(data_out_initial_moirai[data_out_initial_moirai$LT_HYDE=="Unmanaged" & data_out_initial_moirai$cover=="Other",],
		gcam_other_sub_reg, by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$area_ha
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp$rockicedesert = temp$area_ha_scaled * temp$lt_gcam_frac
	temp = merge(data_reg_gcam_moirai[data_reg_gcam_moirai$LT_HYDE=="Unmanaged" & data_reg_gcam_moirai$cover=="Other",],
		temp[,c("region", "gcam_scalar", "lt_gcam_frac")], by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$area_ha = temp$area_ha * temp$gcam_scalar
	temp$gcam_scalar = NULL
	temp$lt_gcam_frac = NULL
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	data_reg_gcam = rbind(data_reg_gcam, temp)
	
	if ( sum(temp$area_ha[temp$region=="Global"]) != gcam_other_sub_reg$gcam_cover_area_ha[gcam_other_sub_reg$region=="Global"] ) {
		cat("Scaled global moirai other ha", sum(temp$area_ha[temp$region=="Global"]), 
			"doesn't equal gcam global sum ha", gcam_other_sub_reg$gcam_cover_area_ha[gcam_other_sub_reg$region=="Global"], "\n")
	}	
	
	
	# urban, don't need the split
	# need to combine the urban records
	temp = data_out_initial_moirai[(data_out_initial_moirai$LT_HYDE=="UrbanLand"),]
	temp$cover = "Urban"
	temp = aggregate(area_ha ~ region + cover + LT_HYDE, data = temp, FUN = sum)
	temp = merge(temp,gcam_urban_reg, by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$area_ha
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp2 = data_reg_gcam_moirai[(data_reg_gcam_moirai$LT_HYDE=="UrbanLand"),]
	temp2$cover = "Urban"
	temp2 = aggregate(area_ha ~ region + cover + LT_HYDE + Status, data = temp2, FUN = sum)
	temp = merge(temp2, temp[,c("region", "gcam_scalar")], by.x=c("region"), by.y=c("region"), all.x = TRUE, sort=FALSE)
	temp$area_ha = temp$area_ha * temp$gcam_scalar
	temp$gcam_scalar = NULL
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	data_reg_gcam = rbind(data_reg_gcam, temp)
	
	if ( sum(temp$area_ha[temp$region=="Global"]) != gcam_urban_reg$gcam_cover_area_ha[gcam_urban_reg$region=="Global"] ) {
		cat("Scaled global moirai urban ha", sum(temp$area_ha[temp$region=="Global"]), 
			"doesn't equal gcam global sum ha", gcam_urban_reg$gcam_cover_area_ha[gcam_urban_reg$region=="Global"], "\n")
	}
	
	cat("Global sum data_reg_gcam ha is", sum(data_reg_gcam$area_ha[data_reg_gcam$region=="Global"]), "\n")
	cat("Global in landleaf area is", sum(gcam_init_area$lt_gcam_area_ha), "ha\n")
	
	
	### now add the unmanaged unknown records
	# the unknown land does not get scaled because there isn't a gcam reference area
	
	temp = data_reg_gcam_moirai[(data_reg_gcam_moirai$LT_HYDE=="Unmanaged" & data_reg_gcam_moirai$cover=="Unknown"),]
	data_reg_gcam = rbind(data_reg_gcam, temp)
	
	
	### now rebuild data_out with the scaled and split/merged area data
	
	# get general land category areas regardless of status, as status groups will be added as columns
	data_out = aggregate(data_reg_gcam$area_ha, by = list(data_reg_gcam$region, data_reg_gcam$cover, data_reg_gcam$LT_HYDE), FUN = sum)
	colnames(data_out) = c("region", "cover", "LT_HYDE", "area_ha")
	
	# recalc total land area per region based on scaled data plus unknown
	temp = aggregate(area_ha ~ region, data = data_reg_gcam, FUN = sum)
	colnames(temp) = c("region", "total_land_ha")
	data_out = merge(data_out, temp, by = "region", all.x= TRUE, sort= FALSE)
	
	# get land use sums and the convertible land (convertible does not include other and unknown)
	# land use is the total of all covers for each LT_HYDE land use
		# this is to make comparisons with total land type values
	lu_sums = aggregate(data_reg_gcam$area_ha, by = list(data_reg_gcam$region, data_reg_gcam$LT_HYDE), FUN = sum)
	colnames(lu_sums) = c("region", "LT_HYDE", "land_use_ha")
	data_out = merge(data_out, lu_sums, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# gcam convertible land is the unmanaged land that does not include the other and unknown categories
	# this is to make direct comparisons with fixed restrictions that apply the same percentages across land types
	temp = data_reg_gcam[data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	conv_sums = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(conv_sums) = c("region", "LT_HYDE", "convertible_ha")
	data_out = merge(data_out, conv_sums, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	data_out$convertible_ha[data_out$LT_HYDE!="Unmanaged"] = 0
	
	data_out = data_out[order(data_out$region, data_out$LT_HYDE, data_out$cover),]
	
	
	
	
	############## begin calculating status-based areas and fractions
	# get the relevant available area and suitable and protected sums
	# exclude "other" and "unknown" because they are not convertible land
	# the percentages below are calculated as available convertible land divided by all convertible land
	
	# these are suitable and unprotected
	temp = data_reg_gcam[data_reg_gcam$Status == "SuitableUnprotected" & data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	low_avail_sums = aggregate(temp$area_ha, by = list(temp$region, temp$cover, temp$LT_HYDE), FUN = sum)
	colnames(low_avail_sums) = c("region", "cover", "LT_HYDE", "low_avail_ha")
	data_out = merge(data_out, low_avail_sums, by.x = c("region", "cover", "LT_HYDE"), by.y = c("region", "cover", "LT_HYDE"),
		all.x = TRUE, sort = FALSE)

	# these are all suitable
	temp = data_reg_gcam[(data_reg_gcam$Status == "SuitableUnprotected" | data_reg_gcam$Status == "SuitableHighProtectionIntact" |
		data_reg_gcam$Status == "SuitbaleHighProtectionDeforested" | data_reg_gcam$Status == "SuitableLow Protection")
		 & data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	high_avail_sums = aggregate(temp$area_ha, by = list(temp$region, temp$cover, temp$LT_HYDE), FUN = sum)
	colnames(high_avail_sums) = c("region", "cover", "LT_HYDE", "high_avail_ha")
	data_out = merge(data_out, high_avail_sums, by.x = c("region", "cover", "LT_HYDE"), by.y = c("region", "cover", "LT_HYDE"),
		all.x = TRUE, sort = FALSE)

	# these are unsuitable and unprotected
	temp = data_reg_gcam[data_reg_gcam$Status == "UnsuitableUnprotected"  & data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	unsuit_unprot_sums = aggregate(temp$area_ha, by = list(temp$region, temp$cover, temp$LT_HYDE), FUN = sum)
	colnames(unsuit_unprot_sums) = c("region", "cover", "LT_HYDE", "unsuit_unprot_ha")
	data_out = merge(data_out, unsuit_unprot_sums, by.x = c("region", "cover", "LT_HYDE"), by.y = c("region", "cover", "LT_HYDE"),
		all.x = TRUE, sort = FALSE)

	# these are all unsuitable
	temp = data_reg_gcam[(data_reg_gcam$Status == "UnsuitableUnprotected" | data_reg_gcam$Status == "UnsuitableHighProtection" |
		data_reg_gcam$Status == "UnsuitableLowProtection")
		& data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	unsuitable_sums = aggregate(temp$area_ha, by = list(temp$region, temp$cover, temp$LT_HYDE), FUN = sum)
	colnames(unsuitable_sums) = c("region", "cover", "LT_HYDE", "unsuitable_ha")
	data_out = merge(data_out, unsuitable_sums, by.x = c("region", "cover", "LT_HYDE"), by.y = c("region", "cover", "LT_HYDE"),
		all.x = TRUE, sort = FALSE)
			
	# calculate some other categorical sums
	
	# protected and suitable
	data_out$suit_prot_ha = data_out$high_avail_ha - data_out$low_avail_ha
	
	# protected and unsuitable
	data_out$unsuit_prot_ha = data_out$unsuitable_ha - data_out$unsuit_unprot_ha
	
	# all protected
	data_out$protected_ha = data_out$suit_prot_ha + data_out$unsuit_prot_ha
	
	######### this calc somehow blew up due to overflow. not sure how since it is smaller than total land
	#### element 211; converting to double to solve the problem; convert all number fields to double?
	# all unprotected
	data_out$unprotected_ha = as.double(data_out$unsuit_unprot_ha) + data_out$low_avail_ha
	
	# calculate availability percentages by land cover with respect to land use (mainly for unmanaged)
	
	# low availability - suitable and unprotected
	data_out$low_avail_prcnt_cover = data_out$low_avail_ha / data_out$area_ha * 100
	
	# high availability - all four suitable
	data_out$high_avail_prcnt_cover= data_out$high_avail_ha / data_out$area_ha * 100
	
	# unsuitable unprotected
	data_out$unsuit_unprot_prcnt_cover = data_out$unsuit_unprot_ha / data_out$area_ha * 100
	
	# suitable protected
	data_out$suit_prot_prcnt_cover = data_out$suit_prot_ha / data_out$area_ha * 100
	
	# unsuitable protected
	data_out$unsuit_prot_prcnt_cover = data_out$unsuit_prot_ha / data_out$area_ha * 100
	
	
	# calculate availability percentages by land cover with respect to total land
	
	# low availability - suitable and unprotected
	#data_out$low_avail_prcnt_total = data_out$low_avail_ha / data_out$total_land_ha * 100
	
	# high availability - all four suitable
	#data_out$high_avail_prcnt_total = data_out$high_avail_ha / data_out$total_land_ha * 100
	
	# unsuitable unprotected
	#data_out$unsuit_unprot_prcnt_total = data_out$unsuit_unprot_ha / data_out$total_land_ha * 100
	
	# suitable protected
	#data_out$suit_prot_prcnt_total = data_out$suit_prot_ha / data_out$total_land_ha * 100
	
	# unstuiable protected
	#data_out$unsuit_prot_prcnt_total = data_out$unsuit_prot_ha / data_out$total_land_ha * 100
	
	
	# generate sums across land cover for non-differentiated comparison (no other and unknown)
	
	# low availability
	temp = data_reg_gcam[data_reg_gcam$Status == "SuitableUnprotected" & data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	low_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(low_agg) = c("region", "LT_HYDE", "low_avail_conv_ha")
	data_out = merge(data_out, low_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)

	# high availability
	temp = data_reg_gcam[(data_reg_gcam$Status == "SuitableUnprotected" | data_reg_gcam$Status == "SuitableHighProtectionIntact" |
		data_reg_gcam$Status == "SuitbaleHighProtectionDeforested" | data_reg_gcam$Status == "SuitableLow Protection")
		& data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	high_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(high_agg) = c("region", "LT_HYDE", "high_avail_conv_ha")
	data_out = merge(data_out, high_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# unsuitable unprotected
		temp = data_reg_gcam[(data_reg_gcam$Status == "UnsuitableUnprotected") & data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	unsuit_unprot_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(unsuit_unprot_agg) = c("region", "LT_HYDE", "unsuit_unprot_conv_ha")
	data_out = merge(data_out, unsuit_unprot_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# suitable protected
	temp = data_reg_gcam[(data_reg_gcam$Status == "SuitableHighProtectionIntact" |
		data_reg_gcam$Status == "SuitbaleHighProtectionDeforested" | data_reg_gcam$Status == "SuitableLow Protection")
		& data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	suit_prot_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(suit_prot_agg) = c("region", "LT_HYDE", "suit_prot_conv_ha")
	data_out = merge(data_out, suit_prot_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# unsuitable protected
	temp = data_reg_gcam[(data_reg_gcam$Status == "UnsuitableHighProtection" | data_reg_gcam$Status == "UnsuitableLowProtection")
		& data_reg_gcam$cover != "Other" & data_reg_gcam$cover != "Unknown",]
	unsuit_prot_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(unsuit_prot_agg) = c("region", "LT_HYDE", "unsuit_prot_conv_ha")
	data_out = merge(data_out, unsuit_prot_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# calculate undifferentiated availability percentages with respect to convertible land (no other and unknown)
	
	# low availability - suitable and unprotected
	data_out$low_avail_conv_prcnt_conv = data_out$low_avail_conv_ha / data_out$convertible_ha * 100
	
	# high availability - all four suitable
	data_out$high_avail_conv_prcnt_conv = data_out$high_avail_conv_ha / data_out$convertible_ha * 100
	
	# unsuitable unprotected
	data_out$unsuit_unprot_conv_prcnt_conv = data_out$unsuit_unprot_conv_ha / data_out$convertible_ha * 100
	
	# suitable protected
	data_out$suit_prot_conv_prcnt_conv = data_out$suit_prot_conv_ha / data_out$convertible_ha * 100
	
	# unsuitable protected
	data_out$unsuit_prot_conv_prcnt_conv = data_out$unsuit_prot_conv_ha / data_out$convertible_ha * 100
	
	# calculate undifferenteated availability percentages with respect to total land (no other and unknown)
	
	# low availability - suitable and unprotected
	data_out$low_avail_conv_prcnt_total = data_out$low_avail_conv_ha / data_out$total_land_ha * 100
	
	# high availability - all four suitable
	data_out$high_avail_conv_prcnt_total = data_out$high_avail_conv_ha / data_out$total_land_ha * 100
	
	# unsuitable unprotected
	data_out$unsuit_unprot_conv_prcnt_total = data_out$unsuit_unprot_conv_ha / data_out$total_land_ha * 100
	
	# suitable protected
	data_out$suit_prot_conv_prcnt_total = data_out$suit_prot_conv_ha / data_out$total_land_ha * 100
	
	# unsuitable protected
	data_out$unsuit_prot_conv_prcnt_total = data_out$unsuit_prot_conv_ha / data_out$total_land_ha * 100
	
	
	
	#### now calculate the undifferentiated land cover values including "other" and "unknown" land
	# this is mainly to look at unmanaged land use
	# only unmanaged land has other and unknown
	# 	so this won't be different from the values above for non-unmanaged land types
	
	# low availability
	temp = data_reg_gcam[data_reg_gcam$Status == "SuitableUnprotected",]
	low_all_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(low_all_agg) = c("region", "LT_HYDE", "low_avail_all_ha")
	data_out = merge(data_out, low_all_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)

	# high availability
	temp = data_reg_gcam[(data_reg_gcam$Status == "SuitableUnprotected" | data_reg_gcam$Status == "SuitableHighProtectionIntact" |
		data_reg_gcam$Status == "SuitbaleHighProtectionDeforested" | data_reg_gcam$Status == "SuitableLow Protection"),]
	high_all_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(high_all_agg) = c("region", "LT_HYDE", "high_avail_all_ha")
	data_out = merge(data_out, high_all_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# unsuitable unprotected
	temp = data_reg_gcam[(data_reg_gcam$Status == "UnsuitableUnprotected"),]
		unsuit_unprot_all_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(unsuit_unprot_all_agg) = c("region", "LT_HYDE", "unsuit_unprot_all_ha")
	data_out = merge(data_out, unsuit_unprot_all_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# suitable protected
	temp = data_reg_gcam[(data_reg_gcam$Status == "SuitableHighProtectionIntact" |
		data_reg_gcam$Status == "SuitbaleHighProtectionDeforested" | data_reg_gcam$Status == "SuitableLow Protection"),]
	suit_prot_all_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(suit_prot_all_agg) = c("region", "LT_HYDE", "suit_prot_all_ha")
	data_out = merge(data_out, suit_prot_all_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# unsuitable protected
	temp = data_reg_gcam[(data_reg_gcam$Status == "UnsuitableHighProtection" | data_reg_gcam$Status == "UnsuitableLowProtection"),]
	unsuit_prot_all_agg = aggregate(temp$area_ha, by = list(temp$region, temp$LT_HYDE), FUN = sum)
	colnames(unsuit_prot_all_agg) = c("region", "LT_HYDE", "unsuit_prot_all_ha")
	data_out = merge(data_out, unsuit_prot_all_agg, by.x = c("region", "LT_HYDE"), by.y = c("region", "LT_HYDE"), all.x = TRUE, sort = FALSE)
	
	# calculate undifferentiated availability percentages with respect to all land cover, by land use (mainly for unmanaged)
	
	# low availability - suitable and unprotected
	data_out$low_avail_all_prcnt_all = data_out$low_avail_all_ha / data_out$land_use_ha * 100
	
	# high availability - all four suitable
	data_out$high_avail_all_prcnt_all = data_out$high_avail_all_ha / data_out$land_use_ha * 100
	
	# unsuitable unprotected
	data_out$unsuit_unprot_all_prcnt_all = data_out$unsuit_unprot_all_ha / data_out$land_use_ha * 100
	
	# suitable protected
	data_out$suit_prot_all_prcnt_all = data_out$suit_prot_all_ha / data_out$land_use_ha * 100
	
	# unsuitable protected
	data_out$unsuit_prot_all_prcnt_all = data_out$unsuit_prot_all_ha / data_out$land_use_ha * 100
	
	# calculate undifferenteated availability percentages with respect to total land
	
	# low availability - suitable and unprotected
	data_out$low_avail_all_prcnt_total = data_out$low_avail_all_ha / data_out$total_land_ha * 100
	
	# high availability - all four suitable
	data_out$high_avail_all_prcnt_total = data_out$high_avail_all_ha / data_out$total_land_ha * 100
	
	# unsuitable unprotected
	data_out$unsuit_unprot_all_prcnt_total = data_out$unsuit_unprot_all_ha / data_out$total_land_ha * 100
	
	# suitable protected
	data_out$suit_prot_all_prcnt_total = data_out$suit_prot_all_ha / data_out$total_land_ha * 100
	
	# unsuitable protected
	data_out$unsuit_prot_all_prcnt_total = data_out$unsuit_prot_all_ha / data_out$total_land_ha * 100
	
	#### now calculate the undifferentiated all land values including "other" and "unknown" land
	# this includes managed land as well
	
	# low availability
	temp = data_reg_gcam[data_reg_gcam$Status == "SuitableUnprotected",]
	low_extra_agg = aggregate(temp$area_ha, by = list(temp$region), FUN = sum)
	colnames(low_extra_agg) = c("region", "low_avail_extra_ha")
	data_out = merge(data_out, low_extra_agg, by.x = c("region"), by.y = c("region"), all.x = TRUE, sort = FALSE)

	# high availability
	temp = data_reg_gcam[(data_reg_gcam$Status == "SuitableUnprotected" | data_reg_gcam$Status == "SuitableHighProtectionIntact" |
		data_reg_gcam$Status == "SuitbaleHighProtectionDeforested" | data_reg_gcam$Status == "SuitableLow Protection"),]
	high_extra_agg = aggregate(temp$area_ha, by = list(temp$region), FUN = sum)
	colnames(high_extra_agg) = c("region", "high_avail_extra_ha")
	data_out = merge(data_out, high_extra_agg, by.x = c("region"), by.y = c("region"), all.x = TRUE, sort = FALSE)
	
	# unsuitable unprotected
	temp = data_reg_gcam[(data_reg_gcam$Status == "UnsuitableUnprotected"),]
		unsuit_unprot_extra_agg = aggregate(temp$area_ha, by = list(temp$region), FUN = sum)
	colnames(unsuit_unprot_extra_agg) = c("region", "unsuit_unprot_extra_ha")
	data_out = merge(data_out, unsuit_unprot_extra_agg, by.x = c("region"), by.y = c("region"), all.x = TRUE, sort = FALSE)
	
	# suitable protected
	temp = data_reg_gcam[(data_reg_gcam$Status == "SuitableHighProtectionIntact" |
		data_reg_gcam$Status == "SuitbaleHighProtectionDeforested" | data_reg_gcam$Status == "SuitableLow Protection"),]
	suit_prot_extra_agg = aggregate(temp$area_ha, by = list(temp$region), FUN = sum)
	colnames(suit_prot_extra_agg) = c("region", "suit_prot_extra_ha")
	data_out = merge(data_out, suit_prot_extra_agg, by.x = c("region"), by.y = c("region"), all.x = TRUE, sort = FALSE)
	
	# unsuitable protected
	temp = data_reg_gcam[(data_reg_gcam$Status == "UnsuitableHighProtection" | data_reg_gcam$Status == "UnsuitableLowProtection"),]
	unsuit_prot_extra_agg = aggregate(temp$area_ha, by = list(temp$region), FUN = sum)
	colnames(unsuit_prot_extra_agg) = c("region", "unsuit_prot_extra_ha")
	data_out = merge(data_out, unsuit_prot_extra_agg, by.x = c("region"), by.y = c("region"), all.x = TRUE, sort = FALSE)
	
	
	# calculate undifferentiated availability percentages with respect to total land
	
	# low availability - suitable and unprotected
	data_out$low_avail_extra_prcnt_total = data_out$low_avail_extra_ha / data_out$total_land_ha * 100
	
	# high availability - all four suitable
	data_out$high_avail_extra_prcnt_total = data_out$high_avail_extra_ha / data_out$total_land_ha * 100
	
	# unsuitable unprotected
	data_out$unsuit_unprot_extra_prcnt_total = data_out$unsuit_unprot_extra_ha / data_out$total_land_ha * 100
	
	# suitable protected
	data_out$suit_prot_extra_prcnt_total = data_out$suit_prot_extra_ha / data_out$total_land_ha * 100
	
	# unsuitable protected
	data_out$unsuit_prot_extra_prcnt_total = data_out$unsuit_prot_extra_ha / data_out$total_land_ha * 100
	
	######## create output tables
	
	# all regions low and high available as percent of convertible, by cover and undifferentiated, unmanaged land; no other or unknown
	percent_convertible = data_out[data_out$LT_HYDE == "Unmanaged",
		c("region", "cover", "area_ha", "land_use_ha", "convertible_ha", "low_avail_conv_ha", "high_avail_conv_ha",
		"low_avail_conv_prcnt_conv", "high_avail_conv_prcnt_conv", "low_avail_prcnt_cover", "high_avail_prcnt_cover")]
	percent_convertible$other_prcnt_unman = NA
	percent_convertible$other_prcnt_unman[percent_convertible$cover == "Other"] = 100 *
		percent_convertible$area_ha[percent_convertible$cover == "Other"] / percent_convertible$land_use_ha[percent_convertible$cover == "Other"]
	percent_convertible$unknown_prcnt_unman = NA
	percent_convertible$unknown_prcnt_unman[percent_convertible$cover == "Unknown"] = 100 *
		percent_convertible$area_ha[percent_convertible$cover == "Unknown"] / percent_convertible$land_use_ha[percent_convertible$cover == "Unknown"]
	colnames(percent_convertible) = c("Region", "Land_cover", "Cover_area_ha", "Unmanaged_area_ha", "Convertible_area_ha",
		"Low_avail_area_ha", "High_avail_area_ha", "Low_avail_percent_convertible", "High_avail_percent_convertible",
		"Low_avail_cover_percent_convertible", "High_avail_cover_percent_convertible", "Other_percent_unmanaged", "Unknown_percent_unmanaged")
	reference=c("Forest", "Shrubland", "Grassland", "Other", "Unknown")
	percent_convertible = percent_convertible[order(percent_convertible$Region, match(percent_convertible$Land_cover, reference)),]
	write.csv(percent_convertible, file = paste0(outdir, "percent_convertible_unmanaged_adj_", year, ".csv"), row.names = FALSE, na="")
	
	# all regions low and high available and suitable prot and unsuitable prot as percent of total, including all land, undifferentiated
	#	with 20% minus all prot as percent of total
	# it doesn't matter which row to grab at this level of aggregation, except can't use other or shrubland due to some missing catgories
	percent_total = data_out[data_out$cover == "Forest" & data_out$LT_HYDE == "Unmanaged",
		c("region", "total_land_ha", "low_avail_extra_ha", "high_avail_extra_ha", "suit_prot_extra_ha", "unsuit_prot_extra_ha",
		"low_avail_extra_prcnt_total", "high_avail_extra_prcnt_total", "suit_prot_extra_prcnt_total", "unsuit_prot_extra_prcnt_total")]
	percent_total$to20percent_prot = 20 - (percent_total$suit_prot_extra_prcnt_total + percent_total$unsuit_prot_extra_prcnt_total)
	colnames(percent_total) = c("Region", "Total_land_ha", "Low_avail_total_ha", "High_avail_total_ha", "Suitable_protected_total_ha",
		"Unsuitable_protected_total_ha", "Low_avail_total_percent_total", "High_avail_total_percent_total",
		"Suitable_protected_total_percent_total", "Unsuitable_protected_total_percent_total", "Percent_total_needed_for20percent_protected")
	percent_total = percent_total[order(percent_total$Region),]
	write.csv(percent_total, file = paste0(outdir, "percent_total_adj_", year, ".csv"), row.names = FALSE, na="")
	
	# all regions the % of reduced convertible calculated as the area of (20%-all prot from above) / convertible area
	#	with the suitable unprotected and unsuitable unprotected as percent of convertible, unmanaged land
	needed_as_percent_convertible = data_out[data_out$cover == "Forest" & data_out$LT_HYDE == "Unmanaged",
		c("region", "total_land_ha", "convertible_ha", "low_avail_conv_ha", "unsuit_unprot_conv_ha", "low_avail_conv_prcnt_conv", "unsuit_unprot_conv_prcnt_conv",
		"suit_prot_extra_prcnt_total", "unsuit_prot_extra_prcnt_total")]
	needed_as_percent_convertible$to20percent_prot_total = 20 -
		(needed_as_percent_convertible$suit_prot_extra_prcnt_total + needed_as_percent_convertible $unsuit_prot_extra_prcnt_total)
	needed_as_percent_convertible$needed_percent_unprot_convertible =
		needed_as_percent_convertible$to20percent_prot_total * needed_as_percent_convertible$total_land_ha / needed_as_percent_convertible$convertible_ha
	needed_as_percent_convertible$needed_percent_suit_unprot_convertible_after_unsuit_unprot =
		needed_as_percent_convertible$needed_percent_unprot_convertible - needed_as_percent_convertible$unsuit_unprot_conv_prcnt_conv
	needed_as_percent_convertible$needed_percent_unsuit_unprot_convertible_after_suit_unprot =
		needed_as_percent_convertible$needed_percent_unprot_convertible - needed_as_percent_convertible$low_avail_conv_prcnt_conv
	# don't need the duplicate columns
	needed_as_percent_convertible$suit_prot_extra_prcnt_total = NULL
	needed_as_percent_convertible$unsuit_prot_extra_prcnt_total = NULL
	needed_as_percent_convertible$to20percent_prot_total = NULL
	colnames(needed_as_percent_convertible) = c("Region", "Total_land_ha", "Convertible_area_ha", "Suitable_unprotected_area_ha",
		"Unsuitable_unprotected_area_ha", "Suitable_unprotected_percent_convertible", "Unsuitable_unprotected_percent_convertible",
		"Needed_unprotected_percent_convertible", "Needed_suitable_unprot_percent_conv_after_unsuit_unprot",
		"Needed_unsuitable_unprot_percent_conv_after_suit_unprot")
	needed_as_percent_convertible = needed_as_percent_convertible[order(needed_as_percent_convertible$Region),]
	write.csv(needed_as_percent_convertible, file = paste0(outdir, "percent_convertible_needed_adj_", year, ".csv"), row.names = FALSE, na="")
	
	# all relevant global values
	global_values = data_out[data_out$region=="Global",]
	global_values$cover_percent_total = global_values$area_ha / global_values$total_land_ha * 100
	global_values$use_percent_total = global_values$land_use_ha / global_values$total_land_ha * 100
	write.csv(global_values, file = paste0(outdir, "global_values_", year, ".csv"), row.names = FALSE, na="")
	
	######## create output maps of land availability by gcam regionXglu
	# note that there are 397 valid regionXglu units
	# but here we have only 378 with convertible area
	# also, I think only 384 are active in GCAM (check!): these 384 are greater than 200000 ha
	#	the africa-northern dead sea is the smallest at 216051 ha, the others are small region boundary units that do not align exactly with the basin boundaries,
	#	that are either merged or ignored as they may not have much data
	# in ie3sm there are 392 output units - the missing 5 are:
	#	13019 eu-15 atlantic ocean seaboard (canatl), st pierre miquelon france
	#	13101 eu-15 carribean, turks and caicos britain
	#	13080 eu-15 med south coast, melilla spain
	#	12063 eu-12 med sea islands, malta
	#	29177 southeast asia madagascar
	# the 8 additional 'missing' small ones down to 384 are all boundary mismatches:
	#	31207 argentina north chile pacific coast
	#	22085 pakistan tarim interior
	#	11076 china syr_darya
	#	26212 south america southern sa colorado (argcolor)
	#	11058 china amu darya
	#	22058 pakistan amu darya
	#	24196 south africa namibia coast (afrcstsw) - note that this should be part of africa southern region (namibia since 1993)
	#	26205 south america southern salinas grandes
	# but only 378 have convertible area
	# the 6 seemingly valid ones > 200000 ha without convertible area are (to get down to 378):
	#	3151 africa-northern lake chad
	#	3098 africa-northern sinai peninsula
	#	3100 africa-northern red sea gulf aden coast (africa coast ne)
	#	3095 africa-northern dead sea
	#	3149 africa-northern niger
	#	13086 eu-15 africa north west coast (canary islands)
	# note that year 2000 vmap 0 appears to be available from university of toronto now
	
	# also note that the rhone basin 62 has a typo in the moirai input csv file
	# also note that the portion of namibia that is labeled as south africa needs to be updated in the country file
	
	# first aggregate the data by gcam region and glu
	data_reg_glu = aggregate(data_in$value, by = list(data_in$GCAM_region_ID, data_in$glu_code, data_in$region, data_in$glu_name,
								data_in$land_type, data_in$LT_SAGE, data_in$LT_HYDE, data_in$Status), FUN = sum)
	colnames(data_reg_glu) = c("region_id", "glu_id", "region", "glu", "land_type", "LT_SAGE", "LT_HYDE", "Status", "area_ha")
	
	# add the raster thematic field for regionXglu
	data_reg_glu$unit_id = data_reg_glu$region_id * 1000 + data_reg_glu$glu_id
	
	# remove the unknown unmanaged land cover to match gcam regionXglu area for scaling
	data_reg_glu = data_reg_glu[data_reg_glu$land_type >= 10,]
	
	# get total knowon regionXglu land sums for general comparison and add as a new column
	# managed types with unknown cover are considered known
	#reg_glu_land = aggregate(data_reg_glu$area_ha, by = list(data_reg_glu$unit_id), FUN = sum)
	#colnames(reg_glu_land) = c("unit_id", "total_known_land_ha")
	#data_reg_glu = merge(data_reg_glu, reg_glu_land, by.x = c("unit_id"), by.y = c("unit_id"), all.x = TRUE, sort = FALSE)
	
	# scale to gcam regionXglu land area and adjust for managed forest and pasture
	
	# forest
	
	temp = data_reg_glu[data_reg_glu$LT_HYDE == "Unmanaged" & data_reg_glu$land_type >= 100 & data_reg_glu$land_type < 900,]
	temp = aggregate(area_ha ~ unit_id + glu_id + region + Status, data = temp, FUN = sum)
	temp2 = aggregate(area_ha ~ unit_id + glu_id + region, data = temp, FUN = sum)
	colnames(temp2)[ncol(temp2)] <- "unit_ha"
	temp = merge(temp, temp2, by=c("unit_id", "glu_id", "region"), all.x = TRUE)
	temp = merge(gcam_forest_sub, temp, by.x=c("region", "GCAM_basin_ID"), by.y=c("region", "glu_id"), all.x=TRUE, all.y=TRUE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$unit_ha
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp$managed_forest_ha = temp$area_ha_scaled * temp$lt_gcam_frac
	temp$unmanaged_forest_ha = temp$area_ha_scaled * (1 - temp$lt_gcam_frac)
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	
	# there may be some additional units in the gcam data with area, not sure why - probably needed wood product
	# so filter out the unit_id == 0 below, as this shows what units have added area
	cat("moirai global forest scaled ha is", sum(temp$area_ha_scaled), "\n")
	cat("gcam local global forest scaled ha is",sum(unique(temp$gcam_cover_area_ha)), "\n")
	cat("gcam in global forest scaled ha is",gcam_forest_sub_reg$gcam_cover_area_ha[gcam_forest_sub_reg$region=="Global"], "\n")
	
	# store the relevant info in a df
	temp = temp[temp$unit_id != 0, c("region", "GCAM_basin_ID", "unit_id", "Status", "managed_forest_ha", "unmanaged_forest_ha")]
	plot_df = temp


	# pasture/grass
	
	temp = data_reg_glu[(data_reg_glu$LT_HYDE == "Unmanaged" & data_reg_glu$land_type >= 900 & data_reg_glu$land_type < 1100) |
		data_reg_glu$LT_HYDE == "Pasture",]
	temp = aggregate(area_ha ~ unit_id + glu_id + region + Status, data = temp, FUN = sum)
	temp2 = aggregate(area_ha ~ unit_id + glu_id + region, data = temp, FUN = sum)
	colnames(temp2)[ncol(temp2)] <- "unit_ha"
	temp = merge(temp, temp2, by=c("unit_id", "glu_id", "region"), all.x = TRUE)
	temp = merge(gcam_pasture_sub, temp, by.x=c("region", "GCAM_basin_ID"), by.y=c("region", "glu_id"), all.x=TRUE, all.y=TRUE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$unit_ha
	temp$area_ha_scaled = temp$area_ha * temp$gcam_scalar
	temp$grazed_pasture_ha = temp$area_ha_scaled * temp$lt_gcam_frac
	temp$grassland_ha = temp$area_ha_scaled * (1 - temp$lt_gcam_frac)
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	
	# there may be some additional units in the gcam data with area, not sure why - probably needed wood product
	# so filter out the unit_id == 0 below, as this shows what units have added area
	cat("moirai global grass scaled ha is", sum(temp$area_ha_scaled), "\n")
	cat("gcam local global grass scaled ha is",sum(unique(temp$gcam_cover_area_ha)), "\n")
	cat("gcam in global grass scaled ha is",gcam_pasture_sub_reg$gcam_cover_area_ha[gcam_pasture_sub_reg$region=="Global"], "\n")
	
	# store the relevant info in a df
	temp = temp[temp$unit_id != 0, c("region", "GCAM_basin_ID", "unit_id", "Status", "grazed_pasture_ha", "grassland_ha")]
	plot_df = merge(plot_df, temp, by=c("region", "GCAM_basin_ID", "unit_id", "Status"),  all.x=TRUE, all.y=TRUE)
	plot_df[is.na(plot_df)] = 0
	
	
	# shrubland

	temp = data_reg_glu[data_reg_glu$LT_HYDE == "Unmanaged" & data_reg_glu$land_type >= 1100 & data_reg_glu$land_type < 1300,]
	temp = aggregate(area_ha ~ unit_id + glu_id + region + Status, data = temp, FUN = sum)
	temp2 = aggregate(area_ha ~ unit_id + glu_id + region, data = temp, FUN = sum)
	colnames(temp2)[ncol(temp2)] <- "unit_ha"
	temp = merge(temp, temp2, by=c("unit_id", "glu_id", "region"), all.x = TRUE)
	temp = merge(gcam_shrub_sub, temp, by.x=c("region", "GCAM_basin_ID"), by.y=c("region", "glu_id"), all.x=TRUE, all.y=TRUE)
	temp$gcam_scalar = temp$gcam_cover_area_ha / temp$unit_ha
	temp$shrubland_ha = temp$area_ha * temp$gcam_scalar
	temp[is.na(temp)] = 0
	temp[temp == Inf] = 0
	
	# there may be some additional units in the gcam data with area, not sure why - probably needed wood product
	# so filter out the unit_id == 0 below, as this shows what units have added area
	cat("moirai global shrub scaled ha is", sum(temp$shrubland_ha), "\n")
	cat("gcam local global shrub scaled ha is",sum(unique(temp$gcam_cover_area_ha)), "\n")
	cat("gcam in global shrub scaled ha is",gcam_shrub_sub_reg$gcam_cover_area_ha[gcam_shrub_sub_reg$region=="Global"], "\n")
	
	# store the relevant info in a df
	temp = temp[temp$unit_id != 0, c("region", "GCAM_basin_ID", "unit_id", "Status", "shrubland_ha")]
	plot_df = merge(plot_df, temp, by=c("region", "GCAM_basin_ID", "unit_id", "Status"),  all.x=TRUE, all.y=TRUE)
	plot_df[is.na(plot_df)] = 0
	
		
	# aggregate the convertible area
	plot_df$unmanaged_ha = plot_df$unmanaged_forest + plot_df$grassland_ha + plot_df$shrubland_ha
	temp = aggregate(unmanaged_ha ~ region + GCAM_basin_ID + unit_id, data=plot_df, FUN = sum)
	colnames(temp)[ncol(temp)] = c("convertible_unit_ha")
	plot_df = merge(plot_df, temp, by = c("region", "GCAM_basin_ID", "unit_id"), all.x = TRUE)
	
	# aggregate the four map variables
	
	# low availability (suitable and unprotected)
	temp = plot_df[plot_df$Status == "SuitableUnprotected",]
	temp_agg = aggregate(temp$unmanaged_ha, by = list(temp$unit_id), FUN = sum)
	colnames(temp_agg) = c("unit_id", "low_avail_ha")
	plot_df = merge(plot_df, temp_agg, by.x = c("unit_id"), by.y = c("unit_id"), all.x = TRUE, sort = FALSE)
	
	# high availability (all suitable)
	temp = plot_df[plot_df$Status == "SuitableUnprotected" | plot_df$Status == "SuitableHighProtectionIntact" |
		plot_df$Status == "SuitbaleHighProtectionDeforested" | plot_df$Status == "SuitableLow Protection",]
	temp_agg = aggregate(temp$unmanaged_ha, by = list(temp$unit_id), FUN = sum)
	colnames(temp_agg) = c("unit_id", "high_avail_ha")
	plot_df = merge(plot_df, temp_agg, by.x = c("unit_id"), by.y = c("unit_id"), all.x = TRUE, sort = FALSE)

	# suitable protected
	plot_df$suit_prot_ha = plot_df$high_avail_ha - plot_df$low_avail_ha
	
	# unsuitable protected - don't include unknown as unsuitable and protected (because it doesn't exist)
	temp = plot_df[plot_df$Status == "UnsuitableHighProtection" | plot_df$Status == "UnsuitableLowProtection",]
	temp_agg = aggregate(temp$unmanaged_ha, by = list(temp$unit_id), FUN = sum)
	colnames(temp_agg) = c("unit_id", "unsuit_prot_ha")
	plot_df = merge(plot_df, temp_agg, by.x = c("unit_id"), by.y = c("unit_id"), all.x = TRUE, sort = TRUE)

	# reduce the df for calculations and rat
	plot_df_trim = plot_df[,c("region", "GCAM_basin_ID", "unit_id", "convertible_unit_ha", "low_avail_ha", "high_avail_ha", "suit_prot_ha", "unsuit_prot_ha")]
	plot_df_trim = unique(plot_df_trim)
	plot_df_trim[is.na(plot_df_trim)] = 0
	plot_df_trim = plot_df_trim[order(plot_df_trim$region, plot_df_trim$GCAM_basin_ID),]

	# calculate the percentages of convertible land to map
	
	# low availability (suitable and unprotected)
	plot_df_trim$low_avail_pcnt_conv = plot_df_trim$low_avail_ha / plot_df_trim$convertible_unit_ha * 100
	
	# high availability (all suitable)
	plot_df_trim$high_avail_pcnt_conv = plot_df_trim$high_avail_ha / plot_df_trim$convertible_unit_ha * 100
	
	# suitable protected
	plot_df_trim$suit_prot_pcnt_conv = plot_df_trim$suit_prot_ha / plot_df_trim$convertible_unit_ha * 100
	
	# unsuitable protected
	plot_df_trim$unsuit_prot_pcnt_conv = plot_df_trim$unsuit_prot_ha / plot_df_trim$convertible_unit_ha * 100

	plot_df_trim[plot_df_trim == Inf] = 0
	plot_df_trim[plot_df_trim == -Inf] = 0
	plot_df_trim[is.na(plot_df_trim)] = 0

	# write the data in a table
	write.csv(plot_df_trim, file = paste0(outdir, "percent_convertible_plots_adj_", year, ".csv"), row.names = FALSE, na="")

	# link the df to the raster via rat
	gcam_reg_glu_rast = raster(paste0(rast_dir,gcam_reg_glu_rast_fname))
	gcam_reg_glu_rast_rat = ratify(gcam_reg_glu_rast)
	rat = levels(gcam_reg_glu_rast_rat)[[1]]
	rat = merge(rat, plot_df_trim, by.x = c("ID"), by.y = c("unit_id"), all.x = TRUE, sort = FALSE)
	# this ensures that the remaining units with non-convertible land have zero values rather than being blank
	rat[is.na(rat)] = 0
	levels(gcam_reg_glu_rast_rat) <- rat
	
	pal <- colorRampPalette(brewer.pal(11,"Spectral"))

	# gcam region boundaries
	gcam_region_shapes = readOGR(shape_dir, gcam_region_shape_lname)
	country_shapes = readOGR(shape_dir, country_shape_lname)

	low_avail_pcnt_conv_rast=deratify(gcam_reg_glu_rast_rat, att="low_avail_pcnt_conv")
    outim <- levelplot(low_avail_pcnt_conv_rast, main="Low avail % convertible", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101))
    outim <- outim + layer(sp.lines(country_shapes, lwd=0.4, col = "white")) + layer(sp.lines(gcam_region_shapes, lwd=0.4, col = "black"))
    print(outim)
    pdf(file=paste0(outdir, "low_avail_prcnt_conv_adj_", year, ".pdf"))
    print(outim)
    dev.off()
    
	high_avail_pcnt_conv_rast=deratify(gcam_reg_glu_rast_rat, att="high_avail_pcnt_conv")
    outim <- levelplot(high_avail_pcnt_conv_rast, main="High avail % convertible", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101))
	print(outim)
    pdf(file=paste0(outdir, "high_avail_prcnt_conv_adj_", year, ".pdf"))
    print(outim)
    dev.off()
    
    suit_prot_pcnt_conv_rast=deratify(gcam_reg_glu_rast_rat, att="suit_prot_pcnt_conv")
    outim <- levelplot(suit_prot_pcnt_conv_rast, main="Suitable protected % convertible", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101))
	print(outim)
    pdf(file=paste0(outdir, "suit_prot_prcnt_conv_adj_", year, ".pdf"))
    print(outim)
    dev.off()

	unsuit_prot_pcnt_conv_rast=deratify(gcam_reg_glu_rast_rat, att="unsuit_prot_pcnt_conv")
    outim <- levelplot(unsuit_prot_pcnt_conv_rast, main="Unsuitable protected % convertible", col.regions = pal(100), margin = FALSE, at=seq(0,100,length.out=101))
	print(outim)
    pdf(file=paste0(outdir, "unsuit_prot_prcnt_conv_adj_", year, ".pdf"))
    print(outim)
    dev.off()
	
	cat("finish proc_moirai_land_distribution at", date(), "\n")
}