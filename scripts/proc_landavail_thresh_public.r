# proc_landavail_thresh.r

# make sure the working directory is this scripts directory
setwd("./")

# this is all based on 2050 allocation and the 2015 initial data

marginal_data = read.csv("../outputs/csv/6. Marginal_plot_percent.csv", stringsAsFactors=FALSE)
marginal_data$X = NULL

marginal_area_data = read.csv("../outputs/csv/6. Marginal_plot_thouskm2.csv", stringsAsFactors=FALSE)
marginal_area_data$X = NULL
marginal_area_data$available = NULL

conv_data = read.csv("../outputs/distribution_threshold/percent_convertible_unmanaged_adj_2015.csv",
     stringsAsFactors=FALSE)

need_data = read.csv("../outputs/distribution_threshold/percent_convertible_needed_adj_2015.csv",
     stringsAsFactors=FALSE)

alloc_data = read.csv("../outputs/csv/1. Land allocation by typeProtected area paperall regions.csv",
	 stringsAsFactors=FALSE)

marginal_year = 2050

outdir = "../outputs/distribution_threshold/"
impact_csv = paste0(outdir, "impact_of_protection_", marginal_year, ".csv")
efficiency_csv = paste0(outdir, "efficiency_of_protection_", marginal_year, ".csv")

# this is for checking inflection point
threshold = 0.1

# this is the case interval (10%) for calculating the available area change
interval_pct = 10
interval_frac = interval_pct / 100

ha2thkmsq = 1 / 100000

region_names = unique(marginal_data$region)
land_names = unique(marginal_data$agg.land)

# need this to map land names between files
map_land_names = c("Biomass", "Crops", "Forests (Harvested)", "Forests (Unmanaged)", "Grassland", "Grazed Pasture", "Shrubland")
map_alloc_land_names = c("Biomass", "Crops", "Forests (Harvested)", "Forests (Unmanaged)", "Grassland", "Grazed Pasture", "Shrubland" )

# distinguish between managed and unmanaged land
man_land = c("Biomass", "Crops", "Forests (Harvested)", "Grazed Pasture")
unman_land = c("Forests (Unmanaged)", "Grassland", "Shrubland")

# for now use the protection constraint files and use these names to reorder the records to availability
prot_constr = c("10 percent", "20 percent", "30 percent", "40 percent", "50 percent", "60 percent", "70 percent", "80 percent", "90 percent", "100 percent")
prot_constr_val = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
num_prot = length(prot_constr)
avail = c(90, 80, 70, 60, 50, 40, 30, 20, 10, 0)

# output dfs
# full df
out_df = NULL

# percent impact of protection df
impact_prot_df = NULL

# percent of protection df
efficiency_prot_df = NULL

reg_man_sum_pct = array(dim = length(avail))
reg_unman_sum_pct = array(dim = length(avail))
reg_all_sum_pct = array(dim = length(avail))
reg_man_sum_pct[] = 0.0
reg_unman_sum_pct[] = 0.0
reg_all_sum_pct[] = 0.0
reg_sum_df = data.frame(avail, reg_man_sum_pct, reg_unman_sum_pct, reg_all_sum_pct)

for (r in region_names) {
	
	# get the land allocation file and data for this region
	alloc_data_reg = alloc_data[alloc_data$region == r,]
	alloc_data_reg$X = NULL
	alloc_land_names = unique(alloc_data_reg$agg.land)
	
	reg_sum_df$reg_man_sum_pct = 0.0
	reg_sum_df$reg_unman_sum_pct = 0.0
	reg_sum_df$reg_all_sum_pct = 0.0
	
	for ( l in land_names) {
		
		curr_df = marginal_data[marginal_data$region == r & marginal_data$agg.land == l,]
		curr_area_df = marginal_area_data[marginal_data$region == r & marginal_data$agg.land == l,]
		if (nrow(curr_df) != 0 & l != "desert" & l != "rock and desert" & l != "urban") {
			
			cat(r, l, "processing \n")
			
			# get the marginal_year land area for this land type and the low scenario (in th km^2)
			aln_ind = match(l, map_land_names)
			curr_alloc = alloc_data_reg$value[alloc_data_reg$year == marginal_year & alloc_data_reg$Availability == "Low Availability" &
			                        alloc_data_reg$agg.land == map_alloc_land_names[aln_ind]]                      
			
			# get the available area change
			avail_area_change = interval_frac * conv_data$Convertible_area_ha[conv_data$Region == r][1] * ha2thkmsq
			
			curr_df$Availability = NA
			curr_area_df$Availability = NA
			for (p in 1:num_prot) {
				curr_df$Availability[curr_df$Protection == prot_constr_val[p]] = avail[p]
				curr_area_df$Availability[curr_area_df$Protection == prot_constr_val[p]] = avail[p]
			}
			curr_df = curr_df[order(curr_df$Availability, decreasing = TRUE),]
			curr_area_df = curr_area_df[order(curr_area_df$Availability, decreasing = TRUE),]
			curr_df$Protection = NULL
			curr_area_df$Protection = NULL
			
			# create the temporary out df for this r and l
			temp_df = curr_area_df
			hist_ind = match("hist_value", names(temp_df))
			names(temp_df)[hist_ind] = "hist_value_thkm2"
			val_ind = match("value", names(temp_df))
			names(temp_df)[val_ind] = "marginal_change_thkm2"
			temp_df = merge(temp_df, curr_df, by=c("region", "year", "policy", "agg.land", "Availability"), all.x=TRUE)
			temp_df$hist_value = NULL
			val_ind = match("value", names(temp_df))
			names(temp_df)[val_ind] = "marginal_change_percent"
			temp_df = temp_df[order(temp_df$Availability, decreasing = TRUE),]
			
			# get change and slope from prev point to current point, for both percent and area
			# also get land type area change as percent of available area change
			for ( a in 1:length(avail)) {
				if (a > 1){
					# slope of percent change
					temp_df$slope_percent[temp_df$Availability == avail[a]] = 
				       ( temp_df$marginal_change_percent[temp_df$Availability == avail[a]] - 
				       temp_df$marginal_change_percent[temp_df$Availability == avail[a-1]]) / interval_pct
				    curr_df$slope[curr_df$Availability == avail[a]] = temp_df$slope_percent[temp_df$Availability == avail[a]]
				    temp_df$slope_area[temp_df$Availability == avail[a]] = 
				       ( temp_df$marginal_change_thkm2[temp_df$Availability == avail[a]] - 
				       temp_df$marginal_change_thkm2[temp_df$Availability == avail[a-1]]) / interval_pct
				} else {
					temp_df$slope_percent[temp_df$Availability == avail[a]] = 0.0
					temp_df$slope_area[temp_df$Availability == avail[a]] = 0.0
					curr_df$slope[curr_df$Availability == avail[a]] = 0.0
				}
				# area change as percent of available area change
				temp_df$percent_type2avail_area_change[temp_df$Availability == avail[a]] =
				    		temp_df$marginal_change_thkm2[temp_df$Availability == avail[a]] / avail_area_change * 100
			}
			
			# get the current amount of available convertible land
			# for managed types this value is based on the total unmanaged land
			# for unmanaged types this values is based on the specific land type
			if (l != "Forests (Unmanaged)" & l != "Grassland" & l != "Shrubland") {
				curr_avail = conv_data$Low_avail_percent_convertible[conv_data$Region == r][1]
			} else {
				if (l == "Forests (Unmanaged)") {lc = "Forest"
				} else if (l == "Grassland") {lc = "Grassland"
				} else if (l == "Shrubland") {lc = "Shrubland"}
				curr_avail = conv_data$Low_avail_cover_percent_convertible[conv_data$Region == r & conv_data$Land_cover == lc]
			}
			if(is.na(curr_avail)) {curr_avail = 0.0}
			temp_df$low_avail_percent_conv = curr_avail
			
			# get the percent needed to reach global double total land protection (20% in gcam, 30% in wdpa)
			need_add_prot = need_data$Needed_unprotected_percent_convertible[need_data$Region == r]

			# calculate the final available percent after added protection
			temp_df$needed_percent_conv = need_add_prot
			final_avail = curr_avail - need_add_prot
			temp_df$final_avail_percent_conv = final_avail
			
			
			
			# these are diagnostic - probably can delete
			# this assumes that low avail is always <= 100 
			low_availhi_ind = which(temp_df$low_avail_percent_conv < temp_df$Availability)
			if(length(low_availhi_ind) > 0) {
				low_availhi_ind = max(low_availhi_ind)
				low_availhi = temp_df$Availability[low_availhi_ind]
			} else {
				low_availhi_ind = -1
				low_availhi = 100
			}
			low_availlo_ind = which(temp_df$low_avail_percent_conv >= temp_df$Availability)
			if(length(low_availlo_ind) > 0) {
				low_availlo_ind = min(low_availlo_ind)
				low_availlo = temp_df$Availability[low_availlo_ind]
			} else {
				low_availlo_ind = -1
				low_availlo = -10
			}
			# final can be less than zero
			# this assumes that final avail is always <= 100
			final_availhi_ind = which(temp_df$final_avail_percent_conv < temp_df$Availability)
			if(length(final_availhi_ind) > 0) {
				final_availhi_ind = max(final_availhi_ind)
				final_availhi = temp_df$Availability[final_availhi_ind]
			} else {
				final_availhi_ind = -1
				final_availhi = 100
			}
			final_availlo_ind = which(temp_df$final_avail_percent_conv >= temp_df$Availability)
			if(length(final_availlo_ind) > 0) {
				final_availlo_ind = min(final_availlo_ind)
				final_availlo = temp_df$Availability[final_availlo_ind]
			} else {
				final_availlo_ind = -1
				final_availlo = -10
			}
			# probably can delete to here
			
			
			# calculate the percent change in land type area from low avail to added protection
			# use the areas to determine the percent change from the starting point
			# note that the y-values are area or percent change from the state at the previous (higher availability) segment
			
			# need to check if final has lower or higher availability
			if (need_add_prot > 0) {
				# normal direction: decreasing availability
				# sum weighted changes down to final or 0 availability
				start = curr_avail
				start_lo = start %/% interval_pct * interval_pct
				start_lo_ind = match(start_lo, avail)
				area_sum = 0.0
				while (start_lo >= 0) {
					if (final_avail >= start_lo) {
						# last segment
						area_sum = area_sum + (start - final_avail) * temp_df$slope_area[start_lo_ind]
						start_lo = -1
					} else {
						# intermediate segment
						area_sum = area_sum + (start - start_lo) * temp_df$slope_area[start_lo_ind]
						start = start_lo
						start_lo = start_lo - interval_pct
						start_lo_ind = match(start_lo, avail)
					} 
				} # end while calculating percent change segments
				
				temp_df$area_change_via_protection = area_sum
			} else if (need_add_prot < 0) {
				# reverse direction because this region already has enough protected area
				# sum weighted changes up to final or 100 availability
				start = curr_avail
				start_hi = start %/% interval_pct * interval_pct + interval_pct
				start_lo = start %/% interval_pct * interval_pct	# the slope is at start lo
				start_lo_ind = match(start_lo, avail)
				area_sum = 0.0
				while (start_hi <= 100) {
					#cat("start is", start, "\n")
					#start_hi = start %/% interval_pct * interval_pct + interval_pct
					#start_lo = start %/% interval_pct * interval_pct	# the slope is at start lo
					#start_lo_ind = match(start_lo, avail)
					if (final_avail <= start_hi) {
						# last segment
						area_sum = area_sum + (start - final_avail) * temp_df$slope_percent[start_lo_ind]
						start_hi = 101
					} else {
						# intermediate segment
						area_sum = area_sum + (start - start_hi) * temp_df$slope_percent[start_lo_ind]
						start = start_hi
						start_hi = start_hi + interval_pct 
						start_lo = start_lo + interval_pct
						start_lo_ind = match(start_lo, avail)
					} 
				} # end while calculating percent change segments
				
				temp_df$area_change_via_protection = area_sum
				
			} else {
				temp_df$area_change_via_protection = 0
			} # end if less availability else more availability
			
			temp_df$percent_change_via_protection = temp_df$area_change_via_protection / curr_alloc * 100
			
			if (TRUE) {
			# find the threshold past current avail land
			thresh_inds = which(abs(curr_df$slope) > threshold & curr_df$Availability < curr_avail)
			#lowest_thresh_ind = which(abs(curr_df$slope) == min(  abs(curr_df$slope[thresh_inds]) ) )
			first_thresh_ind = min(thresh_inds)
			cat("\n", r, l, "current available percent is", curr_avail, "and threshold slope is", threshold, "\n")
			cat(r, l, "need add protected percent is", need_add_prot, "reducing avail to", curr_avail - need_add_prot, "\n")
			cat(r, l, "first curr slope at decrease to", curr_df$Availability[first_thresh_ind], "is", curr_df$slope[first_thresh_ind], "\n")
            
            # find the slope where the current avail percent falls
            currsl_inds = which(curr_df$Availability < curr_avail)
            currsl_ind = min(currsl_inds)
            cat(r, l, "slope at curr avail is", curr_df$slope[currsl_ind], "\n")
            
            # find the threshold regardless of current avail land
			thresh_inds = which(abs(curr_df$slope) > threshold)
			lowest_thresh_ind = which(abs(curr_df$slope) == min(  abs(curr_df$slope[thresh_inds]) ) )
			cat(r, l, "min slope at decrease to", curr_df$Availability[lowest_thresh_ind], "is", curr_df$slope[lowest_thresh_ind], "\n")
			}
			
			out_df = rbind(out_df, temp_df)
			
			# now sum the unmanaged percent type2avail area changes at each avail level
			# assume availability is descending in both dfs
			if (l %in% unman_land) {
				reg_sum_df$reg_unman_sum_pct = reg_sum_df$reg_unman_sum_pct + temp_df$percent_type2avail_area_change
				reg_sum_df$reg_all_sum_pct = reg_sum_df$reg_all_sum_pct + temp_df$percent_type2avail_area_change
			}
			if (l %in% man_land) {
				reg_sum_df$reg_man_sum_pct = reg_sum_df$reg_man_sum_pct + temp_df$percent_type2avail_area_change
				reg_sum_df$reg_all_sum_pct = reg_sum_df$reg_all_sum_pct + temp_df$percent_type2avail_area_change
			}
	
			# now sum the manage percent type2avail area changes at each avail level
			
			# just get the percent impact rows by r and l
			temp_impact_df = unique(temp_df[,c("region", "year", "policy", "agg.land", "hist_value_thkm2", "low_avail_percent_conv",
			                   "needed_percent_conv", "final_avail_percent_conv", "area_change_via_protection", "percent_change_via_protection")])
			temp_impact_df$percent_change_via_protection = round(temp_impact_df$percent_change_via_protection,3)         
			impact_prot_df = rbind(impact_prot_df, temp_impact_df)
			
		} # end if valid records
		
	} # end for l
	
	temp_sum_df = merge(temp_df[,c(1:3,5)], reg_sum_df, by.x = "Availability", by.y = "avail", all.x = TRUE)
	temp_sum_df = temp_sum_df[order(temp_sum_df$Availability, decreasing = TRUE),]
	
	
	efficiency_prot_df = rbind(efficiency_prot_df, temp_sum_df)
	
} # end for r

# find some values
for (l in unique(marginal_area_data$agg.land)) {
	cat("max", l, "percent change from low to target:\n")
	print(impact_prot_df[which(max(impact_prot_df$percent_change_via_protection[impact_prot_df$agg.land == l], na.rm=TRUE) ==
			impact_prot_df$percent_change_via_protection ),])
	cat("min", l, "percent change from low to target:\n")
	print(impact_prot_df[which(min(impact_prot_df$percent_change_via_protection[impact_prot_df$agg.land == l], na.rm = TRUE) ==
			impact_prot_df$percent_change_via_protection ),])
}

write.csv(impact_prot_df, file = impact_csv, row.names = FALSE, na="")
write.csv(efficiency_prot_df, file = efficiency_csv, row.names = FALSE, na="")


