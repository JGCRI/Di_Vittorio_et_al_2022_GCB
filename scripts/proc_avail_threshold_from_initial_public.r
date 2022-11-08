# proc_avail_threshold_from_initial_public.r

# determine the availability at which a change threshold occurs for 2015 to 2050 changes across availability intervals
# change units are percent change from 2015 (from 2025 in the case of biomass)
# interval units are 10% change in availabilty

# these thresholds are based on a fraction of the initial (100% availabiltiy) percent change to normalize the effects due to differences in initial land area
#    and change with full availability

# just calculate the global trend

change_file = "../paper_figures/Figure_3.csv"

out_file = "../paper_figures/Figure_3_thresholds.csv"

intervals = c("100 percent", "90 percent", "80 percent", "70 percent", "60 percent", "50 percent", "40 percent", "30 percent", "20 percent", "10 percent", "0 percent")
num_int = length(intervals)
interval_value = 10

# the fraction of initial change that determines a significant effect
threshold_frac = 0.90

change_all = read.csv(change_file,  stringsAsFactors=FALSE)
# the global value is its own column, so just need one regional record
# extract the USA record cuz it is in every case for every land type
change_usa = change_all[change_all$region == "USA",]

land_types = unique(change_all$agg.land)
num_lt = length(land_types)

# store the bracketing availability where the threshold is crossed
availability_thresh = array(dim=c(num_lt, 2))
availability_thresh[,] = NA

# store the bracketing changes where the threshold is crossed
change_brackets = array(dim=c(num_lt, 2))
change_brackets[,] = NA

# store the land type threshold
lt_thresh = array(dim=c(num_lt))
lt_thresh[] = NA

for (l in 1:num_lt){
	change_lt = change_usa[change_usa$agg.land == land_types[l],]
	
	# threshold value - use absolute value
	full_avail_change = change_lt[change_lt$Availability == "100 percent", "global_value"]
	lt_thresh[l] = abs(threshold_frac * full_avail_change)
	
	# determine when the threshold has been reached
	# use absolute values to reduce code redundancy
	for (i in 2:num_int) {
		
		if (abs(change_lt[change_lt$Availability == intervals[i-1], "global_value"]) > lt_thresh[l] &
				abs(change_lt[change_lt$Availability == intervals[i], "global_value"]) <= lt_thresh[l]) {
					availability_thresh[l,1] = intervals[i-1]
					availability_thresh[l,2] = intervals[i]
					change_brackets[l,1] = abs(change_lt[change_lt$Availability == intervals[i-1], "global_value"])
					change_brackets[l,2] = abs(change_lt[change_lt$Availability == intervals[i], "global_value"])
		}
		
	} # end for i loop over intervals
	
}	# end for l loop over land types

output = data.frame(land_types=land_types, thresh_frac=rep(threshold_frac,num_lt), thresh_val=lt_thresh,
	avail_ceiling=availability_thresh[,1], avail_floor=availability_thresh[,2], change_ceiling = change_brackets[,1], change_floor = change_brackets[,2])

write.csv(output, file = out_file, row.names=FALSE, na="")