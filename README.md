[![DOI](https://zenodo.org/badge/476038123.svg)](https://zenodo.org/badge/latestdoi/476038123)

# Di_Vittorio_et_al_2022_GCB

**Uniform land protection targets may be inefficient at preserving the extent of undeveloped land and could cause substantial regional shifts in land use**

Alan V. Di Vittorio<sup>1\*</sup>,Kanishka B. Narayan <sup>2</sup>
,Pralit Patel <sup>2</sup>, Katherine Calvin <sup>2</sup> & Chris R. Vernon <sup>2</sup>


<sup>1 </sup> Lawrence Berkeley National Laboratory, Berkeley CA  

<sup>2 </sup> Joint Global Change Research Institute, Pacific Northwest National Lab, Washington DC, USA 


\* corresponding author:  avdivittorio@lbl.gov

## Abstract
Land change projection is highly uncertain yet drives critical estimates of carbon emissions, climate change, and food and bioenergy production. We use new, spatially-explicit land availability data in conjunction with a model sensitivity analysis to estimate the effects of additional land protection on land use and cover. The land availability data includes protected land and agricultural suitability and is incorporated into the Moirai land data system for initializing the Global Change Analysis Model (GCAM). Overall, decreasing land availability is relatively inefficient at preserving undeveloped land while having considerable regional land use impacts. Current amounts of protected area have little effect on land and crop production estimates, but including the spatial distribution of unsuitable (i.e., unavailable) land dramatically shifts bioenergy production from high northern latitudes to the rest of the world, as compared to uniform availability. This highlights the importance of spatial heterogeneity in understanding and managing land change. Approximately doubling current protected area to emulate a 30% protected area target may avoid conversion by 2050 of less than half the newly protected extent while reducing bioenergy feedstock land by 10.4% and cropland and grazed pasture by over 3%. Regional bioenergy land may be reduced by up to 46%, cropland reduced by up to61% and pasture reduced by up to 100%, with only a few regions showing notable gains in some undeveloped land types of up to 34%. Half of the regions can reach the target using only unsuitable land, which would minimize impacts to agriculture but may not meet conservation goals. Rather than focusing on an area target, it may be more important to carefully select newly protected land in order to meet well-defined conservation goals while minimizing impacts to agriculture.


## Data reference

### Models and software used
Models used for this experiment include, 
1. Global Change Analysis Model (GCAM) v5.4 with modifications for the protected areas and bio-energy constraints. Latest version of GCAM available [here](https://zenodo.org/record/6619287) and latest documentation on GCAM available [here](https://jgcri.github.io/gcam-doc/). The specific model version used for this analysis with all configuration files used is available [here](https://zenodo.org/record/7057561).
2. moirai land data system v 3.1.1. Latest version with documentation available on GitHub [here](https://github.com/JGCRI/moirai) 

### Input data
All GCAM runs are available as `R` project files within the `project_files/` directory.Input xml files for GCAM runs can be made available on reasonbable request. Ancillary mapping files are stored within the repo under folder `other_data/`. Shapefiles and raster files used to make plots are available [here](https://zenodo.org/record/4688451)  
## Reproduce our experiment
To reproduce the results and figures shown in Di Vittorio et al.,

1. Install `R` here - https://www.r-project.org/
2. Install `R studio` from here - https://www.rstudio.com/
3. Run the rmd file in the root directory called `Protected_area_paper.rmd` chunk by chunk to generate relevant figures. All outputs (csv and images) will be saved to the `outputs/` folder (in separate subfolders).
4. We have made all outputs available to users on the repo for convenience sake. 


<br>
<p align="center">
<a href="https://jgcri.github.io/Di_Vittorio_et_al_2022_GCB/docs/articles/Protected_area_paper.html" target="_blank"><img src="https://github.com/JGCRI/jgcricolors/blob/main/vignettes/button_metarepo.PNG?raw=true" height="60"/></a>
</p>
