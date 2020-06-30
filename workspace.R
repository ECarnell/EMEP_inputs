list.of.packages <- c("sp","raster","stringr","rgeos","rgdal","dplyr","utils","ggplot2","data.table","stats","readr","readxl","sf","ncdf4")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# coordinate reference systems required #
LL <<- CRS("+init=epsg:4326") # For EMEP European data
BNG <<- CRS("+init=epsg:27700")  # NAEI data in British National Grid

# new extended domain to include both UK & Eire
uk.domain.1km <<- raster(xmn = -230000, xmx = 750000, ymn = -50000, ymx = 1300000, res = 1000, crs = BNG, vals = NA)

# This is the lat long equivalent raster of the UK domain at 1km in BNG
uk.latlon.grid <<- raster(xmn = -13.8, xmx = 4.6, ymn = 49, ymx = 61.5, res = 0.01, crs = LL, vals = NA)

print("Creating a 0.01 degree mask for UK terrestrial cells...")

# the emissions need to be masked to terrestrial cells (plus some coastal cells) - Massimo wants EMEP emissions data on the sea
# the mask is in 0.1 degree, disaggregate to 0.01 so masking can be done
emis.mask <<- crop(extend(disaggregate(raster("C:/FastProcessingSam/Git_repos/EMEP_inputs/Emissions_mask.tif"), fact=10), uk.latlon.grid), uk.latlon.grid)