list.of.packages <- c("sp","raster","stringr","rgeos","rgdal","dplyr","utils","ggplot2","data.table","stats","readr","readxl","sf","ncdf4")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)



