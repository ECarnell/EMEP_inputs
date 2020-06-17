# FUNCTIONS FOR CREATION ON EMEP INPUT FILES

NAEI.LL.to.GNFR <- function(years, pollutants, uk.latlon.grid, mapping.yr){
  
  all.data.list <- list()
  
  # cycle through every year given, inserting all pollutants into one file for that year
  for(year in years){
    
    # for the year, cycle through every given GHG/pollutant
    for(species in pollutants){
  
      print(paste0(Sys.time(),": Converting SNAP codes to GNFR sectors for ",species," in ",year,"..."))
      
      
  # function to combine the point and diffuse data from NAEI and reclassify to GNFR
  
  # reclassification of SNAP to GNFR is not strictly possible so come up with rough method
  # this table ensure only ONE SNAP sector goes into a GNFR to avoid doubling. 
  SNAP.GNFR <- data.table(SNAP = c(1,3,4,2,5,6,7,NA,8,NA,9,10,NA,11), 
                          GNFR = c("A_PublicPower","B_Industry","B_Industry","C_OtherStationaryComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural"),
                          SEC.ID = c(1,2,2,3,4,5,6,7,8,9,10,11,12,13))
  
  
  # for each GNFR code, call in the diffuse and point data from the correct SNAP
  # if the GNFR does not have a SNAP, make a blank surface
  # merge diffuse and point to one surface (EMEP4UK has all data in one input)
  
  all.SN.files <- list.files("C:/FastProcessingSam/EMEP_new_grid/", pattern = paste0(species,"_",year,"_S.*\\NAEImap.tif$"))
  
  gnfr.st <- stack()
  
  for(GN in unique(SNAP.GNFR[,GNFR])){
    
    req.snap <- SNAP.GNFR[GNFR == GN, SNAP]
    
    # create blank surface is SNAP = NA
    if(any(is.na(req.snap) ==T)){
      
      diff.SN <- uk.latlon.grid
      
    }else{
    
      ### pre-generated data 
      # Diffuse#
      req.files <- all.SN.files[grep(paste0(species,"_",year,"_S",req.snap,".*\\NAEImap.tif$", collapse = "|"), all.SN.files, perl = T)]
      
      diff.SN <- stack(paste0("C:/FastProcessingSam/EMEP_new_grid/",req.files))
      
    } # end of ifelse for diffuse
      
      # Point - needs subsetting to SNAP (UK) and GNFR (Eire) and rasterizing
      pts <- fread(paste0("//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS/Emissions_grids_plain/LL/point/",species,"/",year,"/",species,"_pt_ems_",year,"_ukeire_t_LL.csv"))
      
      pts.sub <- pts[SNAP %in% c(req.snap, GN)]
      
      if(nrow(pts.sub) == 0){
        
        pt.SN <- uk.latlon.grid
        
      }else{
        
        pt.SN <- rasterize(x = pts.sub[,1:2], y = uk.latlon.grid, field = pts.sub[,Emission], fun = 'sum', background=NA)
        
      } # end of ifelse for points
      
      # Combine the points
      
      all.SN <- calc(stack(diff.SN, pt.SN), sum, na.rm = T)
      
      names(all.SN) <- paste0(SNAP.GNFR[GNFR==GN,SEC.ID][1],"_",GN)
    
      gnfr.st <- stack(gnfr.st, all.SN)
      
  
    } # end of SNAP to GNFR combining loop
  
  
  # add the data to the master list
  all.data.list[[paste0(species,"_",year)]] <- gnfr.st
   
  
   } # end of polluatnts/GHGs loop
  
    
  
  } # end of year loop
    
  return(all.data.list)
  print(paste0(Sys.time(),": Processing Complete."))
  
}

##############

check.netcdf.status <- function(reclassified.data, years, pollutants, uk.latlon.grid, mapping.yr){
  
  # cycle through every year given, checking all netcdfs
  for(year in years){
  
    nc.filename <- paste0("C:/FastProcessingSam/EMEP_new_grid/EMEP4UK_UKems_",year,"_0.01.nc")
    
    if(file.exists(nc.filename)){
      
      nc <- nc_open(nc.filename, write = T)
      
      # now check which polluants/GHGs exist within the netcdf and ask whether to overwrite those that do
      already.in.netcdf <- pollutants[pollutants %in% names(nc$var)]
      not.in.netcdf <- pollutants[!(pollutants %in% names(nc$var))]
      
      print(paste0("NetCDF exists for ", year,", but ",not.in.netcdf," data does not; updating .nc file..."))
      
      # ask the question whether you want to 
      x <- readline(paste0(already.in.netcdf," data already exists in ",year," netCDF: do you want to quit (q) or overwrite (o)?")) 
      if(x == "q"){stop("Change data to convert to netCDF")}else{}
      
      
      nc_close(nc)
      
    }else{
      
    }
  
  
  } # end of year loop
  
  
}

##############

input.to.netcdf <- function(reclassified.data, years, pollutants, mapping.yr){
  
  # reclassification of SNAP to GNFR is not strictly possible so come up with rough method
  # this table ensure only ONE SNAP sector goes into a GNFR to avoid doubling. 
  SNAP.GNFR <- data.table(SNAP = c(1,3,4,2,5,6,7,NA,8,NA,9,10,NA,11), 
                          GNFR = c("A_PublicPower","B_Industry","B_Industry","C_OtherStationaryComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural"),
                          SEC.ID = c(1,2,2,3,4,5,6,7,8,9,10,11,12,13))
  
  # cycle through every year given, inserting all pollutants into one file for that year
  for(year in years){
    
    ##### FIRST CHECK WHETHER;
    # 1. the NETCDF for the year already exists, if it doesnt carry on
    # 2. if it exists for the year, whether the pollutant/GHG is already in it
    
    nc.filename <- paste0("C:/FastProcessingSam/EMEP_new_grid/EMEP4UK_UKems_",year,"_0.01.nc")
    
    if(file.exists(nc.filename)){
      
      nc <- nc_open(nc.filename, write = T)
      
      # now check which polluants/GHGs exist within the netcdf and ask whether to overwrite those that do
      already.in.netcdf <- pollutants[pollutants %in% names(nc$var)]
      not.in.netcdf <- pollutants[!(pollutants %in% names(nc$var))]
      
      print(paste0("NetCDF exists for ", year,", but ",not.in.netcdf," data does not; updating .nc file..."))
      
      # ask the question whether you want to 
      x <- readline(paste0(already.in.netcdf," data already exists in ",year," netCDF: do you want to quit (q) or overwrite (o)?")) 
      if(x == "q"){stop("Change data to convert to netCDF")}else{}
      
      
      nc_close(nc)
      
    }else{
      
      print(paste0("No NetCDF exists for ", year,", creating new .nc file..."))
      
      # new netcdf file
      filename = paste0("C:/FastProcessingSam/EMEP_new_grid/EMEP4UK_UKems_",year,"_0.01.nc")
      
      ## Set up the dimensions: latlong, time, sectors

      # generate lons, lats and set time
      
      extent(uk.latlon.grid)
      
      
      lonvals <- as.array(seq(xmin(uk.latlon.grid) + 0.01/2, xmax(uk.latlon.grid) - 0.01/2, 0.01))
      nlon <- length(lonvals)
      latvals <- as.array(seq(ymin(uk.latlon.grid) + 0.01/2, ymax(uk.latlon.grid) - 0.01/2, 0.01))
      nlat <- length(latvals)
      
      secvals <- 1:13 # this is the same as the table and what EMEP requires
      nsecs <- length(secvals)
      
      timevals <- 1
      ntime <- 1
      
      # create dimensions
      dimlon <- ncdim_def(name = "lon", longname= "longitude", units = "degrees", vals = lonvals)
      dimlat <- ncdim_def(name = "lat", longname= "latitude", units = "degrees", vals = latvals)
      dimtime <- ncdim_def(name = "time", longname = "years since 2017", units = "years", vals = timevals)
      dimsecs <- ncdim_def(name = "sectors", longname = "emission GNFR sectors", units = "secID", vals = secvals)
      
      # data array - colect data from list for the given year
      variables.to.make <- names(reclassified.data)[grep(year, names(reclassified.data))]
      
      ## Set up the variables
      # for each variable in the given year, create a new netcdf var and extract the data as an array
      
      var.list <- list()
      
      for(v in 1:length(variables.to.make)){
        
        fillvalue <- -9999
        
        assign(paste0(variables.to.make[v],"_vardef"), ncvar_def(name = str_split(variables.to.make[v],"_")[[1]][1], 
                                                       units = "Mg cell-1 yr-1", 
                                                       dim = list(dimlon,dimlat,dimtime,dimsecs), 
                                                       missval = fillvalue,
                                                       longname = paste0(str_split(variables.to.make[v],"_")[[1]][1]," emissions for UK terrestrial domain"),
                                                       prec = "float",
                                                       compression=4))
        
        var.list[[paste0(variables.to.make[v],"_vardef")]] <- get(paste0(variables.to.make[v],"_vardef"))
      
          
      }
      
      ## Create the new netcdf
      ncnew <- nc_create(filename, var.list)
      
      # now extract the data from the reclassified list and insert
      for(v in variables.to.make){
        
        # extract the year and pollutant and put in
        a <- array(flip(reclassified.data[[v]],2), dim = c(nlon, nlat, ntime, nsecs ))
        
        ncvar_put(ncnew, get(paste0(v,"_vardef")), a)
        
      }
      
      
      ## A couple of extra dimension attributes for the sectors
      ncatt_put(ncnew,"sectors","GNFR names","1: A_PublicPower; 2: B_Industry; 3: C_OtherStationaryComb; 4: D_Fugitive; 5: E_Solvents; 6: F_RoadTransport; 7: G_Shipping; 8: H_Aviation; 9: I_Offroad; 10: J_Waste; 11: K_AgriLivestock; 12: L_AgriOther; 13: N_Natural")
      ncatt_put(ncnew,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
      ncatt_put(ncnew,"lat","axis","Y")
      ncatt_put(ncnew,"time","axis","T")
      
      ## Finally the global attributes
      ncatt_put(ncnew, 0, "Conventions","CF-1.0", prec="char")
      ncatt_put(ncnew, 0, "projection","WGS84", prec="char")
      ncatt_put(ncnew, 0, "Grid_resolution", "0.01", prec="char")
      ncatt_put(ncnew, 0, "Original data source", "UK NAEI", prec="char")
      ncatt_put(ncnew, 0, "Category schema", "GNFR", prec="char")
      ncatt_put(ncnew, 0, "Spatial distribution year", mapping.yr, prec="int")
      ncatt_put(ncnew, 0, "Created_with", R.Version()$version.string, prec="char")
      ncatt_put(ncnew, 0, "ncdf4_version", packageDescription("ncdf4")$Version, prec="char")
      ncatt_put(ncnew, 0, "Created_by","Sam Tomlinson samtom@ceh.ac.uk", prec="char")
      ncatt_put(ncnew, 0, "Created_date", as.character(Sys.time()), prec="char")
      ncatt_put(ncnew, 0, "Sector_names", "GNFR", prec="char")
      
  
      nc_close(ncnew)
      
  
      
      
    } # end of ifelse loop to see whether data exists
    
    
    
    
    
    
    
    
    
    # for the year, cycle through every given GHG/pollutant
    for(species in pollutants){
  
  
 
  
  
  
  
    } # end of pollutant loop
    
    
  } # end of year loop
  
  
}



