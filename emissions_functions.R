# FUNCTIONS FOR CREATION ON EMEP INPUT FILES

check.netcdf.status <- function(years, pollutants, mapping.yr, region){
  
  all.years.polls <- as.data.table(expand.grid(years, pollutants))
  names(all.years.polls) <- c("years", "pollutants")
  
  ## function takes output from the combination function above
  ## it checks for netcdfs of the years and if those pollutants/ghgs are already in them
  ## if the year doesnt exist, or pollutants/ghgs arent in them, no action
  ## if the pollutant/ghg exists, asks a question and then subsets the data (or not) based on the response to overwrite or not
  
  # cycle through every year given, checking all netcdfs
  for(year in years){
    
    #message.vec <- NULL
    
    nc.filename <- paste0("N:/dump/EMEP4UK_UKems_",year,"_0.01.nc")
    
    if(file.exists(nc.filename)){
      
      nc <- nc_open(nc.filename, write = T)
      
      # now check which polluants/GHGs exist within the netcdf and ask whether to overwrite those that do
      already.in.netcdf <- pollutants[pollutants %in% names(nc$var)]
      not.in.netcdf <- pollutants[!(pollutants %in% names(nc$var))]
      
      m1 <- paste0("NetCDF already exists for ", year,":\n")
      
      #if(length(not.in.netcdf) == 0){
      #  #m2 <- paste0("    ALL proposed pollutants/GHGs already present.\n")
      #  m2 <- NULL
      #}else{
      #  m2 <- paste0("    Proposed pollutants/GHGs NOT present: ", not.in.netcdf,"\n")
      #}
      
      m2 <- paste0("    Proposed pollutants/GHGs ALREADY present: ",already.in.netcdf,"\n")
      
      print(cat(m1,m2))
      
      # ask the question whether you want to 
      x <- readline(paste0("REMOVE ", already.in.netcdf," data from processing (r) or OVERWRITE it in the netcdf (o)?")) 
      
      if(x == "r"){
        
        all.years.polls <- all.years.polls[!(years==year & pollutants %in% already.in.netcdf)]
        
        }else{}
      
      
      nc_close(nc)
      
      
    }else{
      
      print(paste0(Sys.time()," No netCDF file exists for ", year, " - NO subset needed."))
      
    } # end of if else to check pollutants
    
  } # end of year loop
  
  print(paste0(Sys.time()," Years and pollutants subsetted (if needed) and returned."))
  return(all.years.polls)
  
} # func

##############

combine.all.region.data <- function(yr.poll.subset, mapping.yr, class, region){
  
  if(class == "SNAP"){
    sectors.to.input <- paste0("S", 1:11)
    res.crs <- "1km_BNG"}else{
    sectors.to.input <- c("A_PublicPower","B_Industry","C_OtherStatComb","D_Fugitive","E_Solvents","F_RoadTransport","G_Shipping","H_Aviation","I_Offroad","J_Waste","K_AgriLivestock","L_AgriOther","N_Natural")
    res.crs <- "0.01_LL"
  }
  
  ### MASTER TABLE FOR COMBINING ###
  # this table allows re-direction of data into different sectors in EMEP4UK (e.g. for injection heights)
  # 03/07/20: currently only for class = GNFR, need to add class = SNAP if wanted
  #master.input.lookup <- fread(paste0("./lookups/EMEP4UK_sectoral_change_",class,"_lookup.csv"))
  
  all.data.list <- list()
  
  # cycle through every row in the subsetted data frame
  for(i in 1:nrow(yr.poll.subset)){
    
    
    year <- yr.poll.subset[i, years]
    species <- yr.poll.subset[i, as.character(pollutants)]
  
  
      print(paste0(Sys.time(),": Combining point and diffuse data for ",species," ",year," in ",region," as ",class," codes..."))
      
      # function to combine the point and diffuse data for the given region and classification
      
      # all diffuse data
      #all.diff.files <- list.files(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class), pattern = paste0(species,"_diff_",year,"_",region,".*",mapping.yr,"NAEImap.tif$"), full.names = T)
      
      temp.st <- stack()
      
      for(sec.code in sectors.to.input){
        
        ### FOR SECTOR A_PublicPower, ONLY use power station point data
        ### FOR SECTOR B_Industry, TAKE A_PublicPower diffuse surface plus non-power station point data
        ### FOR all else, use specific surface and point data
        # currently only intiated for GNFR sectors
        if(sec.code == "A_PublicPower"){
          
          diff.sec <- uk.latlon.grid
          
          # Point data - needs subsetting to SNAP (UK) and GNFR (Eire) and rasterizing
          pts <- fread(paste0("./Emissions_grids_plain/LL/",species,"/point/",year,"/",species,"_pt_",year,"_",region,"_",class,"_t_LL.csv"))
          # subset points
          pts.sub <- pts[get(class) == sec.code & pow.flag == 1]
          
          # if there are no points, use a blank raster, otherwise rasterize
          if(nrow(pts.sub) == 0){
            
            pt.sec <- uk.latlon.grid
            
          }else{
            
            pt.sec <- rasterize(x = pts.sub[,1:2], y = uk.latlon.grid, field = pts.sub[,Emission], fun = 'sum', background=NA)
            
          } # end of ifelse for points
          
        } else if (sec.code == "B_Industry"){
          
          # Diffuse data for Industry AND power
          diff.secA <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_",region,"_",class,"_A_PublicPower_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
          diff.secB <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_",region,"_",class,"_B_Industry_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
          
          diff.sec <- calc(stack(diff.secA, diff.secB), sum, na.rm=T)
          
          # Point data - needs subsetting to SNAP (UK) and GNFR (Eire) and rasterizing
          pts <- fread(paste0("./Emissions_grids_plain/LL/",species,"/point/",year,"/",species,"_pt_",year,"_",region,"_",class,"_t_LL.csv"))
          # subset points
          pts.sub <- pts[get(class) == "B_Industry" | get(class) == "A_PublicPower" & is.na(pow.flag)]
          
          
          # if there are no points, use a blank raster, otherwise rasterize
          if(nrow(pts.sub) == 0){
            
            pt.sec <- uk.latlon.grid
            
          }else{
            
            pt.sec <- rasterize(x = pts.sub[,1:2], y = uk.latlon.grid, field = pts.sub[,Emission], fun = 'sum', background=NA)
            
          } # end of ifelse for points
          
          
          
        }else{
          
          # Diffuse data
          diff.sec <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_",region,"_",class,"_",sec.code,"_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
          
          # Point data - needs subsetting to SNAP (UK) and GNFR (Eire) and rasterizing
          pts <- fread(paste0("./Emissions_grids_plain/LL/",species,"/point/",year,"/",species,"_pt_",year,"_",region,"_",class,"_t_LL.csv"))
          # subset points
          pts.sub <- pts[get(class) == sec.code]
          
          
          # if there are no points, use a blank raster, otherwise rasterize
          if(nrow(pts.sub) == 0){
            
            pt.sec <- uk.latlon.grid
            
          }else{
            
            pt.sec <- rasterize(x = pts.sub[,1:2], y = uk.latlon.grid, field = pts.sub[,Emission], fun = 'sum', background=NA)
            
          } # end of ifelse for points
          
        }
        
        ########
       
        # Combine the data
        
        all.sec <- calc(stack(diff.sec, pt.sec), sum, na.rm = T)
        
        names(all.sec) <- paste0(species,"_",year,"_",sec.code)
        
        temp.st <- stack(temp.st, all.sec)
      
  
    } # end of SNAP to GNFR combining loop
      
      
      # add the data to the master list
      all.data.list[[paste0(species,"_",year)]] <- temp.st
   
   
  
  } # end of row loop
    
  return(all.data.list)
  print(paste0(Sys.time(),": Combining Complete."))
  
}

##############


input.to.netcdf <- function(combined.data, years, pollutants, mapping.yr, class, region, yr.poll.subset){
  
  
  # cycle through every year given, inserting all pollutants into one file for that year
  for(year in years){
    
    ##### FIRST CHECK WHETHER;
    # 1. the NETCDF for the year already exists, if it doesnt carry on
    # 2. if it exists for the year, whether the pollutant/GHG is already in it
    
    nc.filename <- paste0("N:/dump/EMEP4UK_UKems_",year,"_0.01.nc")
    
    if(file.exists(nc.filename)){
      
      print(paste0("NetCDF already exists for ", year,", adding and replacing pollutants..."))
      
      ## UPDATE EXISTING NETCDF
      
      ## Set up the dimensions: latlong, time, sectors
      
      # generate lons, lats and set time
      
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
      variables.to.make <- yr.poll.subset[years == year, as.character(pollutants)]
      
      ## Set up the variables
      # for each variable in the given year, create a new netcdf var and extract the data as an array
      
      var.list <- list()
      
      for(v in 1:length(variables.to.make)){
        
        fillvalue <- -9999
        total.ems <- paste0(round(sum(cellStats(combined.data[[paste0(v,"_",year)]], sum))/1000,1)," kt")
        
        assign(paste0(variables.to.make[v],"_vardef"), ncvar_def(name = paste0(variables.to.make[v]), 
                                                                 units = "Mg cell-1 yr-1", 
                                                                 dim = list(dimlon,dimlat,dimtime,dimsecs), 
                                                                 missval = fillvalue,
                                                                 longname = paste0(variables.to.make[v]," emissions for UK terrestrial domain in ", year, " = ", total.ems),
                                                                 prec = "float",
                                                                 compression=4))
        
        var.list[[paste0(variables.to.make[v],"_vardef")]] <- get(paste0(variables.to.make[v],"_vardef"))
        
        
      }
      
      ## Open the netcdf
      nccurrent <- nc_open(nc.filename, write = T)
      
      # now extract the data from the reclassified list and insert
      for(v in variables.to.make){
        
        # as this netcdf exists - ADD the variable
        nccurrent <- ncvar_add( nccurrent, var.list[[paste0(v,"_vardef")]] )
        
        # extract the year and pollutant and put in
        a <- array(flip(combined.data[[paste0(v,"_",year)]],2), dim = c(nlon, nlat, ntime, nsecs ))
        
        ncvar_put(nccurrent, get(paste0(v,"_vardef")), a)
        
      }
      
      nc_close(nccurrent)
      
      
    }else{
      
      print(paste0("No NetCDF exists for ", year,", creating new .nc file..."))
      
      # new netcdf file
      filename = paste0("N:/dump/EMEP4UK_UKems_",year,"_0.01.nc")
      
      ## Set up the dimensions: latlong, time, sectors

      # generate lons, lats and set time
      
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
      variables.to.make <- yr.poll.subset[years == year, as.character(pollutants)]
      
      ## Set up the variables
      # for each variable in the given year, create a new netcdf var and extract the data as an array
      
      var.list <- list()
      
      for(v in 1:length(variables.to.make)){
        
        fillvalue <- -9999
        total.ems <- paste0(round(sum(cellStats(combined.data[[paste0(variables.to.make[v],"_",year)]], sum))/1000,1)," kt")
        
        assign(paste0(variables.to.make[v],"_vardef"), ncvar_def(name = paste0(variables.to.make[v]), 
                                                       units = "Mg cell-1 yr-1", 
                                                       dim = list(dimlon,dimlat,dimtime,dimsecs), 
                                                       missval = fillvalue,
                                                       longname = paste0(variables.to.make[v]," emissions for UK terrestrial domain in ", year, " = ", total.ems),
                                                       prec = "float",
                                                       compression=4))
        
        var.list[[paste0(variables.to.make[v],"_vardef")]] <- get(paste0(variables.to.make[v],"_vardef"))
      
          
      }
      
      ## Create the new netcdf
      ncnew <- nc_create(filename, var.list)
      
      # now extract the data from the reclassified list and insert
      for(v in variables.to.make){
        
        print(paste0("     Inserting ",v," data..."))
        
        
        # extract the year and pollutant and put in
        a <- array(flip(combined.data[[paste0(v,"_",year)]],2), dim = c(nlon, nlat, ntime, nsecs ))
        
        ncvar_put(ncnew, get(paste0(v,"_vardef")), a)
        
      }
      
      
      ## A couple of extra dimension attributes for the sectors
      #ncatt_put(ncnew,"sectors","GNFR names","1: A_PublicPower; 2: B_Industry; 3: C_OtherStationaryComb; 4: D_Fugitive; 5: E_Solvents; 6: F_RoadTransport; 7: G_Shipping; 8: H_Aviation; 9: I_Offroad; 10: J_Waste; 11: K_AgriLivestock; 12: L_AgriOther; 13: N_Natural")
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
      
      ncatt_put(ncnew, 0, "sec01", "publicpower", prec="char")
      ncatt_put(ncnew, 0, "sec02", "industry", prec="char")
      ncatt_put(ncnew, 0, "sec03", "otherstationarycomb", prec="char")
      ncatt_put(ncnew, 0, "sec04", "fugitive", prec="char")
      ncatt_put(ncnew, 0, "sec05", "solvents", prec="char")
      ncatt_put(ncnew, 0, "sec06", "roadtransport", prec="char")
      ncatt_put(ncnew, 0, "sec07", "shipping", prec="char")
      ncatt_put(ncnew, 0, "sec08", "aviation", prec="char")
      ncatt_put(ncnew, 0, "sec09", "offroad", prec="char")
      ncatt_put(ncnew, 0, "sec10", "waste", prec="char")
      ncatt_put(ncnew, 0, "sec11", "agrilivestock", prec="char")
      ncatt_put(ncnew, 0, "sec12", "agriother", prec="char")
      ncatt_put(ncnew, 0, "sec13", "other", prec="char")
      
  
      nc_close(ncnew)
      
  
      
    } # end of ifelse loop to see whether data exists
    
    
  } # end of year loop
  
  
} # func



