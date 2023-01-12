### Code is adapted from the                 ###
###   1_weather_data.R file from             ###
###   Ariel Ortiz-Bobea's                    ###
###   R handbook available alongside his     ###
###   contribution to the "Handbook of       ###
###   Agricultural Economics"                ###
###                                          ###
### R handbook to be found here:             ###
# https://archive.ciser.cornell.edu/reproduction-packages/2856/data-and-documentation
###                                          ###
###   Ortiz-Bobea, A. (2021).                ###
###     Chapter 76 - the empirical           ###
###     analysis of climate change           ###
###     impacts and adaptation in            ###
###     agriculture. In Barrett, C. B.       ###
###     and Just, D. R., editors,            ###
###     Handbook of Agricultural             ###
###     Economics, volume 5 of Handbook      ###
###     of Agricultural Economics, pages     ###
###     3981– 4073. Elsevier.                ###

prism_dir <- "Data/PRISM_Data/"

### Create Matrix that has    ###
###   weights for population- ###
###   weighted aggregation of ###
###   weather variables to    ###
###   MSA/UA level            ###

# Create land mask
mask_temp <- raster(
  paste0(
    prism_dir,
    "tmax/PRISM_tmax_stable_4kmD2_20180101_bil/PRISM_tmax_stable_4kmD2_20180101_bil.bil"
  )
)
mask <- mask_temp
mask[] <- 0
mask[!is.na(mask_temp[])] <- 1

# Create raster with cell IDs
id <- mask
id[] <- 1:length(id)
id[mask[]==0] <- NA
names(id) <- names(mask) <- "cellid"

# MSA map
cbsa <- readxl::read_excel("Data/Census_Data/census_msa.xls", skip = 2)
msa <- cbsa %>%
  filter(
    `Metropolitan/Micropolitan Statistical Area` == "Metropolitan Statistical Area" &
      !(`FIPS State Code` %in% c("15","02","72")) &
      !is.na(`State Name`)
  )
map <- rgdal::readOGR(
  "Data/Census_Data/tl_2020_us_cbsa/tl_2020_us_cbsa.shp", 
  "tl_2020_us_cbsa"  # Source: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
)
map <- map[map@data$CBSAFP %in% unique(msa$`CBSA Code`), ] # drop all but MSAs
map <- spTransform(map, CRS(projection(mask_temp))) # make sure map has same projection as raster

# UA map
ua <- readRDS("Data/Processed_Data/ua_cbg.RDS")
map_ua <- rgdal::readOGR(
  "Data/Census_Data/tl_2021_us_uac10/tl_2021_us_uac10.shp",
  "tl_2021_us_uac10" # Source: https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
) 
map_ua <- map_ua[!is.na(map_ua@data$UATYP10),]
map_ua <- map_ua[map_ua@data$UATYP10 == "U",] # drop all but UAs
map_ua <- spTransform(map_ua, CRS(projection(mask_temp))) # make sure map has same projection as raster

# Gridded population data from 
#   Falcone, J.A., 2016,
#     U.S. block-level population density rasters for 1990, 2000, and 2010:
#     U.S. Geological Survey data release,
#     http://dx.doi.org/10.5066/F74J0C6M.
#
# Data at https://www.sciencebase.gov/catalog/item/57753ebee4b07dd077c70868
pop_raster <- raster("Data/Census_Data/pden2010_block/pden2010_60m.tif") 

# Convert to polygon of grids and match population density projection
# This grid will be needed to compute population density
# for each PRISM grid cell
grid <- rasterToPolygons(id)
grid <- spTransform(grid, CRS(projection(pop_raster)))


# Create tiles of polygons with PRISM grid cell ids

# Raster ID list: split raster into 144 tiles
# break ID raster into tiles. (it starts as 144, but trims down to 107 tiles when NA are removed)
ridlist <- SpaDES.tools::splitRaster(id, nx=12 , ny=12) 

# polygon ID list: convert these tiles of prism raster to sf polygons
pidlist <- lapply(
  1:length(ridlist), 
  function(i) { 
    r <- ridlist[[i]]
    # transform to polygons; (have to keep as na.rm false so it can loop and allow for tiles without values)
    p <- rasterToPolygons(r, na.rm=FALSE)  
    p <- spTransform(p, CRS(projection(pop_raster)))
    p <- sf::st_as_sf(p) %>% filter(cellid!="NA") # remove NA polygons
    p
  }
) # create polygon for these tiles

# Remove empty tiles
pidlist <- Filter(function(k) dim(k)[1]>0, pidlist)

saveRDS(pidlist, "Data/Processed_Data/Weather_Data/pidlist.RDS")
pidlist <- readRDS("Data/Processed_Data/Weather_Data/pidlist.RDS")

# Loop over tiles
lout <- parallel::mclapply(
  1:length(pidlist), 
  mc.cores=2, # set number of cores
  FUN=function(t) { 
    cat("tile ",t,"/",length(pidlist),sep="")
    
    # 1. Crop to tile 
    d <- crop(pop_raster,pidlist[[t]], snap="out")
    
    # 2. rasterize tile of IDs 
    idraster <- fasterize::fasterize(pidlist[[t]], d, field="cellid")
    idraster <- crop(idraster, d)
    
    # 3. Compute zonal stats using ddply 
    dat <- data.frame(id=idraster[], value=d[])
    dat <- dat[!is.na(dat$id) & !is.na(dat$value),]
    out <- plyr::ddply(
      dat, 
      "id",
      .progress="text", 
      .fun=function(df) {
        f <- sum(df$value)
      }
    )
    
    # 4. Export
    out
  }
) # tile loop
  
# Merge & save df
out <- do.call(rbind, lout)
saveRDS(out,"Data/Processed_Data/Weather_Data/out.RDS")
out <- readRDS("Data/Processed_Data/Weather_Data/out.RDS")

# Store in raster
r <- mask
r[] <- NA
rout <- lapply(
  c(1,2), 
  function(i) {
    r[out$id] <- out[,i]
    r
  }
)

rstack <- stack(rout)

# Save to disk
saveRDS(rstack, "Data/Processed_Data/Weather_Data/pop_weights.RDS")

rstack <- readRDS("Data/Processed_Data/Weather_Data/pop_weights.RDS")
all <- rstack[[2]]

temp <- stack(id,all)

# Extract MSA weights from temp
info <- raster::extract(temp, map, weights = TRUE, cellnumbers = TRUE)
names(info) <- as.character(map@data$CBSAFP)

info <- lapply(
  names(info),
  function(i) {
    print(i)
    df <- info[[paste(i)]]
    df <- as.data.frame(df)
    # Weight both by fraction covered by cell and pop density
    df$w <- df$cellid.2*df$weight/sum(df$cellid.2*df$weight,na.rm=T) 
    # drop=F, to make sure object stays as a matrix, even with 1 row
    df <- df[!is.na(df$w),, drop=F] 
    df$fips.order <- match(i, names(info))
    df
  }
)

names(info) <- as.character(map@data$CBSAFP)
info <- do.call("rbind",info)
info <- info[,c("cellid","fips.order","w")]

# Create MSA aggregating matrix P
P <- Matrix::sparseMatrix(i = info$cell,
                          j = info$fips.order,
                          x = info$w,
                          dims = c(ncell(mask_temp),
                                   nrow = length(unique(info$fips.order))))
                  
colnames(P) <- map@data$CBSAFP

saveRDS(P, "Data/Processed_Data/Weather_Data/P_popw.RDS")

# Extract UA weights from temp
info <- raster::extract(temp, map_ua, weights = TRUE, cellnumbers = TRUE)
names(info) <- as.character(map@data$UACE10)
info <- info[lengths(info)!=0]
keepnames <- names(info)
info <- lapply(
  names(info), 
  function(i) {
    print(i)
    df <- info[[paste(i)]]
    df <- as.data.frame(df)
    # Weight by pop density (forgot  aboutcell coverage,
    #   do not expect this to be a major problem)
    df$w <- df$cellid.2/sum(df$cellid.2,na.rm=T) 
    # drop=F, to make sure object stays as a matrix, even with 1 row
    df <- df[!is.na(df$w),, drop=F] 
    df$fips.order <- match(i, names(info))
    df
  }
)

names(info) <- keepnames
info <- do.call("rbind",info)
info <- info[,c("cellid","fips.order","w")]

# Create UA aggregating matrix P_ua
P_ua <- Matrix::sparseMatrix(i = info$cell,
                             j = info$fips.order,
                             x = info$w,
                             dims = c(ncell(mask_temp),
                                      nrow = length(unique(info$fips.order))))
                  
colnames(P_ua) <- keepnames

saveRDS(P_ua, "Data/Processed_Data/Weather_Data/P_ua_popw.RDS")

### Bin weather variables and then aggregate to MSA/UA
P <- readRDS("Data/Processed_Data/Weather_Data/P_popw.RDS")
P_ua <- readRDS("Data/Processed_Data/Weather_Data/P_ua_popw.RDS")

var <- c("ppt", "tmax", "tavg", "hi", "hwi_binary")

lapply(
  var, 
  function(v) { # v <- "tavg"
    
    # Set bins
    bins_seq <- if (v == "tmax") seq(0, 35, by = 5) else 
      if (v == "tavg") seq(-5, 30, by = 5)  else 
        if (v == "hi") seq(0, 35, by = 5)   else 
          if (v == "ppt") c(1e-100, seq(40, 280, by = 40))  else 
            if (grepl("hwi", v)) 1:7
    
    # Function that covers all values on a day's raster
    bins <- function(r) c(-1000000, bins_seq, 1000000) 
    
    # Get all file names
    if(v %in% c("tmax", "ppt")) {
      flist <- list.files(
        paste0(prism_dir, v),
        full.names = TRUE,
        recursive = TRUE
      )
      # PRISM raw data is .bil
      flist <- flist[extension(flist) == ".bil"]
    } else if(v %in% c("tavg", "hi", "hwi_binary")) {
      flist <- list.files(
        paste0("Data/Processed_Data/Weather_Data/", v),
        full.names = TRUE,
        recursive = TRUE
      )
      # Processed data is .gri
      flist <- flist[extension(flist) == ".gri"] 
    } 
    flist <- sort(flist)
    
    # Aggregate day by day to MSA/UA
    dlist <- lapply(
      flist, 
      function(f) {
        
        r <- raster(f)
        
        # Cut raster into indicators
        #   for each interval
        b <- bins(r)
        r <- cut(
          r, 
          b, 
          include.lowest = TRUE, 
          right = FALSE
        )
        rlist <- lapply(
          seq_along(b[-1]),
          function(i) r == i
        )
        s <- do.call(stack, rlist) 
        
        # Convert to matrix with one
        #   column per bin
        G <- do.call(
          cbind, 
          lapply(
            seq_along(b[-1]),
            function(i) as.numeric(s[[i]][])
          )
        )
        
        # Aggregate to MSA level
        A <- t(P) %*% G
        fips <- rownames(A)
        A <- as.matrix(A)
        colnames(A) <- sapply(seq_along(b[-1]), function(i) paste0(v, "bin_", i))
        
        # Aggregate to UA level
        A_ua <- t(P_ua) %*% G
        fips_ua <- rownames(A_ua)
        A_ua <- as.matrix(A_ua)
        colnames(A_ua) <- sapply(seq_along(b[-1]), function(i) paste0(v, "bin_", i))
        
        # Arrange for export
        if(v %in% c("tmax", "ppt")) {
          ymd <- rev(strsplit(f,"/")[[1]])[1]
          ymd <- rev(strsplit(ymd,"_")[[1]])[2]
        } else if(v %in% c("tavg", "hi")) {
          ymd <- rev(strsplit(f,"/")[[1]])[2]
        } else if(grepl("hwi", v)) {
          ymd <- rev(strsplit(f,"/")[[1]])[1]
          ymd <- rev(strsplit(ymd,"_")[[1]])[1]
        }
        year <- as.numeric(substr(ymd,1,4))
        month <- as.numeric(substr(ymd,5,6))
        day <- as.numeric(substr(ymd,7,8))
        cat("\r", year*10000+month*100+day)
        
        list(
          "MSA" = data.frame(fips=fips, year=year, month=month, day = day, A),
          "UA" = data.frame(fips=fips_ua, year=year, month=month, day = day, A_ua)
        )
      }
    )
    
    # Combine aggregated data from all files
    d <- lapply(dlist, function(l) l[[1]])
    d <- rbindlist(d)
    d <- d[order(d$year,d$month, d$day, d$fips),]
    d <- mutate(d, date = as.IDate(paste(year,month,day,sep="-")))
    
    d_ua <- lapply(dlist, function(l) l[[2]])
    d_ua <- rbindlist(d_ua)
    d_ua <- d_ua[order(d_ua$year,d_ua$month, d_ua$day, d_ua$fips),]
    d_ua <- mutate(d_ua, date = as.IDate(paste(year,month,day,sep="-")))
    
    # Write to disk
    fname <- paste0("Data/Processed_Data/Weather_Data/prism_msa_popw_",v,"bin.fst")
    write_fst(d, fname)
    
    fname <- paste0("Data/Processed_Data/Weather_Data/prism_ua_popw_",v,"bin.fst")
    write_fst(d_ua, fname)
    
    cat("\r", v, "\bbin complete\n")
  }
)

# Sometimes we need non-binned 
#   MSA/UA-level weather vars
#   Aggregate them too
P <- readRDS("Data/Processed_Data/Weather_Data/P_popw.RDS")
P_ua <- readRDS("Data/Processed_Data/Weather_Data/P_ua_popw.RDS")

var <- c("tmax", "ppt", "hwi_binary", "tmin", "hi", "tavg")

lapply(
  var,
  function(v){
    
    # Get all file names
    if(v %in% c("tmax", "ppt", "tmin")) {
      flist <- list.files(
        paste0(prism_dir, v),
        full.names = TRUE,
        recursive = TRUE
      )
      # PRISM raw data is .bil
      flist <- flist[extension(flist) == ".bil"]
    } else if(v %in% c("tavg", "hi", "hwi_binary")) {
      flist <- list.files(
        paste0("Data/Processed_Data/Weather_Data/", v),
        full.names = TRUE,
        recursive = TRUE
      )
      # Processed data is .gri
      flist <- flist[extension(flist) == ".gri"] 
    } 
    flist <- sort(flist)
    
    d <- lapply(
      flist, 
      function(f) { 
        # Read data to a matrix
        r <- raster(f)
        G <- r[]
        
        # Aggregate to MSA level
        A <- t(P) %*% G
        fips <- rownames(A)
        A <- as.matrix(A)
        colnames(A) <- v
        
        # Aggregate to UA level
        A_ua <- t(P_ua) %*% G
        fips_ua <- rownames(A_ua)
        A_ua <- as.matrix(A_ua)
        colnames(A_ua) <- v
        
        # Arrange for export
        if(v %in% c("tmax", "ppt")) {
          ymd <- rev(strsplit(f,"/")[[1]])[1]
          ymd <- rev(strsplit(ymd,"_")[[1]])[2]
        } else if(v %in% c("tavg", "hi")) {
          ymd <- rev(strsplit(f,"/")[[1]])[2]
        } else if(grepl("hwi", v)) {
          ymd <- rev(strsplit(f,"/")[[1]])[1]
          ymd <- rev(strsplit(ymd,"_")[[1]])[1]
        }
        year <- as.numeric(substr(ymd,1,4))
        month <- as.numeric(substr(ymd,5,6))
        day <- as.numeric(substr(ymd,7,8))
        cat("\r", year*10000+month*100+day)
        
        list(
          "MSA" = data.frame(fips=fips, year=year, month=month, day = day, A),
          "UA" = data.frame(fips=fips_ua, year=year, month=month, day = day, A_ua)
        )
      }
    )
    
    # Combine aggregated data from all files
    d <- lapply(dlist, function(l) l[[1]])
    d <- rbindlist(d)
    d <- d[order(d$year,d$month, d$day, d$fips),]
    d <- mutate(d, date = as.IDate(paste(year,month,day,sep="-")))
    
    d_ua <- lapply(dlist, function(l) l[[2]])
    d_ua <- rbindlist(d_ua)
    d_ua <- d_ua[order(d_ua$year,d_ua$month, d_ua$day, d_ua$fips),]
    d_ua <- mutate(d_ua, date = as.IDate(paste(year,month,day,sep="-")))
    
    # Write to disk
    fname <- paste0("Data/Processed_Data/Weather_Data/prism_msa_popw_",v,".fst")
    write_fst(d, fname)
    
    fname <- paste0("Data/Processed_Data/Weather_Data/prism_ua_popw_",v,".fst")
    write_fst(d_ua, fname)
    
    cat("\r", v, "complete\n")
  }
)