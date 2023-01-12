# Creat daily average temperature
#   and daily apparent temperature
#   (heat index) grids

flist_max <- list.files(
  "Data/PRISM_Data/tmax", 
  full.names = TRUE,
  recursive = TRUE
)

flist_min <- list.files(
  "Data/PRISM_Data/tmin", 
  full.names = TRUE,
  recursive = TRUE
)

flist_vpd <- list.files(
  "Data/PRISM_Data/vpdmax",
  full.names = TRUE, 
  recursive = TRUE
)

flistlist <- lapply(
  list(flist_max, flist_min, flist_vpd),
  function(flist) flist[extension(flist) == ".bil"]
)

flistlist <- lapply(flistlist, sort)

names(flistlist) <- c("max", "min", "vpd")

lapply(
  seq_along(flistlist[["max"]]),
  function(f) {
    s <- stack(lapply(flistlist, function(flist) flist[f]))
    # Average temperature
    r_avg <- (s[["max"]] + s[["min"]])/2
    # Heat index
    s <- stack(
      s,
      calc(
        s,
        function(r) {
          100*min(
            max( # Estimate relative humidity with Tetens' formula
              (1 - r[3] / (6.1078 * exp(r[1] / (r[1] + 238.3) * 17.2694))),
              0
            ),
            1
          )
        }
      )
    )
    r_hi <- calc(
      s, 
      function(x)
        weathermetrics::heat.index(
          x[1], 
          rh = x[4], 
          temperature.metric = "celsius", 
          round = 2
        )
    )
    # Get the date
    d <-  rev(str_split(flistlist[[1]][f], "_")[[1]])[2]
    writeRaster(r_avg, "Data/Processed_Data/Weather_Data/tavg", overwrite = TRUE)
    writeRaster(r_hi, "Data/Processed_Data/Weather_Data/hi", overwrite = TRUE)
    cat("\r", "Average temperature and heat index:", as.character(d))
  }
)
cat("\n")

#### Create heat index à la Miller

tmax_wd <- "Data/PRISM_Data/tmax_historical/"
nyears <- 30

# List raster files with daily max temperature
f <- list.files(tmax_wd, recursive = TRUE)
f <- f[extension(f) == ".bil"]
f <- sort(f)

doy <- lubridate::as_date("2018-01-01") + seq(0,364)

# For every day of year, compute the
#   sum of maximum temperatures over
#   the last 30 years and the sum of
#   squared maximum temperatures
#   We use these to calulate mean and
#   standard deviation.
S <- lapply(
  doy, 
  function(d) {
    file <- grep(
      format(d-lubridate::years(1), "%Y%m%d"),
      f,
      value = TRUE
    )
    tmax <- raster(paste0(tmax_wd, file))
    s <- tmax
    s2 <- tmax^2
    for(y in 2:nyears) { 
      d_ <- d - lubridate::years(y)
      file <- grep(
        format(d_, "%Y%m%d"),
        f,
        value = TRUE
      )
      tmax <- raster(paste0(tmax_wd, file))
      s <- s + tmax
      s2 <- s2 + tmax^2
    }
    S <- stack(s,s2)
    writeRaster(
      S, 
      paste0(
        "Data/Processed_Data/Weather_Data/tmax_S/", 
        format(d, "%Y%m%d"), "_S30"
      ),
      overwrite = TRUE
    )
    cat("\r", "Heat wave index threshold sums:", as.character(d))
  }
)
cat("\n")

# Now, compute summary statistics.
#   We need these for the abnormal
#   temperature threshold
smmry <- lapply(
  doy, 
  function(d) {
    ddates <- d + seq(-15,15)
    lubridate::year(ddates) <- 2018
    s <- stack(
      paste0(
        "Data/Processed_Data/Weather_Data/tmax_S/", 
        format(d, "%Y%m%d"),
        "_S30.grd"
      )
    )
    s[[1]] <- 0
    s[[2]] <- 0
    for(dt in seq_along(ddates)) { 
      r <- stack(
        x = paste0(
          "Data/Processed_Data/Weather_Data/tmax_S/",
          format(ddates[dt], "%Y%m%d"),
          "_S30.grd"
        )
      )
      s[[1]] <- r[[1]] + s[[1]]
      s[[2]] <- r[[2]] + s[[2]]
    }
    n <- nyears*31
    s[[2]] <- sqrt(s[[2]]/(n-1)-s[[1]]^2/((n-1)*n))
    s[[1]] <- s[[1]]/n
    writeRaster(
      s,
      paste0(
        "Data/Processed_Data/Weather_Data/tmax_S/tmax_S30_",
        format(d, "%Y%m%d"),
        "_summary"
      )
    )
    cat("\r", "Heat wave index threshold mean and SD:", as.character(d), sep = "")
  }
)
cat("\n")

f <- list.files("Data/Processed_Data/Weather_Data/tmax_S", full.names = T, recursive = T)
f <- f[grepl("S30_",f)]
f <- f[extension(f) == ".grd"]
s <- stack(f)
# s_1 has historical means 
#  s_2 has historical SDs
s_1 <- dropLayer(s, grep("layer.2", names(s), value = TRUE))
s_2 <- dropLayer(s, grep("layer.1", names(s), value = TRUE))

# Top 90 hottest historical
#   days criterion as logical
#   raster object- Takes long
#   to compute and can be 
#   loaded without re-computing
#   below. ||
#          ||
s_rk <- calc(
  s_1,#    ||
  function(x) 
    rank(x, na.last = "keep", ties.method = "max") >= 90
)#         ||
s_rk[s_rk==0] <- NA
#          ||
#          ||
writeRaster(s_rk, "Data/Processed_Data/Weather_Data/s30_rk")
#          \/
s_rk <- stack("Data/Processed_Data/Weather_Data/s30_rk")

### heat index location-date specific threshold

tmax_wd <- "Data/PRISM_Data/tmax/"
f <- list.files(tmax_wd, recursive = TRUE)
f <- f[extension(f) == ".bil"]
f <- sort(f)
dates <- seq(lubridate::ymd("2018-01-01"), lubridate::ymd("2020-03-08"), by = "days")
# Raster template
r <- raster(paste0(tmax_wd,  grep(format(dates[1], "%Y%m%d"), f, value = TRUE)))
us_r <- r
us_r[!is.na(us_r)] <- 1
# Set HI on first day to zero
#   I compute heat waves in
#   cumulative temperature 
#   above threshold (day_hwi)
#   and in consecutive days
#   above threshold (day_hwi_binary).
#   I only present the latter in
#   the written part of the thesis.
day_hwi <- us_r*0       
day_hwi_binary <- us_r*0

for(d in seq_along(dates)){ 
  
  # extract specific day from raster stack and code cells below heat threshold to NA
  tmp  <- raster(paste0(tmax_wd, grep(format(dates[d], "%Y%m%d"), f, value = TRUE)))
  tmp  <- tmp * s_rk[[yday(dates[d])]]
  T_d <- s_1[[yday(dates[d])]] + 1.5*s_2[[yday(dates[d])]]
  tmp[tmp < T_d] <- NA
  tmp_binary <- tmp
  
  # subtract heat threshold from temperatures above threshold 
  if(any(!is.na(tmp[]))) {
    tmp[!is.na(tmp)]  <- tmp[!is.na(tmp)] - T_d[!is.na(tmp)]
    tmp_binary[!is.na(tmp_binary)] <- 1
  } #h+
  
  # add HI value of last day to HI of current day
  tmp <- day_hwi[[d]] + tmp
  tmp_binary <- day_hwi_binary[[d]] + tmp_binary
  
  # recode NA values to 0
  tmp[is.na(tmp)] <- 0
  tmp_binary[is.na(tmp_binary)] <- 0
  
  # recode non-land pixels to NA
  tmp <- tmp*us_r
  tmp_binary <- tmp_binary*us_r
  
  # save HI raster in day_hwi
  day_hwi <- stack(day_hwi, tmp)
  day_hwi_binary <- stack(day_hwi_binary, tmp_binary)
  
  cat("\r", "Heat wave index:", as.character(dates[d]), sep = "")
}

day_hwi <- dropLayer(day_hwi, 1)
day_hwi_binary <- dropLayer(day_hwi_binary, 1)

# add dates to the raster stack and save
filenames <- sapply(f, function(x) { 
  rev(strsplit(x, "/")[[1]])[1]
})

names(day_hwi)    <- filenames
names(day_hwi_binary)    <- filenames

lapply(seq_len(dim(day_hwi_binary)[3]), function(i) { 
  writeRaster(
    day_hwi[[i]],
    paste0(
      "Data/Processed_Data/Weather_Data/hwi_temp/hwi_ld30_",
      rev(strsplit(filenames[i], "[_]")[[1]])[2],
      ".grd"
    ),
    overwrite = TRUE
  )
  writeRaster(
    day_hwi_binary[[i]],
    paste0(
      "Data/Processed_Data/Weather_Data/hwi_binary/hwi_binary_ld30_",
      rev(strsplit(filenames[i], "[_]")[[1]])[2],
      ".grd"
    ),
    overwrite = TRUE
  )
})
