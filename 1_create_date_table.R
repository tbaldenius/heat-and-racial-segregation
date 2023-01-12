### Create dataset that shows which weeks      ###
###  are contained in which parts of the data  ###

### PATTERNS DATA                              ###

# List files and remove all other than patterns data
flist <- list.files("Data/SG_Patterns")
flist <- flist[extension(flist) == ".gz" & !grepl("safegraph", flist)]
flist <- sort(flist)

sg_date <- lapply(
  flist,
  function(f) {
    # Counter (optional)
    cat("\r", "Reading ", f, "...")
    # Pull unique dates from part i
    sg_date_new <- unique(
      fread(
        paste0("Data/SG_Patterns/", f),
        select = c("date_range_start"),
        showProgress = FALSE
      )[, date_range_start]
    )
    # Create table with one observation for each unique date
    i <- strsplit(f, "\\.")
    i <- substr(i[[1]][1], start = 5, stop = 99)
    data.table(
      part = rep(i, length(sg_date_new)),
      date = sg_date_new
    )
  }
)
  
sg_date <- rbindlist(sg_date)

write_fst(sg_date, "Data/Processed_Data/date_table.fst")

### HOME PANEL DATA                           ###

# List files and remove all other than patterns data
flist <- list.files("Data/SafeGraph_Data/Patterns")
flist <- flist[extension(flist) == ".gz" & grepl("home", flist)]
flist <- sort(flist)

# Get unique dates from Patterns data,
#  because dates are fuzzy in home_panel_summary data
sg_date <- unique(
  read.fst(
    "Data/Processed_Data/date_table.fst",
    as.data.table = TRUE,
    columns = "date"
  )
)[, date]

sg_date_home <- lapply(
  flist,
  function(f) {
    # Counter (optional)
    cat("\r", "Reading ", f, "...")
    
    # Pull unique dates from part i,
    #  match to closest sg_date date
    #  and keep only unique dates
    sg_date_new <- unique(
      sg_date[
        sapply(
          unique(
            fread(
              paste0("Data/SG_Patterns/", f),
              select = c("date_range_start"),
              showProgress = FALSE
            )[, as.IDate(date_range_start)]
          ),
          function(x) which.min(abs(sg_date - x))
        )
      ]
    )
    
    # Create table with one observation for each unique date
    i <- strsplit(f, "\\.")
    i <- substr(i[[1]][1], start = 34, stop = 99)
    data.table(
      part = rep(i, length(sg_date_new)),
      date = sg_date_new
    )
  }
)

sg_date_home <- rbindlist(sg_date_home)

write_fst(sg_date_home, "Data/Processed_Data/date_table_home.fst")
