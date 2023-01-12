### Code is adapted from the                 ###
###   0_download_prism.R file from           ###
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

# Daily data
# Variables: tmin, tmax, ppt, vpdmin, vpdmax
lapply(
  c("tmin", "tmax", "ppt", "vpdmin", "vpdmax"),
  function(var){
    print(var)

    # Set directory to write daily files
    prism::prism_set_dl_dir(
      paste0("Data/PRISM_Data/",var)
    )

    # Download files
    prism::get_prism_dailys(
      type = var,
      minDate = "2018-01-01",
      maxDate = "2020-03-08",
      keepZip = FALSE
    )

    if(var == "tmax") {

      # Set directory to write daily files
      prism::prism_set_dl_dir(
        paste0("Data/PRISM_Data/",var, "_historical")
      )

      # Download files
      prism::get_prism_dailys(
        type = var,
        minDate = "1987-01-01",
        maxDate = "2017-12-31",
        keepZip = FALSE
      )
    }

    cat(var, "data downloaded")
  }
)
