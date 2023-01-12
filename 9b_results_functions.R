# Functions related to regression estimation
#   and visualization thereof

# There is a mistake in the Conley SE implementation
#   in the fixest package. For details consider my 
#   comment and previous discussion on the package's github:
#   https://github.com/lrberge/fixest/issues/177#issuecomment-1311961769
#
# I therefore use an implementation by Kyle Butts,
#   who is also a contributor to fixest
#   A version I implemented myself is much too slow,
#   this has to be done in C++ with rcpp. If have
#   only made minor adaptions to the original code.
# Source: https://github.com/kylebutts/reprex/tree/main/conley_ses
source("Code/9c_helper_conley.R")

# Merge VI index data with weather data
#   and allow for a couple of options
get_bin_data <- function(
  # Main explanatory var
  X = TMAXBIN,
  # At future point there
  #   could be more weather
  #   controls than ppt
  Z = PPTBIN, 
  # Sample split option
  #   when analyzing
  #   heterogeneity
  split = NULL, 
  # VI index data
  .indices = indices_vi, 
  # Response variable
  yvar = "VI", 
  # Population-weighted
  #   quantiles when 
  #   splitting sample
  q_weighted = TRUE
) {
  
  Xvar <- tolower(deparse(substitute(X)))
  Zvar <- tolower(deparse(substitute(Z)))
  
  if(!is.null(split)) {
    if(names(split) == "temperature") {
      # MSA or UA-based?
      if("msa" %in% names(.indices)) {
        TMAX <- read_fst("Data/Processed_Data/Weather_Data/prism_msa_popw_tmax.fst") %>%
          mutate(week = (as.numeric(date)+3)%/%7-2504)
        # Determine MSA tmax means
        groups <- TMAX %>%
          group_by(fips) %>%
          summarise(gvar = mean(tmax)) %>%
          mutate(fips = str_pad(fips, 5, "left", pad = "0")) %>%
          select(fips, gvar)
      } else {
        TMAX <- read_fst("Data/Processed_Data/Weather_Data/prism_ua_popw_tmax.fst") %>%
          mutate(week = (as.numeric(date)+3)%/%7-2504)
        # Determine UA tmax means
        groups <- TMAX %>%
          group_by(fips) %>%
          summarise(gvar = mean(tmax)) %>%
          select(fips, gvar)
      }
    } else {
      if(names(split) == "income") {
        # MSA or UA-based?
        if("msa" %in% names(.indices)) {
          msa <- read.fst("Data/Processed_Data/msa_cbg_nw.fst")
          # Determine MSA inc means
          groups <- msa %>%
            mutate(gvar = inc) %>%
            select(fips, gvar)
        } else {
          ua <- read.fst("Data/Processed_Data/ua_cbg_nw.fst")
          # Determine UA inc means
          groups <- ua %>%
            mutate(gvar = inc,
                   fips = UACE10) %>%
            select(fips, gvar)
        }
      } else {
          stop("Cannot group by this variable.")
        }
    }
    .indices <- left_join(.indices, groups, by = "fips")
    
    # Quantile-weighted/unweighted mean split
    if(!q_weighted) {
      .indices <- .indices %>%
        mutate(group = ntile(desc(gvar), split)) %>%
        select(-gvar)
    } else {
      .indices <- .indices %>%
        mutate(group = -cut(
          gvar, 
          MetricsWeighted::weighted_quantile(
            .indices$gvar, 
            w = .indices$pop, 
            probs = seq(0, 1, 1/split)
          ),
          include.lowest = TRUE,
          labels = FALSE)
        ) %>%
        select(-gvar)
    }
  }

  # Aggregate daily bins to weekly level
  X <- X %>%
    mutate(fips = str_pad(fips, 5, "left", "0")) %>%
    group_by(fips, week) %>%
    summarise(across(starts_with(Xvar), sum))
  Z <- Z %>%
    mutate(fips = str_pad(fips, 5, "left", "0")) %>%
    group_by(fips, week) %>%
    summarise(across(starts_with(Zvar), sum)) 
  
  # Merge VI and weather data
  full_data <- full_join(X, Z, by = c("week", "fips")) %>%
    inner_join(.indices, by = c("week", "fips")) %>%
    arrange(fips, week) %>%
    ungroup() 
  
  # Remeber how bins were created?
  X_bins <- if (grepl("tmax", Xvar)) seq(0, 35, by = 5) else 
    if (grepl("tavg", Xvar)) seq(-5, 30, by = 5)  else 
      if (grepl("hi", Xvar)) seq(0, 35, by = 5)   else 
        if (grepl("ppt", Xvar)) c(1e-100, seq(40, 280, by = 40))  else 
          if (grepl("hwi", Xvar)) 1:7
  
  # Return an object that saves
  #   relevant information
  list(
    data = full_data,
    vars = c(Xvar, Zvar),
    X_bins = X_bins,
    yvar = yvar,
    groups = names(split)
  )
}

# Return a list with coefficent
#   and variacne estimates
#   and some relevant additional
#   information
get_bin_coefs <- function(
  # Data must be object returned
  #   by get_bin_data
  data,
  # What bin number should
  #   serve as reference bin?
  refbin = 6,
  # Fixed effects specified
  #   as list of character
  #   vectors.
  #   Length of list is 
  #   number of different
  #   specs
  fix_ef = list(c(
    "fips",
    "week",
    "state^month^year"
  )),
  # Variance estimator
  vcov = "conley-hac",
  # Weight by population?
  weighted = TRUE,
  # Logarithmic
  #   outcome
  #   (not used
  #    eventually)
  logged = FALSE,
  # Distance cutoff
  #   if vcov is
  #   conley
  cutoff = 500,
  # Distance kernel
  #   "uniform" or
  #   "bartlett"
  kernel = "bartlett",
  # How to calculate
  #   conley variance?
  #   -> "kyle" is
  #     correct
  #   "fixest" for
  #   comparison
  se_method = "kyle",
  # Spatial distance
  #   spherical
  #   ("haversine")
  # or "triangular"
  dist_fn = "haversine",
  # Return fixest object
  #   only for inspection
  return_mod = FALSE  
) {
  
  # This function estimates
  #   coefficient with help
  #   of the fixest::feols
  #   function. It is wrapped
  #   in here so that we can 
  #   apply different 
  #   specifications easily
  bin_ols_est <- function(
    # Almost the same
    #   inputs
    data,
    # This is one individual
    #   fixed effect spec now
    fixef,
    var,
    refbin,
    vcov,
    weighted,
    yvar,
    cutoff = 500,
    kernel = "bartlett",
    se_method,
    dist_fn,
    return_mod
  ) {
    
    # Remove reference bin
    if(refbin>0) data <- data %>% 
        dplyr::select(
          !all_of(
            paste0(
              var[1],
              "_", 
              refbin
            )
          )
        )
    
    # Set weights vector
    w <- if(weighted) data$pop else 1
    
    # Get fixest object
    #   with coefficients
    mod <- feols(
      as.formula(
        paste0(
          # Respons variable
          yvar,
          " ~ ",
          # Explanatory variables
          paste(
            c(
              grep(
                var[1],
                names(data), 
                value = TRUE
              ), 
              grep(
                var[2],
                names(data),
                value = TRUE
              )
            ),
            collapse = " + "
          ),
          " | ",
          # Fixed effects
          paste(fixef, collapse = " + ")
          )
        ),
      weights = w,
      panel.id = c("fips", "week"),
      data = data,
      # Drop fixed effect estimates
      combine.quick = TRUE,
      # Return demeaned expl.
      #   variables for 
      #   variance estimation
      demeaned = TRUE
    )
    
    # If not Conley
    #   fixest::vcov is
    #   fine to use
    if(vcov != "conley-hac") {
      
      varcov <- list(
        vcov(
          mod,
          vcov = vcov, 
          ssc = ssc(
            fixef.K="full", 
            fixef.force_exact = TRUE
            )
          )
        )
      
      names(varcov) <- vcov
      
      mod <- summary(mod, vcov = varcov)
      
    } else {
      if(se_method == "fixest") {
        
        # Conley: This returns 
        #   faulty estimates
        conley <- fixest::vcov_conley(
          mod,
          cutoff = paste0(cutoff, "km"),
          distance = if(dist_fn == "haversine") "spherical" else "triangular"
        )
        
        # HAC
        hac <- fixest::vcov_NW(
          mod, unit = ~fips, time = ~week,
          lag = 3
        ) 
        
        # Heterosk.-robust
        robust <- vcov(
          mod, 
          vcov = "hetero",
          ssc = ssc(
            fixef.K="full",
            fixef.force_exact = TRUE
          )
        )
        
        mod <- summary(mod, vcov = list("Conley-HAC" = conley + hac - robust))
      } else if(se_method == "kyle") {
        
        # Coordinates matrix
        coords <- as.matrix(data[,c("latitude", "longitude")])
        
        # Kyle Conley-HAC
        sp_hac <- conley_ses(
          X = mod$X_demeaned, 
          e = t(t(mod$residuals)), 
          coords = coords,
          dist_cutoff = cutoff,
          id = t(t(as.numeric(data$fips))),
          time = t(t(data$week)),
          lag_cutoff = 3,
          kernel = kernel,
          W = w,
          dist_fn = dist_fn,
          K = Reduce('+', mod$fixef_sizes)
        )
        
        mod <- summary(mod, vcov = list("Conley-HAC" = sp_hac[["Spatial_HAC"]]))
      }
    }
    
    if(return_mod) return(mod)
    
    # Coefficients
    model_coef <- (summary(mod))$coeftable
    
    # R2 and R2 within 
    etab <- data.frame(etable(mod)[,2])
    rownames(etab) <- etable(mod)[,1]
    
    data.frame(
      coef = model_coef[,1],
      SE = model_coef[,2],
      name = rownames(model_coef),
      ec = paste(fixef, collapse = "_"),
      vcov = vcov,
      r2 = etab["R2",],
      r2within = etab["Within R2",]
    )
    
  }
  
  if(logged) data$yvar <- paste0("log(", data$yvar, ")")
  
  if(return_mod) {
    return(
      lapply(
        fix_ef,
        function(x) bin_ols_est(
          data$data,
          fixef =  unlist(x),
          var = data$vars,
          refbin = refbin,
          vcov = vcov,
          weighted = weighted,
          yvar = data$yvar,
          kernel = kernel,
          cutoff =  cutoff,
          se_method = se_method,
          dist_fn = dist_fn,
          return_mod = return_mod
        )
      )
    )
  }
  
  # Are the data split by temperature
  #   or income?
  if(!("group" %in% names(data$data))) {
    return(
      list(
        # loop over different
        #   fixed effect specs
        #   and apply bin_ols_est()
        coefs = do.call(
          rbind,
          lapply(
            fix_ef,
            function(x) bin_ols_est(
              data$data, 
              fixef =  unlist(x),
              var = data$vars, 
              refbin = refbin, 
              vcov = vcov, 
              weighted = weighted, 
              yvar = data$yvar,
              kernel = kernel,
              cutoff =  cutoff, 
              se_method = se_method, 
              dist_fn = dist_fn,
              return_mod = return_mod
            )
          )
        ),
        refbin = ifelse(refbin, paste0(data$vars[1], "_", refbin), FALSE),
        breaks = data$X_bins,
        vars = data$vars,
        data = data$data,
        fix_ef = lapply(fix_ef, unlist),
        yvar = data$yvar)
    )
  }
  
  # If we have groups loop over both
  #   groups and fixed effect specs
  list(
    coefs = do.call(
      rbind,
      lapply(
        cross2(
          unique(data$data$group), 
          fix_ef
        ), function(x) {
          ols <- bin_ols_est(
            data$data[data$data$group == x[[1]],],
            fixef =  unlist(x[[2]]),
            var = data$vars,
            refbin = refbin,
            vcov = vcov,
            weighted = weighted,
            yvar = data$yvar,
            kernel = kernel,
            cutoff =  cutoff,
            se_method = se_method,
            dist_fn = dist_fn,
            return_mod = return_mod
          )
          data.frame(ols, group = x[[1]])
        }
      )
    ),       
    refbin = ifelse(
      refbin,
      paste0(data$vars[1], "_", refbin), 
      FALSE
    ),
    breaks = data$X_bins,
    vars = data$vars,
    data = data$data,
    fix_ef = lapply(fix_ef, unlist),
    yvar = data$yvar,
    groups = data$groups
  )
}


# Function to correctly label
#   fixed effects specifications
#   when using ggplot2::ggplot()
translate_fe <- function(fixef) {
  sapply(
    fixef, 
    function(fe) {
      fe <- unlist(strsplit(fe, "_"))
      fe <- sapply(
        fe, 
        function(x) {
          x <- unlist(strsplit(x, "\\^"))
          x <- sapply(x, function(x) {
            if (x == "fips") x <- "MSA"
            substr(x, 1, 1) <- toupper(substr(x, 1, 1))
            x
          })
          if (x[length(x)] == "Year")
            x <- x[1:(length(x) - 1)]
          else
            if (x[length(x)] %in% c("Month", "Quarter"))
              x[length(x)] <- paste0("Cal.-", x[length(x)])
          else
            if (x[length(x)] == "Cweek")
              x[length(x)] <- "Cal.-Week"
          paste(x, collapse = " x ")
        }
      )
      paste(fe, collapse = "\n")
    }
  )
}

# Visualize results and save them,
#   if desired
plot_bin_bins <- function(
  # Object return by get_bin_coefs()
  model,
  save = FALSE,
  fname = "",
  # Optional, 
  #   set y-axis limits 
  coords = NULL,
  # If save = TRUE, what
  #   width and height?
  #   In inches?!
  width = 8,
  height = 3.5
) {
  
  # Get tmaxbin varnames
  vars <- grep(
    model$vars[1],
    names(model$data),
    value = TRUE
  )
  # What index position is the
  #   reference bin at?
  irefbin <-  match(model$refbin, vars)
  nbin <- length(vars)
  
  # If other yvars were implemented
  #   set the y-axis title here
  yname <- "Coefficient Estimates"
  # Divide coefficients by some
  #   value for a interpretation
  #   of relative change?
  #   Not for now.
  base_vi <- 1
  
  if (is.null(model$coefs$group)) {
    # If we have NO groups
    if (irefbin == 1) {
      # And the reference bin IS the first
      # one, then these are the relevant
      # figures for plotting
      plt_table <- do.call(
        rbind,
        # Loop over fixed effect specs
        lapply(
          model$fix_ef,
          function(f) {
            f <- paste(f, collapse = "_")
            m <- model$coefs[model$coefs$ec == f,]
            data.frame(
              coef = c(0, m[irefbin:(nbin - 1), 1]) / base_vi,
              # 95% CI lower/upper bound
              lb = c(0, m[irefbin:(nbin - 1), 1]) / base_vi - 
                1.96 * c(0, m[irefbin:(nbin - 1), 2]) / base_vi,
              ub = c(0, m[irefbin:(nbin - 1), 1]) / base_vi +
                1.96 * c(0, m[irefbin:(nbin - 1), 2]) / base_vi,
              var_names = c(model$refbin, m[irefbin:(nbin - 1), 3]),
              fixef = f,
              x = 1:nbin,
              r2 = rep(m$r2[1], nbin),
              r2within = rep(m$r2within[1], nbin)
            )
          }
        )
      ) %>%
        mutate(fixef = factor(fixef, levels = unique(fixef)))
    } else {
      # If we have NO groups
      # And the reference bin is NOT the first
      # one, then these are the relevant
      # figures for plotting
      plt_table <- do.call(
        rbind,
        lapply(
          model$fix_ef, function(f) {
            f <- paste(f, collapse = "_")
            m <- model$coefs[model$coefs$ec == f,]
            data.frame(
              coef = c(m[1:(irefbin - 1), 1], 0, m[irefbin:(nbin - 1), 1]) / base_vi,
              # 95% CI lower/upper bound
              lb = c(m[1:(irefbin - 1), 1], 0, m[irefbin:(nbin -1), 1]) / base_vi - 
                1.96 * c(m[1:(irefbin - 1), 2], 0, m[irefbin:(nbin - 1), 2]) / base_vi,
              ub = c(m[1:(irefbin - 1), 1], 0, m[irefbin:(nbin - 1), 1]) / base_vi + 
                1.96 * c(m[1:(irefbin - 1), 2], 0, m[irefbin:(nbin - 1), 2]) / base_vi,
              var_names = c(m[1:(irefbin - 1), 3], model$refbin, m[irefbin:(nbin - 1), 3]),
              fixef = f,
              x = 1:nbin,
              r2 = rep(m$r2[1], nbin),
              r2within = rep(m$r2within[1], nbin)
            )
          }
        )
      ) %>%
        mutate(fixef = factor(fixef, levels = unique(fixef)))
    }
  } else {
    # If we HAVE groups
    if (irefbin == 1) {
      # And the reference bin IS the first
      # one, then these are the relevant
      # figures for plotting
      plt_table <- do.call(
        rbind,
        lapply(
          cross2(
            unique(model$coefs$group), 
            model$fix_ef
          ),
          function(f) {
            f[[2]] <- paste(f[[2]], collapse = "_")
            m <- model$coefs[model$coefs$group == f[[1]] &
                            model$coefs$ec == f[[2]],]
            data.frame(
              coef = c(0, m[irefbin:(nbin - 1), 1]) / base_vi,
              lb = c(0, m[irefbin:(nbin - 1), 1]) / base_vi -
                1.96 * c(0, m[irefbin:(nbin - 1), 2]) / base_vi,
              ub = c(0, m[irefbin:(nbin - 1), 1]) / base_vi +
                1.96 * c(0, m[irefbin:(nbin - 1), 2]) / base_vi,
              var_names = c(model$refbin, m[irefbin:(nbin - 1), 3]),
              fixef = f[[2]],
              group = f[[1]],
              x = 1:nbin,
              r2 = rep(m$r2[1], nbin),
              r2within = rep(m$r2within[1], nbin)
            )
          }
        )
      ) %>%
        mutate(fixef = factor(fixef, levels = unique(fixef)))
    } else {
      # If we HAVE groups
      # And the reference bin is NOT the first
      # one, then these are the relevant
      # figures for plotting
      plt_table <- do.call(
        rbind,
        lapply(
          cross2(
            unique(model$coefs$group),
            model$fix_ef
          ), function(f) {
            f[[2]] <- paste(f[[2]], collapse = "_")
            m <- model$coefs[model$coefs$group == f[[1]] &
                            model$coefs$ec == f[[2]],]
            data.frame(
              coef = c(m[1:(irefbin - 1), 1], 0, m[irefbin:(nbin - 1), 1]) / base_vi,
              lb = c(m[1:(irefbin - 1), 1], 0, m[irefbin:(nbin - 1), 1]) / base_vi -
                1.96 * c(m[1:(irefbin - 1), 2], 0, m[irefbin:(nbin - 1), 2]) / base_vi,
              ub = c(m[1:(irefbin - 1), 1], 0, m[irefbin:(nbin - 1), 1]) / base_vi +
                1.96 * c(m[1:(irefbin - 1), 2], 0, m[irefbin:(nbin - 1), 2]) / base_vi,
              var_names = c(m[1:(irefbin - 1), 3], model$refbin, m[irefbin:(nbin - 1), 3]),
              fixef = f[[2]],
              group = f[[1]],
              x = 1:nbin,
              r2 = rep(m$r2[1], nbin),
              r2within = rep(m$r2within[1], nbin)
            )
          }
        )
      ) %>%
        mutate(fixef = factor(fixef, levels = unique(fixef)))
    }
  }
  
  # If we compare different vcov specs,
  #   bind the ignored plot tables to
  #   plt_table
  if (length(unique(model$coefs$vcov)) > 1) {
    for (i in seq(length(unique(model$coefs$vcov)) - 1)) {
      plt_table_ <- do.call(
        rbind,
        lapply(
          model$fix_ef, 
          function(f) {
            f <- paste(f, collapse = "_")
            m <-
              model$coefs[model$coefs$ec == f,]
            # It's ugly, but it works
            data.frame(
              coef = c(
                m[(1 + 16 * i):(irefbin - 1 + 16 * i), 1],
                0, 
                m[(irefbin + 16 *i):(nbin - 1 + 16 * i), 1]
              ) / base_vi,
              lb = c(
                m[(1 + 16 * i):(irefbin - 1 + 16 * i), 1],
                0, 
                m[(irefbin + 16 * i):(nbin - 1 + 16 * i), 1]
              ) / base_vi - 1.96 * 
                c(
                  m[(1 + 16 * i):(irefbin - 1 + 16 * i), 2],
                  0, m[(irefbin + 16 * i):(nbin - 1 + 16 * i), 2]
                ) / base_vi,
              ub = c(
                m[(1 + 16 * i):(irefbin -  1 + 16 * i), 1], 
                0,
                m[(irefbin + 16 * i):(nbin - 1 + 16 * i), 1]
              ) / base_vi + 1.96 *
                c(
                  m[(1 + 16 * i):(irefbin - 1 + 16 * i), 2], 
                  0, 
                  m[(irefbin + 16 * i):(nbin -1 + 16 * i), 2]
                ) / base_vi,
              var_names = c(
                m[(1 + 16 * i):(irefbin - 1 + 16 * i), 3],
                model$refbin, 
                m[(irefbin + 16 * i):(nbin - 1 + 16 * i), 3]
              ),
              fixef = f,
              x = 1:nbin,
              r2 = rep(m$r2[(1 + 16 * i)], nbin),
              r2within = rep(m$r2within[(1 + 16 * i)], nbin)
            )
          }
        )
      ) %>%
        mutate(fixef = factor(fixef, levels = unique(fixef)))
      plt_table <- rbind(plt_table, plt_table_)
    }
    plt_table <- plt_table %>%
      mutate(
        vcov = ordered(
          rep(unique(model$coefs$vcov), each = 9),
          levels = unique(model$coefs$vcov)
        )
      )
  }
  
  # Create x-axis labels
  label <- if(model$vars[1] != "hwi_binarybin")
    c(
      paste0("<", model$breaks[1]),
      sapply(seq_along(model$breaks[-1]), function(b)
        paste0(model$breaks[b], "-", model$breaks[b + 1])),
      paste0(">=", model$breaks[length(model$breaks)])
    )
  else
    c(
      0,
      sapply(
        seq_along(model$breaks[-1]), 
        function(b) paste0(model$breaks[b])
      ),
      paste0(">=", model$breaks[length(model$breaks)])
    )
  
  # x-axis title
  xname <- case_when(
    model$vars[1] == "tavgbin" ~ "Avg. Temperature (°C)",
    model$vars[1] == "tmaxbin" ~ "Max. Temperature (°C)",
    model$vars[1] == "hibin"   ~ "Heat Index (°C)",
    model$vars[1] == "hwi_binarybin"  ~ "Binary Heat Wave Index",
    model$vars[1] == "hwi_tempbin"  ~ "Heat Wave (°C) Index",
    model$vars[1] == "hwi_sdbin"  ~ "Heat Wave (SD-Days) Index",
    TRUE ~ "name missing"
  )
  
  # Set legend labels for grouped data
  if (!is.null(plt_table$group)) {
    if (model$groups == "temperature")
      glabels <- c("Mean Max. Temperature", "Above Median", "Below Median")
    if (model$groups == "income")
      glabels <- c("Mean Income", "Above Median", "Below Median")
  }
  
  # For the vcov comparison plot,
  #   hard-coded solution
  if (length(unique(model$coefs$vcov)) > 1) {
    cbbPalette <- rev(c("#E69F00", "#000000", "#56B4E9"))
    
    plt <- ggplot(plt_table, aes(x = x, y = coef)) +
      geom_ribbon(
        aes(
          ymin = lb,
          ymax = ub,
          fill = vcov
        ), 
        alpha = 0.9
      ) +
      geom_hline(
        yintercept = 0,
        linetype = 2,
        alpha = .5
      ) +
      geom_line() +
      scale_x_continuous(
        name = paste0("Daily ", xname, " Bins"),
        breaks = 1:nbin,
        labels = label,
        guide = guide_axis(angle = 45)
      ) +
      scale_y_continuous(name = yname) +
      coord_cartesian(ylim = coords) +
      scale_fill_manual(
        name = "Variance estimator",
        labels = c("Twoway", "Conley-HAC", "Heteroskedasticity-\nrobust"),
        values = cbbPalette
      )
    
    if (save) {
      if (is.null(fname)) {
        ggsave(
          paste0("Figures/", deparse(substitute(model)), ".pdf"),
          device = "pdf",
          plot = plt,
          width = width,
          height = height
        )
      } else
        ggsave(
          paste0("Figures/", fname, ".pdf"),
          device = "pdf",
          plot = plt,
          width = width,
          height = height
        )
    }
    
    return(plt)
  }
  
 
  if (length(unique(plt_table$fixef)) == 1) {
    # Single specification
    if (is.null(plt_table$group)) {
      # No Groups
      plt <- ggplot(plt_table, aes(x = x, y = coef)) +
        geom_ribbon(aes(ymin = lb, ymax = ub), alpha = .2) +
        geom_hline(
          yintercept = 0,
          linetype = 2,
          alpha = .5
        ) +
        geom_line() +
        scale_x_continuous(
          name = paste0("Daily ", xname, " Bins"),
          breaks = 1:nbin,
          labels = label,
          guide = guide_axis(angle = 45)
        ) +
        scale_y_continuous(name = yname) +
        coord_cartesian(ylim = coords) 
    } else {
      # Single specification
      # Groups
      plt <- ggplot(plt_table, aes(x = x, y = coef)) +
        geom_ribbon(
          aes(
            ymin = lb,
            ymax = ub,
            fill = factor(group)
          ), 
          alpha = .2
        ) +
        geom_hline(
          yintercept = 0,
          linetype = 2,
          alpha = .5
        ) +
        geom_line(aes(colour = factor(group))) +
        scale_x_continuous(
          name = paste0("Daily ", xname, " Bins"),
          breaks = 1:nbin,
          labels = label,
          guide = guide_axis(angle = 45)
        ) +
        scale_y_continuous(name = yname) +
        scale_color_discrete(labels = glabels[-1]) +
        scale_fill_discrete(labels = glabels[-1]) +
        labs(color = glabels[1], fill = glabels[1]) +
        coord_cartesian(ylim = coords) 
    }
  } else {
    # Multiple specifications
    if (is.null(plt_table$group)) {
      # No groups
      plt <- ggplot(plt_table, aes(x = x, y = coef)) +
        geom_ribbon(aes(ymin = lb, ymax = ub), alpha = .2) +
        geom_hline(
          yintercept = 0,
          linetype = 2,
          alpha = .5
        ) +
        geom_line() +
        facet_wrap(~ fixef, labeller = labeller(fixef = translate_fe)) +
        scale_x_continuous(
          name = paste0("Daily ", xname, " Bins"),
          breaks = 1:nbin,
          labels = label,
          guide = guide_axis(angle = 45)
        ) +
        scale_y_continuous(name = yname, n.breaks = 4) +
        coord_cartesian(ylim = coords)
    } else {
      # Multiple specifications
      # Groups
      plt <- ggplot(plt_table, aes(x = x, y = coef)) +
        geom_ribbon(
          aes(
            ymin = lb,
            ymax = ub,
            fill = factor(group)
          ), 
          alpha = .2
        ) +
        geom_hline(
          yintercept = 0,
          linetype = 2,
          alpha = .5
        ) +
        geom_line(aes(colour = factor(group))) +
        facet_wrap(~ fixef, labeller = labeller(fixef = translate_fe)) +
        scale_x_continuous(
          name = paste0("Daily ", xname, " Bins"),
          breaks = 1:nbin,
          labels = label,
          guide = guide_axis(angle = 45)
        ) +
        scale_y_continuous(name = yname, n.breaks = 4) +
        scale_color_discrete(labels = glabels[-1]) +
        scale_fill_discrete(labels = glabels[-1]) +
        labs(color = glabels[1], fill = glabels[1]) +
        coord_cartesian(ylim = coords)
    }
  }
  
  if (save) {
    if (is.null(fname)) {
      ggsave(
        paste0("Figures/", deparse(substitute(model)), ".pdf"),
        device = "pdf",
        plot = plt,
        width = width,
        height = height
      )
    } else
      ggsave(
        paste0("Figures/", fname, ".pdf"),
        device = "pdf",
        plot = plt,
        width = width,
        height = height
      )
  }
  
  plt
  
}
