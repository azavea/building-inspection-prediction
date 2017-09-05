###########################################################################
## PROJECT: L+I predictive modeling
##
## SCRIPT PURPOSE: Functions
##    - Helper functions for all scripts within the project
##
## DATE: 18 July 2017
## AUTHOR: Simon Kassel
###########################################################################


# all ---------------------------------------------------------------------

packages <- function(package_vector) {
  # Check to see if packages are alreay installed, if so load them, if not
  # install then load them.
  #
  # Args:
  #   package_vector: a vector of quoted package names
  #
  # Returns:
  #   NA
  #
  # Side effects:
  #   Installs and loads packages
  #
  
  for (lib in package_vector) {
    if (!requireNamespace(lib, quietly = TRUE))
      install.packages(lib)
    suppressMessages(library(lib, character.only = TRUE))
    remove(lib)
  }
}


# data-wrangling-and-feature-engineering.R --------------------------------

getCoords <- function(spObject) {
  # Add the coordinates of a spatial points dataframe as attributes in the
  # table
  #
  # Args:
  #   spObject: a spatial points dataframe to get the coordinates of
  #
  #
  # Returns:
  #   A new sp object with lat/lon fields with the names 'geocode_x' and
  #   'geocode_y'
  #
  
  spObject$geocode_x <- spObject@coords[,1]
  spObject$geocode_y <- spObject@coords[,2]
  
  return(spObject)
}


joinSPdfs <- function(violations, inspections, field) {
  # Joins together violations and inspection sp data frames into one full
  # dataset
  #
  # Args:
  #   violations: a spatial points dataframe of violations
  #   inspections: a spatial points data frame of violations
  #   field: a quoted field name to joine by
  #
  # Returns:
  #   A data frame of all violations at all inspections
  #
  
  vdat <- violations@data
  idat <- inspections@data

  joinColumns <- names(vdat) %>%
    .[!. %in% names(idat)] %>%
    c(field)

  full_dat <- full_join(idat, vdat[, c(joinColumns)], by = field)

  return(full_dat)
}


cleanData <- function(dat) {
  # Misc data cleaning
  #
  # Args:
  #   dat: a data frame to clean and rename
  #
  # Returns:
  #   a cleaned and renamed data frame
  #
  
  dat %>%
    createUID() %>%
    renameFields() %>%
    addViolYear() %>%
    data.table() %>%
    return()
}


filterData <- function(dat) {
  # Remove NAs and filter data frams
  #
  # Args:
  #   dat: a data frame of all inspections with NAs
  #
  # Returns:
  #   filtered data frame with NAs removed
  #
  
  dat %>%
    removeNAs() %>%
    filterByOutcome() %>%
    return()
}


filterByOutcome <- function(dat) {
  # Filter data frame for just passed and failed inspection outcomes
  # and for just inspections after 2014
  #
  # Args:
  #   dat: a data frame of all inspections (outcome and year)
  #
  # Returns:
  #   filtered data frame
  #
  
  dat %>%
    filter(violYear > 2014) %>%
    filter(inspStatus %in% c('Passed', 'Failed')) %>%
    return()
}


createUID <- function(dat) {
  # Add a uid field to the dataset
  #
  # Args:
  #   dat: a data frame with 'apinspkey' and 'apfailkey' fields
  #
  # Returns:
  #   the same data frame with a uid field at the beginning
  #
  
  full_dat$uid <- paste(dat$apinspkey, dat$apfailkey, sep = '.')
  full_dat <- full_dat[ , c(ncol(full_dat), 1:(ncol(dat)))]
  return(full_dat)
}


renameFields <- function(dat) {
  # Rename fields and remove index field
  #
  # Args:
  #   dat: a data frame with the necessary variables
  #
  # Returns:
  #   The same data frame but renamed
  #
  
  dat %>%
    select(-gid, -organizati, -unit) %>%
    dplyr::rename(
      inspType = inspection,
      inspDesc = inspecti_1,
      inspCompl = inspecti_2,
      inspStatus = inspecti_3,
      long = geocode_x,
      lat = geocode_y,
      caseResDte = caseresolu,
      caseResCde = casereso_1,
      violDte = violationd,
      violTpe = violationt,
      violDesc = violatio_1) %>%
    return()
}


addViolYear <- function(dat) {
  # Add a violation year
  #
  # Args:
  #   dat: a data frame with a violDte field
  #
  # Returns:
  #   The same data frame but with violation year field
  #
  
  dat %>%
    dplyr::mutate(violYear = lubridate::year(violDte)) %>%
    return()
}


removeNAs <- function(dat) {
  # Remove NA values from appropraite fields
  #
  # Args:
  #   dat: a data frame with NA values
  #
  # Returns:
  #   a data frame with NAs removes
  #
  
  full_dat %>%
    drop_na(-c(ownername, address, censustrac)) %>%
    return()
}


measureFails <- function(dat, row){
  # Given a row, loop through the dataset measuring the outcomes before
  # and after this point
  #
  # Args:
  #   dat: data.table object containing the measurment row
  #   row: the index of the row
  #
  # Returns:
  #   a data.table of pre- and post- inspection outcomes
  #
  
  dat <- data.table(dat)

  r.t <- dat[row, c('uid', 'casenumber', 'apfailkey',
                    'inspCompl', 'inspStatus', 'violDte')]

  subdat <- dat[casenumber == r.t$casenumber & apfailkey == r.t$apfailkey,
                c('uid', 'casenumber', 'apfailkey',
                  'inspCompl', 'inspStatus', 'violDte')]

  fails <- data.table(uid = r.t$uid, before = 0, after = 0,
                      rec.inspCompl = 'none', rec.inspStatus = 'none', 
                      rec.uid = "none")

  for (r in 1:nrow(subdat)) {

    r.m <- subdat[r, ]

    # if the test insp date is after the reference insp. date
    if (!is.na(as.Date(r.m$inspCompl)) && !is.na(as.Date(r.t$inspCompl))) {

      if (r.t$inspCompl > r.m$inspCompl) {

        if (class(fails$rec.inspCompl) != "Date") {

          # update the "recent" variables
          fails$rec.uid <- r.m$uid
          fails$rec.inspStatus <- r.m$inspStatus
          fails$rec.inspCompl <- r.m$inspCompl

          if (r.m$inspStatus == "Failed") {

            fails$before <- fails$before + 1

          }
        } else if (fails$rec.inspCompl < r.m$inspCompl) {

          # update the "recent" variables
          fails$rec.uid <- r.m$uid
          fails$rec.inspStatus <- r.m$inspStatus
          fails$rec.inspCompl <- r.m$inspCompl

          if (r.m$inspStatus == "Failed") {
            fails$before <- fails$before + 1
          }
        }
      } else if (r.t$inspCompl < r.m$inspCompl && r.m$inspStatus == "Failed") {

        fails$after <- fails$after + 1

      }
    }
  }
  return(fails)
}


saveTimeStampedFile <- function(obj, file = "temp_file", time = FALSE) {
  # save any object with an appended time stamp to the output folder
  #
  # Args:
  #   obj: any type of object to save in an .Rdata file
  #   file: the string for a portion of the filename without path, date
  #         or file extension
  #   specif: a boolean indicating whether to use the time-date (default
  #           to false), or just the regular date
  #
  # Returns:
  #   NA
  #
  # Side effects:
  #    saves the object to output folder as .Rdata file
  #
  
  if (time) {
    str <- paste0("output/", file, "_", Sys.time(), ".Rdata")
  } else {
    str <- paste0("output/", file, "_", Sys.Date(), ".Rdata")
  }
  save(obj, file = str)
}


measureAllFails <- function(dat) {
  # Apply the measurefails function over every row, combining infot a
  # data frame and binding to the original df
  #
  # Args:
  #   dat: the data frame to measure
  #
  # Returns:
  #   a new data frame with recent inspection data merged to it
  #
  # Side effects:
  #    Prints a progress text bar
  #    saves an Rdata file of the resulting dataet to the outpur folder
  #

  # loop over whole dataset
  rec <- ldply(c(1:nrow(dat)), measureFails, dat = dat, .progress = "text")

  # merge recents with original df
  df_rec <- merge(dat, rec, by = "uid")

  # save the dataset to output file
  saveTimeStampedFile(df_rec, file = "lni_15to17_withRecent", time = FALSE)

  return(df_rec)
}


since <- function(dat, var) {
  # Measure the time between the date of a variable and the inspection
  #
  # Args:
  #   dat: the data fram containing all variables
  #   var: the field from df as a string
  #
  # Returns:
  #   a vctor of durations
  #
  
  comp <- dat[ ,"inspCompl"] %>% unlist() %>% as.Date()
  meas <- dat %>%
    select(starts_with(var)) %>%
    unlist() %>% as.Date()

  as.numeric(comp - meas) %>%
    return()
}


durationVars <- function(dat) {
  # Apply the ince function to measure time elapsed since each one
  #
  # Args:
  #   dat: data frame with the following fields (inspCompl,
  #        caseaddedd, violDte, rec.inspCompl)
  #
  # Returns:
  #   a data frame with three duaration vars
  #   
  
  dat %>%
    mutate(
      sinceCaseAdded = since(., "caseaddedd"),
      sinceViolation = since(., "violDte"),
      sinceLastInsp = since(., "rec.inspCompl")
    ) %>%
    return()
}


findLevels <- function(dat, convertChars = FALSE, convertNums = FALSE) {
  # Find the levels of each categorical variable in the dataset
  #
  # Args:
  #   dat: data frame, to measure
  #   convertChars: boolean, do you want to include character vars
  #
  # Returns:
  #   a named integer of the levels of each factor variable
  #   
  
  dat <- as.data.frame(dat)

  if (convertChars) {
    dat <- dat %>%
      mutate_if(is.character, as.factor)
  }

  if (convertNums) {
    dat <- dat %>%
      mutate_if(is.numeric, as.factor)
  }

  factDat <- dat %>% select_if(is.factor)

  table <- sapply(factDat, function(x) length(unique(x)))

  return(table)
}


sumfailed <- function(x) {
  # Cont the number of instances of the string 'failed' in a vector
  #
  # Args:
  #   x: vector, to loop through
  #
  # Returns:
  #   an integer count of 'failed' observations
  #
  
  nf <- 0
  for(i in x) {
    if (i == "Failed") {
      nf <- nf + 1
    }
  }
  return(nf)
}


summariseInspections <- function(dat) {
  # Summarize to the inspection level
  #
  # Args:
  #   dat: data frame, with all requisite fields
  #
  # Returns:
  #   summarized data frame
  #
  
  dat %>%
    group_by(apinspkey) %>%
    dplyr::summarise(

      o.failed = sum(failed) / n(),
      o.numFails = sum(failed),

      e.numViol = n(),
      e.prevFail = sumfailed(rec.inspStatus),
      e.numFailTypes = n_distinct(violTpe),

      f.before = first(before),
      f.priorityde = first(priorityde),
      f.recStatus = first(rec.inspStatus),
      f.inspDesc = first(inspDesc),

      s.sinceLastInsp = first(sinceLastInsp),
      s.sinceViolation = first(sinceViolation),
      s.sinceAdded = first(sinceCaseAdded),

      i.casenumber = first(casenumber),
      i.inspCompl = first(inspCompl),

      l.long = first(long),
      l.lat = first(lat),
      l.address = first(address),
      l.censustrac = first(censustrac),
      l.addresskey = first(addresskey),
      l.owner = first(ownername)

    ) %>%
    dplyr::rename(
      id.apinspkey = apinspkey) %>%
    return()
}


castAndSumm <- function(dat, idVar, castVar, binary = TRUE) {
  # Cast a categorical variable and summarise by id variable
  #
  # Args:
  #   dat: data frame, with variables
  #   idVar: string, the identification variable to summarize by
  #   castVar: string, categorical factor variable to cast
  #
  # Returns:
  #   A new data frame with unique observations of the idVar and dummy
  #   vars for each level of the categorical var
  #
  
  dat2 <- dat %>%
    select(one_of(idVar, castVar)) %>%
    mutate(dummy = 1)

  colnames(dat2) <- c("idVar", "castVar", "dummy")

  if (binary) {
    summ <- dcast.data.table(dat2, 
                             idVar ~ castVar, 
                             sep = ".", 
                             fun.aggregate = function(x) {
      if (sum(x) > 1) {
        return(1)
      } else {
        return(sum(x))
      }
    }, value.var = "dummy", fill = 0)
  } else {
    summ <- dcast.data.table(dat2, idVar ~ castVar, 
                             sep = ".", 
                             fun.aggregate = sum(), 
                             value.var = "dummy", 
                             fill = 0)
  }

  colnames(summ) <- c(
    idVar,
    paste0(castVar, ".", c(1:(ncol(summ) - 1)))
  )

  return(summ)
}


castFactorVars <- function(dat, var) {
  # cast factor variables to a new data frame
  #
  # Args:
  #   dat: data frame, with "id.apinspkey" and var to cast
  #   var: string. name of the factor variable to cast
  #
  # Returns:
  #   a dta frame with 'id.apinspkey' and dummy varsiables
  #
  
  form <- paste0("id.apinspkey ~ ", var)

  if (grepl("f.", var, fixed = TRUE)) {
    var <- gsub("f.", "", var)
  }

  df <- dummyVars(form, data = dat, sep = ".") %>%
    predict(dat) %>%
    data.frame()

  names(df) <- paste0(var, ".", c(1:ncol(df)))

  df <- df %>%
    mutate(id.apinspkey = dat$id.apinspkey)

  return(df)

}


getProj4String <- function(crs) {
  # Return the appropriate CRS object
  #
  # Args:
  #   crs: string, specifying which coordinate proj4string object to
  #        return - 'pas' for PA South, 'wgs' for web mercator
  #
  # Returns:
  #   What the function returns
  #
  
  if (crs == "pas") {
    return(CRS("+init=epsg:6565"))
  } else if (crs == "wgs") {
    return(CRS("+proj=longlat +datum=WGS84"))
  } else {
    stop("Invalid CRS specification")
  }
}


createPointPattern <- function(
  dat, long, lat, initCRS = "wgs", transformCRS = "wgs"
  ) {
  # Create spatial objects from a data frame with coordinates
  #
  # Args:
  #   dat: data frame, with two different coordinate fields
  #   long: string, logitude field name
  #   lat: string, latitude field name
  #   initCRS: string, argument for getProj4String -- the coordinate
  #            system of the lat/long fields. Defaults to 'wgs'
  #   transformCRS: string, argument for getProj4String -- the
  #                 coordinate system to convert sp object to,
  #                 defaults to 'wgs'
  #
  # Returns:
  #   A two object list
  #     [[1]] pointDataFrame: a spatialPointsDataFrame
  #     [[2]] pointPattern: a pointPattern
  #
  
  coordsDat <- cbind(
    dat[ , long],
    dat[ , lat]
  )

  spDf <- SpatialPointsDataFrame(
    coords = coordsDat,
    data = dat,
    proj4string = getProj4String(initCRS)
  ) %>% spTransform(getProj4String(transformCRS))

  win <- owin(spDf@bbox[1, ], spDf@bbox[2, ])

  pp <- ppp(x = spDf@coords[ ,1], y = spDf@coords[ ,2], window = win)

  objList <- list("pointDataFrame" = spDf, "pointPattern" = pp)

  return(objList)
}


getCases <- function(ds) {
  # Condense inspections dataset to a dataframe of cases
  #
  # Args:
  #   ds: a dataframe of cases, must include the following fields
  #     'f.before', 'l.long', 'l.lat'
  #
  # Returns:
  #   A case-level data frame with a binary column indicating whether
  #   the case is repeator or not
  # 
    
  cases <- ds %>%
    
    mutate(f.before = as.numeric(as.character(f.before))) %>%
    group_by(i.casenumber) %>% 
    
    dplyr::summarise(
      f.before = max(f.before),
      l.long = max(l.long),
      l.lat = max(l.lat)
    ) %>% 
    
    mutate(repeator = ifelse(f.before > 1, "y", "n"))
}


average_dist <- function(vec) {
  # Find the average distance in a vector, accounting for a possible 
  #   erroneous 0 value at position 1
  #
  # Args:
  #   vec: a numeric vector, of distances
  #
  # Returns:
  #   an average distance
  # 
    
  l <- length(vec)
  
  if (vec[1] == 0) {
    newVec <- vec[2:l]
  } else {
    newVec <- vec[1: (l-1)]
  }
  
  sum <- newVec %>% unname() %>% sum()
  
  mean <- sum / length(vec)
  
  return(mean)
}


avgDistBetween <- function(data.pp, meas.pp, n) {
  # Find the average distance between points within one pattern and
  #   the nearest n points in a different pattern
  #
  # Args:
  #   data.pp: point pattern, to measure the distances to
  #   meas.pp: point pattern, of instances that you are interested
  #     in the proximity of
  #   n: integer, the number of instances in meas.pp that you would 
  #     like to measure 
  #
  # Returns:
  #   a vector of avergae distances to the n nearest points for
  #   every instance of data.pp
  #   
  
  p <- nncross(data.pp, cases.pp, what = "dist", k = c(1:n))
  apply(as.matrix(p), 1, average_dist) %>%
    return()
}


getSpAutoCorr <- function(n, cases.list) {
  # Get a set of variables measuring the average distances to the
  #   n nearkest repeators, non-repeators and other inspection
  #
  # Args:
  #   n: integer, the number of neighbors
  #   cases.list: a list of spDataFrame and point pattern, output of 
  #               createPointPattern
  #
  # Returns:
  #   a data frame of all features for a given value of n
  # 
    
  cases.sp <- cases.list[[1]]
  cases.pp <- cases.list[[2]]
  
  marks(cases.pp) <- cases.sp@data$repeator
  
  repeators <- subset(cases.pp, marks == "y") %>% unique()
  nonrepeators <- subset(cases.pp, marks == "n") %>% unique()
  
  p.all <- nncross(cases.pp, cases.pp, what = "dist", k = c(1:n))
  p.repeators <- nncross(cases.pp, repeators, what = "dist", k = c(1:n))
  p.nonrepeators <- nncross(cases.pp, nonrepeators, 
                            what = "dist", k = c(1:n))
  
  avgDists.all <- apply(as.matrix(p.all), 1, average_dist)
  avgDists.rep <- apply(as.matrix(p.repeators), 1, average_dist)
  avgDists.nr <- apply(as.matrix(p.nonrepeators), 1, average_dist) 
  
  p <- data.frame(avgDists.all, avgDists.nr, avgDists.rep)
  
  p$avgDists.all <- log(p$avgDists.all + 1)
  p$avgDists.nr <- log(p$avgDists.nr + 1)
  p$avgDists.rep <- log(p$avgDists.rep + 1)
  p$avgDists.repOfAll <- p$avgDists.rep / p$avgDists.all
  
  colnames(p) <- paste0("e.", colnames(p), ".n", n)
  
  return(p)
}


endogMain <- function(ds, nlist) {
  # Add endogenous variables to inspection level dataset
  #
  # Args:
  #   ds: dataframe, inspection level dataset with requisite fields
  #   nlist: vector of integers, the levels of neighbor to try
  #
  # Returns:
  #   a copy of the ds dataset with new variables included
  # 
   
  cases.list <- getCases(ds) %>%
    createPointPattern("l.long", "l.lat", "wgs", "pas") 
  
  pd <- cases.list[[1]]@data %>%
    select(i.casenumber)
  
  for (i in nlist) {
    p <- getSpAutoCorr(i, cases.list)
    pd <- cbind(pd, p)
  }
  
  dss <- ds %>%
    left_join(pd) %>% 
    return()
}


squishToRange <- function(vector, minbound = 0.05, maxbound = 0.95) {
  # Remove outliers within a vector of continuous values
  #
  # Args:
  #   vector: vector, a vector of numeric values
  #   minboud: float, specifying the lower threshold for lowe
  #     outlier values. Values below this threshold will be set to 
  #     this value.
  #   maxbourd: float, specifying the upper threshold for high
  #     outlier values. Values above this threshold will be set to 
  #     this value.
  #
  # Returns:
  #   A numeric vector that fits with in the 5th-95th percentile
  #   range of the original distribution
  #   
  
  range <- stats::quantile(vector, c(minbound, maxbound))
  range <- round(range)
  newVector <- scales::squish(vector, range)
  return(newVector)
}


getFactorLevelLookup <- function(vec, prefix) {
  # Get a lookuptable to find the original vartiable class from a one-hot
  #   encoded categorical variable
  #
  # Args:
  #   vec: factor vector, of a categorical variables
  #   prefix: string, that will be the prefix for all of these features in
  #     the final dataset
  #
  # Returns:
  #   a data frame with one column indicating the variable name in the 
  #     final dataset and the other showing the original factor
  #   
  
  data.frame(name = levels(vec), level = c(1:length(table(vec)))) %>%
    mutate(level = paste0(prefix, ".", level)) %>%
    return
  
}

# model-selection.R -------------------------------------------------------

factor_to_numeric <- function(fac) {
  # Convert a factor to numeric equivalent of characters
  #
  # Args:
  #   fac: a factor or vector of factors
  #
  # Returns:
  #   a number or numeric vector
  #   
  
  fac %>%
    as.character %>%
    as.numeric %>%
    return
}


getVarNames <- function(all_vars, prefixes) {
  # Subset a vector of variable names based on the first two letter 
  #
  # Args:
  #   all_vars: string vector, to filter
  #   prefixes: string or string vector, of two letter prefixe(s). Names
  #     that start with any of these will be returned
  #
  # Returns:
  #   A vector of variable names (strings) 
  #   
  
  keep <- c()
  
  for (i in all_vars) {
    pre <- substr(i, 1, 2)
    if (pre %in% prefixes) {
      keep <- c(keep, i)
    }
  }
  
  return(keep)
}


get_from_grid <- function(grid_id, valid_frame) {
  # Return a list of objects from an h2o grid
  #
  # Args:
  #   grid_id: string, the id for the grid in question
  #   valid_frame: h2OFrame, to predict for
  #
  # Returns:
  #   A list of results objects
  #     [[1]] the original h2o grid 
  #     [[2]] the best model, determined by CV AUC
  #     [[3]] a data frame with fields for predicted values as well as
  #           binary predictions
  #   
  
  # sort results descending by auc
  sorted_models <- h2o.getGrid(
    grid_id = grid_id,
    sort_by = "auc",
    decreasing = T
  )
  
  # get best model
  best_mod <- h2o.getModel(sorted_models@model_ids[[1]])
  
  # make predctions on validation set
  prefix <- gsub("\\..*", "", grid_id)
  
  pred <- h2o.predict(best_mod, valid_frame) %>%
    as.data.frame %>%
    select(-p0) %>%
    mutate_if(is.factor, funs(factor_to_numeric)) %>%
    mutate(
      correct = ifelse(as.data.frame(valid)$o.failed.n == predict, 1, 0))
  
  colnames(pred) <- paste0(prefix, ".", colnames(pred))
  
  # create a list with all three objects
  from_grid <- list(sorted_models, best_mod, pred)
  
  names(from_grid) <- c(
    paste0(prefix, ".sorted_models"),
    paste0("fit.", prefix, ".tuned.h2o"),
    paste0("pred.", prefix)
  )
  
  return(from_grid)
}


getInteractionVars <- function(frame, vars, bind = TRUE) {
  # Get pairwise interaction variables of specified columns in 
  #   h2o dataframe
  #
  # Args:
  #   frame: an h2o data frame
  #   vars: vector of strings, names of fields to
  #     to use as interaction variables
  #   bind: boolean, whould the data frame of interaction
  #     variables be bound to the original data frame (TRUE)
  #     or returned as their own df (FALSE)
  #
  # Returns:
  #   an h2o dataframe containing either just the interaction
  #   variables or the original df with interaction vars appended
  #   
  
  pw <- h2o.interaction(
    frame, 
    destination_frame = 'pairwise',
    factors = list(vars),
    pairwise=TRUE, 
    max_factors = 10, 
    min_occurrence = 1
  ) 
  
  if (bind) {
    all <- h2o.cbind(frame, pw)
    return(all)
  } else {
    return(pw)
  }
}


h2oLogMod <- function(x, model_id, training_frame = train.h2o) {
  # Train an h2o logistic regression with specific params
  #
  # Args:
  #   pred_vars: string, inhereted from h2o.glm
  #   model_id: string, inhereted from h2o.glm
  #   training_frame: h2o data frame, inhereted from h2o.glm
  #
  # Returns:
  #   an h2o glm model
  #   
  
  h2o.glm(
    y = "o.failed.f",
    x = pred_vars,
    model_id = model_id,
    training_frame = training_frame,
    nfolds = 10,
    lambda_search = TRUE,
    family = "binomial",
    standardize = TRUE,
    alpha = 0.25
  )
}


getPredictions <- function(x, validation_frame) {
  # Get predictions from an h2o model 
  #
  # Args:
  #   x: an h2o model
  #   validation_frame: h2o dataframe, to predict for
  #
  # Returns:
  #   A data frame of predictions/predicted probabilities
  #   
  
  name <- x[[1]]
  pred_df <- h2o.predict(x[[2]], validation_frame) %>%
    as.data.frame %>%
    mutate(var = name) %>%
    select(-p0)
  names(pred_df) <- paste0(name, ".", names(pred_df))
  return(pred_df)
}
# data-vizualization.R ----------------------------------------------------

# Color palette to use for visualizations
#   General palette
pal <- c("#5876BF", "#126075", "#2C7A90", "#ECE2C9", "#8D1B1A", 
         "#FFFFFF","#7F7F7F", "#000000")
#   Specific palette for waffle plots
waffle_pal <- c("#126075","#8cd8ed","#8D1B1A","#ea908f")


plotTheme <- function(text_color = 8, strip_color = 4, title_color = 2, map = FALSE) {
  # Define a set of reusable ggplot theme parameters
  #
  # Args:
  #   text_color: integer, index of a color in pal to use for all text
  #   strip_color: integer, index of a color in pal to use for ggplot facet
  #     strips
  #   map: boolean, determines whether to use the void or minimal base theme
  #
  # Returns:
  #   a set of theme parameters to format a ggplot2 plot
  #   
  
  if (map) {
    base <- theme_void() 
  } else {
    base <- theme_minimal()
  }
  base +
    theme(
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
      text = element_text(
        color = pal[text_color],
        size = 10),
      plot.title = element_text(
        hjust = 0, 
        face = "bold", 
        margin = margin(b = 3),
        size = 11,
        color = pal[title_color]),
      plot.subtitle = element_text(
        hjust = 0, 
        face = "italic",
        margin = margin(b = 10),
        color = pal[title_color]),
      legend.direction = "vertical", 
      legend.position = "right", 
      legend.key.height = unit(1, "cm"), 
      legend.key.width = unit(0.2, "cm"),
      legend.title = element_text(
        size = 8,
        face = "italic",
        vjust = -2),
      plot.caption = element_text(
        size = 8,
        hjust = 0,
        face = "italic"),
      strip.background = element_rect(
        color = pal[strip_color]),
      axis.title = element_text(
        hjust = 1,
        face = "italic"
      )
    )
}


tidy_confusion_matrix <- function(dat, response_var, pred_var) {
  # Output a tidies dataset for confusion matrix map
  #
  # Args:
  #   dat: data frame with result vars
  #   response_var: string, name of response variable
  #   pred_var: string, name of prediction outcome variable
  #
  # Returns:
  #   A data frame with confusion matrix fields
  #   
  
  d <- dat %>%
    select(one_of(c(response_var, pred_var, "l.long", "l.lat")))
  names(d) <- c("response", "pred", "long", "lat")
  d <- d %>%
    mutate(
      true_positive = ifelse(response == 1 & pred == 1, 1, 0),
      true_negative = ifelse(response == 0 & pred == 0, 1, 0),
      false_positive = ifelse(response == 1 & pred == 0, 1, 0),
      false_negative = ifelse(response == 0 & pred == 1, 1, 0)
    )
  return(d)
}