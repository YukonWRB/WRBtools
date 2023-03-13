#' Watershed/basin delineation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Delineates watersheds above one or more points using [Whitebox Tools](www.whiteboxgeo.com/). To facilitate this task in areas with poor quality/low resolution DEMs, can "burn-in" a stream network to the DEM to ensure proper stream placement (see details). Many time-consuming raster operations are performed, so the function will attempt to use already-calculated rasters if they are present in the same path as the base DEM and named according to the function's naming conventions. In practice, this means that only the first run of the function needs to be very time consuming. See additional details below for processing steps.
#'
#' NOTE 1: This tool can be extremely slow to execute, and will use a lot of memory. Be patient, it might take several hours with a large DEM, even days on first run or whenever performing operations that call for breaching voids in the DEM.
#'
#' NOTE 2: If you are have already run this tool and are using a DEM in the same directory as last time, you only need to specify the DEM and the points (and, optionally, a projection). Operations using the streams shapefile and generating flow accumulation, etc rasters do not need to be repeated unless you want to use a different DEM or streams shapefile.
#'
#' NOTE 3: This function is very memory (RAM) intensive, despite performing most raster operations to disk rather than holding information in memory. You'll want at least 16GB of RAM, and to ensure that most of it is free. If you get an error such as 'cannot allocate xxxxx bytes', you probably don't have the resources to run the tool. The WhiteboxTool functions in particular are memory hungry: all rasters are un-compressed and converted to 64-bit fload type before starting work, and there needs to be room to store more than twice that uncompressed raster size in memory. Example: for the whole Yukon at a resolution of 16.9 meters (the highest resolution CDEM) the tool attempts to allocate 36GB of memory.
#'
#' @details
#' This function uses software from the Whitebox geospatial analysis package, built by Prof. John Lindsay. Refer to [this link](https://www.whiteboxgeo.com/manual/wbt_book/intro.html) for more information.
#'
#' Explanation of process:
#' Starting from a supplied DEM, the function will burn-in a stream network depression (ensuring that flow accumulations happen in the correct location), breach depressions in the digital elevation model using a least-cost algorithm (i.e. using the pathway resulting in minimal changes to the DEM) then calculate flow accumulation and direction rasters. Then, a raster of streams is created from flow accumulation/direction rasters. The points provided by the user are then snapped to the derived streams raster and watersheds are computed using the flow direction rasters. Finally, the watershed/drainage basin polygons are saved to the specified save path along with the provided points and the snapped pour points.
#'
#' Using streams shapefile to burn-in depressions to the DEM:
#' Be aware that this part of the function should ideally be used with a "simplified" streams shapefile. In particular, avoid or pre-process stream shapefiles that represent side-channels, as these will burn-in several parallel tracks to the DEM. ESRI has a tool called "simplify hydrology lines" which is great if you can ever get it to work, and WhiteboxTools has functions [whitebox::wbt_remove_short_streams()] to trim the streams raster, and [whitebox::wbt_repair_stream_vector_topology()] to help in converting a corrected streams vector to raster in the first place.
#'
#' @param DEM The path to a DEM including extension from which to delineate watersheds/catchments. Must be in .tif format. Note that a new raster will be written with either the projection of the points layer or of the projection specified. Reprojection is time-consuming, try to use an existing DEM if at all possible.
#' @param points The path to the points shapefile containing the points from which to build watersheds. As shapefiles have multiple associated files, point to the .shp file only. The attribute table of this shapefile will not be used, only the geometry. If parameter 'projection' is not specified, the crs of this layer will be used to set the crs of the DEM and subsequently calculated intermediary and output layers.
#' @param streams Optionally, the path to the polylines shapefile containing streams, which will be used to improve accuracy when using poor quality DEMs. If this shapefile is the only input parameter being modified from previous runs (i.e. you've found a new/better streams shapefile) then simply run this function with a shapefile specified for this parameter to incorporate this new layer.
#' @param projection Optionally, specify a projection string in the form epsg:3579 (find them [here](https://epsg.io/)). The inputs points and all derived processing and output layers will use this projection. If no projection is specified the projection of the points will be used.
#' @param snap Snap to the "nearest" derived (calculated) stream, or to the "greatest" flow accumulation cell within the snap distance? Beware that "greatest" will move the point downstream by up to the 'snap_dist' specified, while nearest might snap to the wrong stream.
#' @param snap_dist The search radius within which to snap points to streams. Snapping method depends on 'snap' parameter. Note that distance units will match the projection, so probably best to work on a meter grid.
#' @param breach_dist The max radius (in raster cells) for which to search for a path to breach depressions, passed to whitebox::wbt_breach_depressions_least_cost. Setting a large number here dramatically increases the tool's run time, as the number of possible solutions to sort through increase rapidly. Unbreached depressions are filled if any remain after breaching depressions.
#' @param overwrite If applicable, should rasters present in the same directory as the DEM be overwritten? This will also force the recalculation of derived layers.
#' @param save_path The path where you want the output shapefiles saved. Default "choose" lets you choose interactively.
#' @param force_update_wbt WhiteboxTools is by default only downloaded if it cannot be found on the computer, and no check are performed to ensure the local version is current. Set to TRUE if you know that there is a new version and you would like to use it.
#'
#' @return Saved to disk: an ESRI shapefile for each drainage basin, plus the associated pour point and the point as provided, all in a separate folder for each basin.
#'
#' @seealso [WSC_drainages()] if looking for drainages associated with a WSC monitoring location.
#' @export

# DEM <- "G:/water/Common_GW_SW/Data/basins/GMTED/merged 50N150W and 180W.tif"
# DEM <- "G:/water/Common_GW_SW/Data/basins/CDEM/full_res/Yukon.tif"
# DEM <- "G:/water/Common_GW_SW/Data/basins/CDEM/downsampled/Yukon_downsampled.tif"
# points <- "G:/water/Common_GW_SW/Data/database/polygons/watersheds/09EA004/09EA004_station.shp"
# streams <- "G:/water/Common_GW_SW/Data/basins/Water_Flow_50k_Canvec.shp"

drainageBasins <- function(DEM, points, streams = NULL, projection = NULL, snap = "nearest", snap_dist = 200, breach_dist = 50, overwrite = FALSE, save_path = "choose", force_update_wbt = FALSE) {

  #initial checks
  if (!(snap %in% c("nearest", "greatest"))){
    stop("The parameter 'snap' must be one of 'nearest' or 'greatest'.")
  }
  if (save_path == "choose") {
    print("Select the output folder for shapefiles...")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  if (!file.exists(DEM)){
    stop("The DEM you pointed to does not exist. Perhaps your file path is wrong?")
  }
  directory <- dirname(DEM)
  input_DEM <- DEM
  if (!is.null(streams)){
    if (!file.exists(streams)){
      stop("The streams shapefile you pointed to does not exist. Perhaps your file path is wrong? Reminder, I'm looking for the .shp file with extension.")
    }
  }

  #Check whitebox existence and version, install if necessary or if force_update_wbt = TRUE.
  wbt_check <- whitebox::check_whitebox_binary()
  if (wbt_check){
    version <- whitebox::wbt_version()
    print(paste0("Using WhiteboxTools version ", substr(version[1], 16, 20), ". If this is out of date, run function with force_update_wbt = TRUE."))
  } else if (!wbt_check | force_update_wbt){
    print("Installing WhiteboxTools binaries...")
    whitebox::wbt_install()
    version <- whitebox::wbt_version()
    print(paste0("Installed WhiteboxTools version ", substr(version[1], 16, 20)))
  }

  #change terra options
  old <- terra::terraOptions()
  terra::terraOptions(memfrac = 0.9)
  on.exit(terra::terraOptions(memfrac = old$memfrac))

  points <- terra::vect(points) #load the points
  original_projection <- paste0("epsg:", terra::crs(points, describe=TRUE)$code)
  DEM <- terra::rast(DEM) #load the DEM
  points <- terra::project(points, DEM)
  dir.create(paste0(tempdir(), "/temp_inputs"))
  terra::writeVector(points, paste0(tempdir(), "/temp_inputs/points.shp"), overwrite=TRUE)

  #Check for existence of final layers and that their extents match the provided DEM. Create them if not exist or different extent.
  d8pntr_exists <- FALSE
  streams_derived_exists <- FALSE
  d8fac_exists <- FALSE
  if (!is.null(streams) & !overwrite){ #no point in checking the derived rasters if a new streams layer is specified or if we're overwriting anyways
    print("Checking if the right layers already exist...")
    if (file.exists(paste0(directory, "/D8pointer.tif")) & !overwrite){
      d8pntr <- terra::rast(paste0(directory, "/D8pointer.tif"), overwrite=TRUE)
      if (terra::compareGeom(d8pntr, DEM)){
        d8pntr_exists <- TRUE
      }
    }
    if (file.exists(paste0(directory, "/streams_derived.tif")) & d8pntr_exists & !overwrite){
      streams_derived <- terra::rast(paste0(directory, "/streams_derived.tif"), overwrite=TRUE)
      if (terra::compareGeom(streams_derived, DEM)){
        streams_derived_exists <- TRUE
      }
    }
    if (snap == "greatest"){
      if (file.exists(paste0(directory, "/D8fac.tif"))){
        d8fac <- terra::rast(paste0(directory, "/D8fac.tif"), overwrite=TRUE)
        if (terra::compareGeom(d8fac, DEM)){
          d8fac_exists <- TRUE
        }
      }
    }
  }

  if (!streams_derived_exists | !d8pntr_exists | !d8fac_exists | overwrite){
    print("Caculating layers derived from the DEM as they are either missing, have different extents as the provided DEM, you've requested an overwrite of calculated layers, or you specified a streams shapefile.")
    if (!is.null(streams)){ #load streams, process to raster, and burn-in the DEM
      print("Creating a stream raster from the provided stream shapefile...")
      streams_input <- terra::vect(streams)
      streams_input <- terra::project(streams_input, DEM)
      streams_input <- terra::rasterize(streams_input, DEM, touches = TRUE, filename = paste0(tempdir(), "/temp_inputs/streams_input_rasterized.tif"), overwrite=TRUE) #Make raster stream network. Background has values NA. Write to disk to avoid memory restrictions.
      streams_input <- (streams_input/streams_input) * 20 #Make each cell value = 20 to later burn in a 20 meter depression
      streams_input <- terra::subst(streams_input, NA, 0) #replace background NAs with 0 so that it subtracts (nothing) from the DEM later; subtracting NA results in NA cells.
      print("Burning in depressions to the DEM where streams should be...")
      DEM_burned <- DEM - streams_input #burn-in the DEM
      terra::writeRaster(DEM_burned, paste0(directory, "/DEM_burned.tif"), overwrite = TRUE)
    }

    print("Breaching depressions in the DEM to ensure continuous flow paths...")
    whitebox::wbt_breach_depressions_least_cost(
      dem = if (is.null(streams)) input_DEM else paste0(directory, "/DEM_burned.tif"),
      output = paste0(directory, "/FilledDEM.tif"),
      dist = breach_dist,
      fill = TRUE)

    print("Calculating a flow accumulation raster...")
    whitebox::wbt_d8_flow_accumulation(input = paste0(directory, "/FilledDEM.tif"),
                                       output = paste0(directory, "/D8fac.tif"))

    print("Calculating a flow directions raster...")
    whitebox::wbt_d8_pointer(dem = paste0(directory, "/FilledDEM.tif"),
                             output = paste0(directory, "/D8pointer.tif"))

    # Make a raster of streams only from the DEM, with a threshold (in cells) for flow accumulation
    print("Creating a raster of streams based on the flow accumulation raster...")
    whitebox::wbt_extract_streams(flow_accum = paste0(directory, "/D8fac.tif"),
                                  output = paste0(directory, "/streams_derived.tif"),
                                  threshold = 300)
  } else {
    if (!is.null(streams)){
      print("Using pre-calculated derived layers for basin delineation. NOTE: you specified a streams shapefile which won't be used. If you want to incorporate it run this function again with overwrite = TRUE.")
    } else {
      print("Using pre-calculated derived layers for basin delineation.")
    }
  }

  #Now snap the points and delineate watersheds, returning polygons.
  print("Snapping points according to the parameters selected...")
  dir.create(paste0(tempdir(), "/shapefiles"))
  unlink(list.files(paste0(tempdir(), "/shapefiles"), full.names=TRUE))
  if (snap == "nearest"){
    whitebox::wbt_jenson_snap_pour_points(pour_pts = paste0(tempdir(), "/temp_inputs/points.shp"),
                                          streams = paste0(directory, "/streams_derived.tif"),
                                          output = paste0(tempdir(), "/shapefiles/snapped_points.shp"),
                                          snap_dist = snap_dist)
  } else if (snap == "greatest"){
    whitebox::wbt_snap_pour_points(pour_pts = paste0(tempdir(), "/temp_inputs/points.shp"),
                                   flow_accum = paste0(directory, "/D8fac.tif"),
                                   output = paste0(tempdir(), "/shapefiles/snapped_points.shp"),
                                   snap_dist = snap_dist)
  }

  snapped_points <- terra::vect(paste0(tempdir(), "/shapefiles/snapped_points.shp")) #load to memory so as to iterate over each point, allowing for looping. Otherwise the tool creates non-overlapping rasters.
  dir.create(paste0(tempdir(), "/rasters"))
  unlink(list.files(paste0(tempdir(), "/rasters"), full.names=TRUE))
  dir.create(paste0(save_path, "/watersheds_", Sys.Date()))
  print("Delineating watersheds and creating polygons...")
  for(i in 1:nrow(snapped_points)) {
    tryCatch({
      terra::writeVector(snapped_points[i, ], paste0(tempdir(), "/shapefiles/", i,".shp"), overwrite=TRUE)
      whitebox::wbt_watershed(d8_pntr = paste0(directory, "/D8pointer.tif"),
                              pour_pts = paste0(tempdir(), "/shapefiles/", i, ".shp"),
                              output = paste0(tempdir(), "/rasters/", i, ".tif")
      )

      rast <- terra::rast(paste0(tempdir(), "/rasters/", i, ".tif"))
      poly <- terra::as.polygons(rast)
      if (!is.null(projection)){ #project if called for, otherwise restore original projection
        poly <- terra::project(poly, projection)
        snapped_pt <- snapped_points[i, ]
        snapped_pt <- terra::project(snapped_pt, projection)
        point <- points[i, ]
        point <- terra::project(point, projection)
      } else {
        poly <- terra::project(poly, original_projection)
        snapped_pt <- snapped_points[i, ]
        snapped_pt <- terra::project(snapped_pt, original_projection)
        point <- points[i, ]
        point <- terra::project(point, original_projection)
      }
      #and now that everything worked, create the directory and populate it
      dir.create(paste0(save_path, "/watersheds_", Sys.Date(), "/", as.data.frame(snapped_pt[1,1])))
      terra::writeVector(snapped_pt, paste0(save_path, "/watersheds_", Sys.Date(), "/", as.data.frame(snapped_pt[1,1]), "/snapped_pour_point.shp"), overwrite=TRUE)
      terra::writeVector(point, paste0(save_path, "/watersheds_", Sys.Date(), "/", as.data.frame(snapped_pt[1,1]), "/input_point.shp"), overwrite=TRUE)
      terra::writeVector(poly, paste0(save_path, "/watersheds_", Sys.Date(), "/", as.data.frame(snapped_pt[1,1]), "/drainage_basin.shp"), overwrite=TRUE)
    }, error = function(e) {
      print(paste0("Failed to delineate watershed for point on row ", i))
    })
  }
} #End of function
