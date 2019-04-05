library(sf)
library(dplyr)
library(raster)
library(fasterize)

# buildings
# in_path <- "data/source_from-nyc-website/nyc_building-footprint_shapefile"
#
# nyc_building_orig <- st_read(in_path, layer = "building")
# names(nyc_building_orig) <- tolower(names(nyc_building_orig))
#
# nyc_building <- nyc_building_orig %>%
#   filter(feat_code == 2100)
#
# saveRDS(nyc_building, "data/source_from-nyc-website/nyc_building-footprint.rds", compress = FALSE)

nyc_building <- readRDS("data/source_from-nyc-website/nyc_building-footprint.rds")

# towers
nyc_towers_orig <- st_read("data/source_from-nyc-website/nyc_cooling-tower_shapefile")
nyc_towers <- st_transform(nyc_towers_orig, crs = st_crs(nyc_building))

tower_in_bldg <- st_intersects(nyc_towers, nyc_building)

nyc_towers$bldg_int <- sapply(tower_in_bldg, function(x) length(x) != 0)

nyc_towers_bad <- nyc_towers %>% filter(bldg_int == FALSE)

# nyc jp2 manifest
nyc_tiles <- st_read("data/source_from-nyc-website/nyc_ortho_jp2/2016 Orthos Tile Index")
nyc_tiles <- st_transform(nyc_tiles, crs = st_crs(nyc_building))

nyc_tiles <- st_crop(nyc_tiles, st_bbox(nyc_towers))

nyc_tiles_bad_tower <- st_intersects(nyc_tiles, nyc_towers_bad)

nyc_tiles$has_bad_tower <- sapply(nyc_tiles_bad_tower, function(x) length(x) > 0)

nyc_tiles_bad <- nyc_tiles %>% filter(has_bad_tower == TRUE)

# make overlay of towers on image
make_raster_overlay <- function(base_raster, polygon_sf, outfilename = NULL) {
  if (!is.raster(base_raster) &&
      is.character(base_raster) &&
      length(base_raster) == 1) {

    base_raster <- brick(base_raster)
    base_raster <- dropLayer(img, 4)

    jp2_xml <- xml2::as_list(xml2::read_xml(paste0(base_raster, ".aux.xml")))

    crs(base_raster) <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string
  }

  # rasterize
  polygon_sf <-  <- st_transform(polygon_sf, crs = st_crs(base_raster))

  polygon_sf$cell_val <- 250
  overlay_raster <- fasterize(polygon_sf, base_raster[[1]], "cell_val")

  out <- base_raster
  base_raster[[1]] <- max(out[[1]], overlay_raster, na.rm = TRUE)

  plotRGB(out)

  if (!is.null(outfilename)) {
    writeRaster(
      out,
      outfilename,
      datatype = "INT1U",
      overwrite = TRUE
    )
  }

  out
}

bad_tower_img <- as.character(unique(nyc_tiles_bad$IMAGE))
jp2_paths <- list.files("data/source_from-nyc-website/nyc_ortho_jp2/",
                        pattern = "\\.jp2$",
                        recursive = TRUE,
                        full.names = TRUE)
bad_tower_paths <- sapply(bad_tower_img, function(x) {
  jp2_paths[grepl(x, jp2_paths)][1]
})


make_raster_overlay(base_raster = bad_tower_paths[1], nyc_towers_bad)

# for MS building polygons
test <- data.table::fread("c:/users/wfu3/desktop/NewYork/nyc_microsoft_orig.csv",
                          sep = NULL)
names(test) <- "wkt"
st_as_sf(test[1:5], wkt = "wkt")