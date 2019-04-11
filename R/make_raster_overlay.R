#' make overlay of towers on image
#'
#' @param base_raster Either a path to a jp2 image with paired .aux.xml file
#' or an existing rasterBrick or rasterStack object with valid CRS
#'
#' @param polygon_sf SF data frame containing polygons to overlay on the
#' raster in the red layer
#'
#' @param outfilename Either file path for output raster or NULL
#'
#' @param write_only TRUE for only saving raster to file, FALSE for returning
#' overlayed raster object
#'
#' @importFrom raster brick dropLayer crs plotRGB
#' overlay writeRaster
#' @importFrom sf st_crs
#' @importFrom xml2 as_list read_xml
#' @importFrom lwgeom st_transform_proj
#' @importFrom fasterize fasterize
make_raster_overlay <- function(base_raster, polygon_sf,
                                outfilename = NULL, write_only = FALSE) {
  if (!("raster" %in% class(base_raster)) &&
      is.character(base_raster) &&
      length(base_raster) == 1) {

    base_raster_path <- base_raster

    base_raster <- brick(base_raster)
    base_raster <- dropLayer(base_raster, 4)

    jp2_xml <- xml2::as_list(xml2::read_xml(paste0(base_raster_path, ".aux.xml")))

    crs(base_raster) <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string
  }

  # rasterize
  polygon_sf <- lwgeom::st_transform_proj(polygon_sf,
                                          crs = crs(base_raster)@projargs)

  polygon_sf$cell_val <- 250
  overlay_raster <- fasterize(polygon_sf, base_raster[[1]], "cell_val")

  base_raster[[1]] <- overlay(base_raster[[1]],
                              overlay_raster,
                              fun = function(x, y) pmax(x, y, na.rm = TRUE))

  plotRGB(base_raster)

  if (!is.null(outfilename)) {
    writeRaster(
      base_raster,
      outfilename,
      datatype = "INT1U",
      overwrite = TRUE
    )
  }

  if (write_only) {
    return(NULL)
  } else {
    return(base_raster)
  }
}