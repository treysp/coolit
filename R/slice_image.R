#' Slice an image into tiles for scoring with a Keras model
#'
#' @param img_path Path to image file that can be read by \code{\link[raster]{brick}}.
#'
#' @param img_xml_wkt_path Optional path to .xml file containing metadata about
#'                         image in WKT format.
#' @param wkt_string Optional WKT string containing image projection information.
#'                   Overridden by `img_xml_wkt_path` if present.
#' @param proj4_string Optional proj4 string containing image projection information.
#'                     Overriden by `img_xml_wkt_path` of `wkt_string` if present.
#'
#' @param tile_n_rows Number of rows in each tile. See \code{\link{calc_tile_corners}}.
#' @param tile_n_cols Number of columns in each tile. See \code{\link{calc_tile_corners}}.
#'
#' @param tile_overlap Number of pixel overlap in adjacent tiles (in both X and Y directions).
#'                       See \code{\link{calc_tile_corners}}.
#'
#' @param complete_image If TRUE and the tile size and overlap dimensions do not conform to
#'                       covering the entire source raster/image, an additional row and column
#'                       of tiles will be created that include the excluded pixels but do NOT
#'                       respect the overlap value. If FALSE and the dimensions do not conform,
#'                       the set of tiles will omit some pixels on the right and bottom side
#'                       of the source raster/image. See \code{\link{calc_tile_corners}}.
#'
#' @param verbose Should messages about current step being processes be printed to screen?
#'
#' @return Data frame with one row for each tile, containing:
#' - \code{img_path}
#' - \code{img_xml_wkt_path} (if present)
#' - Numeric tile ID number
#' - Tile corner cells in source jp2 pixel values
#' - List-column containing each tile's extent based on the source jp2's projected raster
#' - sf geometry column containing each tile's sf polygon
#' - List-column containing tile's RGB layers in a 4d array of dimension
#' [1, \code{tile_n_cols}, \code{tile_n_rows}, 3]
#'
#' @export
#' @importFrom xml2 as_list read_xml
#' @importFrom sf st_crs st_sfc st_polygon
#' @importFrom raster brick nlayers dropLayer extent crs
#' @importFrom pbapply pblapply pboptions
#' @importFrom abind abind
slice_image <- function(img_path, img_xml_wkt_path = NULL,
                        wkt_string = NULL, proj4_string = NULL,
                        project_raster = FALSE,
                        tile_n_rows, tile_n_cols,
                        tile_overlap = 0, complete_image = FALSE,
                        verbose = FALSE) {
  if (!verbose) {
    opb <- pbapply::pboptions(type="none")
    on.exit(pboptions(opb))
  }

  # read image
  if (verbose) message("Reading image.")

  source_brick <- raster::brick(img_path)

  if (nlayers(source_brick) > 3) {
    for (i in seq_len(nlayers(source_brick) - 3)) {
      source_brick <- raster::dropLayer(source_brick, i + 3)
    }
  }

  # modify CRS if requested
  if (!is.null(img_xml_wkt_path) ||
      !is.null(wkt_string) ||
      !is.null(proj4_string)) {
    if (!is.null(img_xml_wkt_path)) {

      jp2_xml <- xml2::as_list(xml2::read_xml(jp2_aux_path))

      my_proj4_string <- sf::st_crs(wkt = jp2_xml$PAMDataset$SRS[[1]])$proj4string

    } else if (!is.null(wkt_string)) {

      my_proj4_string <- sf::st_crs(wkt = wkt_string)$proj4string

    } else if (!is.null(proj4_string)) {

      my_proj4_string <- proj4_string

    }

    if (project_raster == TRUE) {
      source_brick <- projectRaster(
        source_brick,
        res = res(source_brick),
        crs = my_proj4_string
      )
    } else {
      crs(source_brick) <- my_proj4_string
    }
  }

  # create raster extents and sf polygons for each tile
  tile_data <- calc_tile_corners(
    source_n_rows = nrow(source_brick),
    source_n_cols = ncol(source_brick),
    tile_n_rows = tile_n_rows,
    tile_n_cols = tile_n_cols
  )

  tile_data <- cbind(source_img = img_path,
                     source_img_aux = img_xml_wkt_path,
                     tile_id = seq_len(nrow(tile_data)),
                     tile_data,
                     stringsAsFactors = FALSE)

  tile_extents <- split(tile_data, 1:NROW(tile_data))

  if (verbose) message("Creating extents:")
  tile_data$extents <- pblapply(tile_extents, function(x) {
    extent(source_brick, x[["y0"]], x[["y1"]], x[["x0"]], x[["x1"]])
  })

  if (verbose) message("Creating polygons:")
  tile_data$geometry <- pblapply(tile_data$extents, function(x) {
    st_sfc(
      st_polygon(
        list(
          rbind(
            c(x@xmin, x@ymin),
            c(x@xmin, x@ymax),
            c(x@xmax, x@ymax),
            c(x@xmax, x@ymin),
            c(x@xmin, x@ymin)
          )
        )
      ),
      crs = crs(source_brick)@projargs
    )
  })

  # create a 4d array for each tile, containing 1 [50, 50, 3] slice
  if (verbose) message("Converting image to array and slicing - may take a minute.")
  source_brick_data <- raster::as.array(source_brick)

  tile_data$tile_array <- lapply(1:nrow(tile_data), function(i) {
    out <- array(NA, c(1, tile_n_cols, tile_n_rows, 3))

    out[1, , ,] <- source_brick_data[
      seq(tile_data[["y0"]][i], tile_data[["y1"]][i]),
      seq(tile_data[["x0"]][i], tile_data[["x1"]][i])
      ,
      ]

    out
  })

  # # must transpose each layer to reassemble original brick
  # temp <- crop(source_brick, tile_data$extents[[1]])
  # plotRGB(temp)
  #
  # temp_array <- drop(tile_data$tile_array[[1]])
  # temp_array[,,1] <- t(temp_array[,,1])
  # temp_array[,,2] <- t(temp_array[,,2])
  # temp_array[,,3] <- t(temp_array[,,3])
  # plotRGB(temp)

  tile_data
}