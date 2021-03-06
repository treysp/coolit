% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_jp2_image.R
\name{score_jp2_image}
\alias{score_jp2_image}
\title{Score a jp2 image with a Keras model}
\usage{
score_jp2_image(model_params, scoring_model, jp2_path, jp2_aux_path,
  score_outpath = NULL, compress_score_rds = FALSE,
  return_score = TRUE, verbose = FALSE, tile_overlap = 0,
  complete_image = FALSE)
}
\arguments{
\item{model_params}{Model parameters object created during model training}

\item{scoring_model}{Trained keras model object}

\item{jp2_path}{Path to .jp2 image files}

\item{jp2_aux_path}{Path to .jp2.aux.xml file containing metadata about .jp2 image}

\item{score_outpath}{Path to save results of image scoring as .rds file}

\item{compress_score_rds}{Should saved score results be compressed}

\item{return_score}{Should score results be returned as an R object by the function}

\item{verbose}{Should messages about current step being processes be printed to screen?}

\item{tile_overlap}{Number of pixel overlap in adjacent tiles (in both X and Y directions).
See \code{\link{calc_slice_corners}}.}

\item{complete_image}{If TRUE and the tile size and overlap dimensions do not conform to
covering the entire source raster/image, an additional row and column
of tiles will be created that include the excluded pixels but do NOT
respect the overlap value. If FALSE and the dimensions do not conform,
the set of tiles will omit some pixels on the right and bottom side
of the source raster/image. See \code{\link{calc_slice_corners}}.}
}
\value{
Data frame with one row for each tile, containing:
- \code{jp2_path}
- \code{jp2_aux_path}
- Numeric tile ID number
- Tile corner cells in source jp2 pixel values
- List-column containing each tile's extent based on the source jp2's projected raster
- sf geometry column containing each tile's sf polygon
- List-column containing tile's RGB layers in a 4d array of dimension
\[1, \code{tile_n_cols}, \code{tile_n_rows}, 3\]
- Model predicted probability for each image tile

If score \code{outpath != NULL} returned data frame will be saved to outpath as an
RDS file, compressed if \code{compress_score_rds == TRUE}.
}
\description{
Score a jp2 image with a Keras model
}
