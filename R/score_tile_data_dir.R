#' Score a directory of tile data files with a Keras model
#'
#' @param tile_data_dir Directory containing sliced image .rds files
#'
#' @param model_h5_weights Saved h5 weights from a trained keras model, used to score
#'                         the image tiles.
#' @param model_params_dput_file Path to file containing \code{dput} export of parameters
#' used during training of the model contained in `model_h5_weights`.
#'
#' @param score_outdir Path to directory where results of image scoring should be save
#' @param compress_score_rds Should saved score results be compressed
#' @param return_score Should list of score results be returned as an R object by the function
#'
#' @param verbose Should messages about current step being processes be printed to screen?
#'
#' @return List of data frame with one row for each tile, containing:
#' List of lists containing model parameters and input `img_data` data frame with
#' additional column containing model predicted probabilities for each slice
#'
#' If \code{score_outdir != NULL} each list will be saved to score_outdir as an
#' RDS file, compressed if \code{compress_score_rds == TRUE}.
#'
#' @export
#' @importFrom pbapply pblapply
#' @importFrom keras load_model_hdf5
score_tile_data_dir <- function(tile_data_dir,
                                model_params_dput_file, model_h5_weights,
                                score_outdir = NULL, compress_score_rds = FALSE,
                                return_score = TRUE, verbose = FALSE) {
  tile_data_files <- list.files

  images_to_score <- split(jp2_file_names, 1:nrow(jp2_file_names))

  # load model and score
  my_model_params <- eval(parse(model_params_dput_file))
  scoring_model <- load_model_hdf5(model_h5_weights)

  out <- pblapply(images_to_score, function(x) {

    tile_data <- readRDS(x)

    score_tile_data(
      tile_data = tile_data,
      model_params = my_model_params,
      scoring_model = scoring_model,
      score_outpath = file.path(score_outdir, paste0("img_", x[["stub"]], "_scores.rds")),
      return_score = return_score,
      verbose = verbose
    )

  })

  out
}