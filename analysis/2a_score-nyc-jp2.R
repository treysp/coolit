library(keras)
library(pbapply)
library(ggplot2)
library(stringr)
library(ROCR)
library(abind)
library(sf)
library(tidyr)
library(raster)
source("model-fun.R")
source("util.R")

test <- score_jp2_image_dir(
  jp2_dir = "data/source_from-nyc-website/boro_manhattan_sp16",
  jp2_aux_dir = "data/source_from-nyc-website/boro_manhattan_sp16",
  model_params_dput_file = "output/multi-model-runs/2019-03-04/models/2019-03-04_20-49-33/run-parameters_dput.txt",
  model_h5_weights = "output/multi-model-runs/2019-03-04/models/2019-03-04_20-49-33/model_fine-tune-2.h5",
  score_outdir = "output/2019-03-20/scored_manhattan",
  return_score = FALSE
)

# examine scores
yes_tiles <- tile_data[tile_data$predicted_probs > .2,]
yes_tiles <- yes_tiles[order(-yes_tiles$predicted_probs), ]

par(mfrow = c(10, 10))

for (i in 196:295) {
  plotRGB(brick(drop(yes_tiles$tile_array[[i]])))
}
