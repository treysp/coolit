library(coolit)
library(tensorflow)
library(coolit)
library(keras)
library(ggplot2)
library(magick)
library(ROCR)
library(raster)
library(sf)

# score train/test images
# my_model <- load_model_hdf5(
#   "output/multi-model-runs/2019-04-09/models/2019-04-09_16-17-24/model_fine-tune-2.h5"
#   )
#
# temp_scores <- score_test_images(
#   c("data/tiles_nyc/validation", "data/tiles_nyc/train"),
#   my_model,
#   out_filename = "output/multi-model-runs/2019-04-09/models/2019-04-09_16-17-24/predicted-probs_train-validation.csv"
#   )

# check some scores
temp_scores <- read.csv(
  paste0("output/multi-model-runs/2019-04-09/models/",
         "2019-04-09_16-17-24/predicted-probs_train-validation.csv")
)

pred <- ROCR::prediction(temp_scores$pred_prob, temp_scores$truth)

message("\nFinal model performance measures:\n",
        "   ROC AUC = ",
        round(performance(pred, "auc")@y.values[[1]], 3), "\n",
        "   Max possible accuracy = ",
        round(max(performance(pred, "acc")@y.values[[1]]), 3)
)

ggplot(temp_scores) +
  geom_histogram(aes(x = pred_prob, y = ..count../sum(..count..),
                     fill = factor(truth)),
                 binwidth = .001, color = NA, alpha = .4) +
  scale_fill_brewer(palette = "Dark2",
                    guide = guide_legend(title = "Truth")) +
  ggtitle("Model predicted probabilities for test set, by actual tower presence") +
  xlab("Predicted probability") +
  ylab("Proportion of all images") +
  theme_minimal()

# examine lowprob towers and higprob non-towers
lowprob_towers <- temp_scores[
  temp_scores$pred_prob < 0.1 & temp_scores$truth == 1,
  ]

lowprob_towers <- lowprob_towers[
  order(-lowprob_towers$pred_prob),
  ]

for (i in 1:((nrow(lowprob_towers) %/% 24) + 1)) {
  if (i == (nrow(lowprob_towers) %/% 24) + 1) {
    index <- nrow(lowprob_towers) - (i - 1) * 24
  } else {
    index <- 24
  }

  image_read(lowprob_towers$img_name[(i - 1) * 24 + 1:index]) %>%
    image_montage() %>%
    image_convert("png") %>%
    image_write(file.path("c:/users/wfu3/desktop/temp",
                          paste0("lowprob_tower_", i, ".png")))
}

highprob_notowers <- temp_scores[
  temp_scores$pred_prob >= 0.1 & temp_scores$truth == 0,
  ]

highprob_notowers <- highprob_notowers[
  order(-highprob_notowers$pred_prob),
  ]

for (i in 1:((nrow(highprob_notowers) %/% 24) + 1)) {
  if (i == (nrow(highprob_notowers) %/% 24) + 1) {
    index <- nrow(highprob_notowers) - (i - 1) * 24
  } else {
    index <- 24
  }

  image_read(highprob_notowers$img_name[(i - 1) * 24 + 1:index]) %>%
    image_montage() %>%
    image_convert("png") %>%
    image_write(file.path("c:/users/wfu3/desktop/temp",
                          paste0("highprob_notower_", i, ".png")))
}

# make raster overlays
overlay_towers <- rbind(
  highprob_notowers[grepl("990215", highprob_notowers$img_name),],
  lowprob_towers[grepl("990215", highprob_notowers$img_name),]
)
overlay_towers$tile_id <- as.numeric(stringr::str_match(
  overlay_towers$img_name, "(.*/)\\d*_(\\d*)\\.png"
  )[, 3]
  )

overlay_towers_sf <- readRDS("output/2019-03-31/sliced_nyc/990215_slices.rds")
overlay_towers_sf$geometry <- st_sfc(do.call("c", overlay_towers_sf$geometry))
overlay_towers_sf <- st_sf(overlay_towers_sf)

overlay_towers_sf <- merge(overlay_towers_sf, overlay_towers, by = "tile_id")

make_raster_overlay(
  base_raster = "data/source_from-nyc-website/nyc_ortho_jp2/boro_manhattan_sp16/990215.jp2",
  polygon_sf = overlay_towers_sf,
  outfilename = "c:/users/wfu3/desktop/test.tif",
  write_only = TRUE
)

