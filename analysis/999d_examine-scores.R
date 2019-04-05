library(raster)
library(sf)
library(dplyr)
library(ggplot2)
library(data.table)

# # compile tile scores and save
# score_dir <- "F:/coolit/output/2019-03-31/scored_nyc_pre-sliced"
# scores <- pbapply::pblapply(list.files(score_dir, full.names = TRUE), function(x) {
#   out <- readRDS(x)
#   out <- out[["tile_data"]]
#
#   out$source_img_aux <- NULL
#   out$tile_id <- NULL
#   out$x0 <- NULL
#   out$x1 <- NULL
#   out$y0 <- NULL
#   out$y1 <- NULL
#   out$extents <- NULL
#
#   out
#   })
#
# scores_df <- bind_rows(scores) %>%
#   mutate(
#     unique_id = paste0(
#       stringr::str_match(source_img, "(.*/)*(.*?)\\.jp2")[, 3],
#       "_",
#       tile_id
#       ),
#
#     row_num = row_number()
#     )
#
# scores_df$geometry <- do.call(c, scores_df$geometry)
# scores_df <- st_sf(scores_df)
#
# saveRDS(scores_df, file.path(score_dir, "collated_scores.rds"))
# saveRDS(scores_df[scores_df$predicted_probs >= .001, ],
#         file.path(score_dir, "collated_scores001.rds"))

score_dir <- "F:/coolit/output/2019-03-31/scored_nyc_pre-sliced"
scores_df <- readRDS(file.path(score_dir, "collated_scores001.rds"))

# get NYC tower shapefile
tower_shp <- st_read("C:/Users/wfu3-su/Desktop/tower-id/data/source_from-nyc-website/nyc_cooling-tower_shapefile")
tower_shp <- st_transform(tower_shp, crs = st_crs(scores_df$geometry)$proj4string)
tower_shp$num_id <- seq_len(nrow(tower_shp))

score_tile_intersect <- st_intersects(scores_df, tower_shp)
scores_df$tower_intersect <- sapply(score_tile_intersect,
                                    function(x) if (length(x) == 0) FALSE else TRUE)

scores_dt <- data.table(scores_df)
scores_dt[, pred_prob01 := round(predicted_probs, 2)]

my_means <- scores_dt[, list(
  mn_tower_intersect = mean(tower_intersect),
  ct_tower_intersect = sum(tower_intersect)
  ), by = pred_prob01]

ggplot(data = my_means) +
  geom_line(aes(x = pred_prob01, y = mn_tower_intersect)) +
  xlab("Predicted prob. of tower") +
  ylab("Prop. of tiles with tower") +
  theme_minimal()
