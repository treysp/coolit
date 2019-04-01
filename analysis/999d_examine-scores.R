library(raster)
library(sf)
library(dplyr)
library(ggplot2)

# get tile scores
score_dir <- "F:/coolit/output/2019-03-31/scored_nyc_pre-sliced"
scores <- pbapply::pblapply(list.files(score_dir, full.names = TRUE)[600:700], function(x) {
  out <- readRDS(x)
  out <- out[["tile_data"]]
  out$extents <- NULL
  out
  })

scores_df <- bind_rows(scores) %>%
  mutate(
    unique_id = paste0(
      stringr::str_match(source_img, "(.*/)*(.*?)\\.jp2")[, 3],
      "_",
      tile_id
      ),

    num_id = row_number()
    )

scores_df$geometry <- do.call(c, scores_df$geometry)
scores_df <- st_sf(scores_df)

# get NYC tower shapefile
tower_shp <- st_read("C:/Users/wfu3-su/Desktop/tower-id/data/source_from-nyc-website/nyc_cooling-tower_shapefile")
tower_shp <- st_transform(tower_shp, crs = st_crs(scores_df$geometry)$proj4string)
tower_shp$num_id <- seq_len(nrow(tower_shp))

tower_tile_intersect <- st_intersects(tower_shp, scores_df)
tower_tile_intersect <- lapply(seq_along(tower_tile_intersect), function(i) {
  if (length(tower_tile_intersect[[i]]) == 0) {
    return(NULL)
  } else {
    data.frame(
      tower_row = rep(i, length(tower_tile_intersect[[i]])),
      tile_row = tower_tile_intersect[[i]]
      )
  }
})

tower_tile_intersect <- bind_rows(tower_tile_intersect)

scores_df <- left_join(scores_df, tower_tile_intersect, by = c("num_id" = "tile_row"))
scores_df_bounds <- st_union(scores_df$geometry)

table(sapply(tower_tile_intersect, length))

ggplot(data = scores_df) +
  geom_histogram(aes(x = predicted_probs), binwidth = .001)
