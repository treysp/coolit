library(sf)
library(raster)
library(mapview)
library(data.table)

# get image crs
philly_crs <- brick(
  "data/source_from-philly-website/6in/26453E204934N_6in.tif"
)
philly_crs <- crs(philly_crs)@projargs

# get scores
scores <- readRDS("output/2019-04-18/scored_philly/philly-scores-gt0338-lte1.rds")

# create tower coords sf
tower_coord <- readxl::read_excel(
  "data/philly_tower-coords_grasp/PHLCTList_02082019.xlsx"
)
names(tower_coord) <- tolower(names(tower_coord))

tower_coord <- st_as_sf(tower_coord,
                        coords = c("pointy", "pointx"),
                        agr = "constant")
st_crs(tower_coord) <- 4326

tower_coord <- st_transform(tower_coord, crs = st_crs(scores))

# get img index
philly_index <- st_read("data/source_from-philly-website/PhiladelphiaImagery2018")
philly_index <- st_transform(philly_index, crs = st_crs(scores))

philly_intersect <- st_intersects(philly_index, tower_coord)

philly_index$hastower_coord <- sapply(philly_intersect, function(x) length(x) != 0)

# make plots
# make plot
make_tower_plot <- function(tower_img_name, scores_sf, tower_coord_sf = NULL, outpath) {
  tower_img <- brick(tower_img_name)

  out_name <- stringr::str_match(tower_img_name,
                                 "(.*/)*(.*)\\.(tif|png|jp2)")[, 3]

  png(file.path(outpath, paste0(out_name, "_tower-plot.png")),
      width = ncol(tower_img), height = nrow(tower_img))

  plotRGB(tower_img, maxpixels = 1e10)

  plot(
    scores_sf$geometry[scores_sf$source_img == tower_img_name],
    col = scales::alpha("red", .4),
    add = TRUE
  )

  if (!is.null(tower_coord_sf)) {
    tower_coord_img <- st_crop(tower_coord, st_bbox(tower_img))

    plot(
      st_buffer(tower_coord_img$geometry, 25),
      col = scales::alpha("blue", .4),
      add = TRUE
    )
  }

    dev.off()
}

# subset scores and identify source images that contain a slice
plot_scores <- scores[scores$predicted_probs >= 0.8,]

philly_intersect <- st_intersects(philly_index, plot_scores)

philly_index$hastower_score <- sapply(philly_intersect, function(x) length(x) != 0)

# make vector of image names: those in plot_scores and those
#  with a coord tower but not in plot_scores
plot_img_names <- unique(plot_scores$source_img)

img_path <- stringr::str_match(plot_img_names[1], "(.*/)*(.*)\\.(tif|png|jp2)")[,2]

coord_img_names <- philly_index[philly_index$hastower_coord &
                                  !philly_index$hastower_score,
                                "Name"]
coord_img_names <- paste0(img_path, coord_img_names$Name, "_6in.tif")

plot_img_names <- c(plot_img_names, coord_img_names)

cl <- parallel::makeCluster(parallel::detectCores() - 1)

parallel::clusterEvalQ(cl, {
  library(raster)
  library(stringr)
  library(sf)
  library(scales)
})

parallel::clusterExport(cl, c("plot_scores", "tower_coord", "make_tower_plot"))

pbapply::pblapply(X = plot_img_names, cl = cl, FUN = function(x) {
  make_tower_plot(x,
                  plot_scores,
                  tower_coord,
                  "output/2019-04-18/scored_philly/tower_plots")
})

parallel::stopCluster(cl)