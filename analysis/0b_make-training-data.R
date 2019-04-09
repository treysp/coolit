library(stringr)
library(raster)
library(sf)
library(magick)

set.seed(1)

# make list of slice files containing towers
slice_dir <- "output/2019-03-31/sliced_nyc"

img_tower <- readRDS("output/2019-04-06/img-index_has-towers_2019-04-08.rds")
img_tower$img_num <- as.character(img_tower$IMAGE)
img_tower$img_num <- gsub("\\.jp2", "", img_tower$img_num)
img_tower <- unique(img_tower$img_num)

slice_rds <- list.files(slice_dir, full.names = TRUE)
slice_rds <- slice_rds[sapply(img_tower, function(x) str_which(slice_rds, x))]

# make list of slices to exclude due to obstruction
slice_exclude <- list.dirs("output/2019-04-06/low-prob-tower-slices",
                            recursive = TRUE, full.names = TRUE)
slice_exclude <- slice_exclude[str_detect(slice_exclude, "/obstruction")]
slice_exclude <- fs::dir_ls(slice_exclude, type = "file")
slice_exclude <- fs::path_file(slice_exclude)
slice_exclude <- str_replace(slice_exclude, "\\.png", "")
slice_exclude <- str_split(slice_exclude, "_")
slice_exclude <- do.call("rbind", slice_exclude)
slice_exclude <- data.frame(img_num = slice_exclude[, 1],
                            tile_id = as.numeric(slice_exclude[, 2]),
                            stringsAsFactors = FALSE)
slice_exclude$exclude <- TRUE

# get NYC shapefiles
tower_shp <- st_read("data/source_from-nyc-website/nyc_cooling-tower_shapefile")

water_shp <- st_read("data/source_from-nyc-website/nyc_hydrography_shapefile")

# create directories
base_dir <- "data/tiles_nyc"

dir.create(file.path(base_dir, "train"))
dir.create(file.path(base_dir, "train/tower"))
dir.create(file.path(base_dir, "train/notower"))
dir.create(file.path(base_dir, "validation"))
dir.create(file.path(base_dir, "validation/tower"))
dir.create(file.path(base_dir, "validation/notower"))
dir.create(file.path(base_dir, "test"))
dir.create(file.path(base_dir, "test/tower"))
dir.create(file.path(base_dir, "test/notower"))
dir.create(file.path(base_dir, "notower"))

# create slice images
ncores <- parallel::detectCores() - 2
cl <- parallel::makeCluster(ncores, outfile = "c:/users/wfu3/desktop/log.txt")
parallel::clusterEvalQ(cl, {
  library(stringr)
  library(sf)
  library(magick)
})
parallel::clusterExport(cl, c("slice_exclude", "base_dir", "tower_shp"))

num_towers <- pbapply::pblapply(X = slice_rds, cl = cl, FUN = function(rds) {
  working <- readRDS(rds)
  working$img_num <- str_match(working$source_img, "(.*/)(.*)\\.jp2")[, 3]
  working <- merge(working, slice_exclude,
                   by = c("img_num", "tile_id"),
                   all.x = TRUE)
  working <- working[is.na(working$exclude), ]
  working$geometry <- do.call(c, working$geometry)
  working <- st_sf(working)

  # identify slices that intersect with tower polygons
  tower_shp <- st_transform(tower_shp, crs = st_crs(working)$proj4string)

  tile_intersect <- st_intersects(working, tower_shp)
  working$tower_intersect <- sapply(tile_intersect,
                                    function(x) if (length(x) == 0) FALSE else TRUE)
  rm(tile_intersect)

  tower <- working[working$tower_intersect == TRUE, ]

  # save slices
  if (nrow(tower) > 0) {
    # split towers across train/valid/test
    tower$status <- sample(c("train", "validation", "test"),
                           size = nrow(tower),
                           prob = c(.7, .2, .1),
                           replace = TRUE)
    tower$out_name <- paste0(base_dir, "/",
                             tower$status, "/",
                             "tower/",
                             tower$img_num, "_",
                             tower$tile_id, ".png")

    # identify slices that are contained by water polygons
    notower <- working[working$tower_intersect == FALSE, ]

    water_shp <- st_transform(water_shp, crs = st_crs(working)$proj4string)

    water_covers <- st_covered_by(notower, water_shp)
    notower$water_covered <- sapply(water_covers,
                                    function(x) if (length(x) == 0) FALSE else TRUE)
    rm(water_covers)

    notower <- notower[notower$water_covered == FALSE,]

    # write all tower slices to `notower` directory so we can sample them later
    dir.create(file.path(base_dir, "notower", unique(notower$img_num)))

    notower$out_name <- paste0(base_dir, "/",
                               "notower/",
                               notower$img_num, "/",
                               notower$img_num, "_",
                               notower$tile_id, ".png")

    lapply(list(tower, notower), function(x) {
      lapply(seq_len(nrow(x)), function(i) {
        magick::image_write(
          image = image_read(drop(x[i, "tile_array", drop = TRUE][[1]]) / 255),
          path = x[i, "out_name", drop = TRUE])
      })
    })
  }

  nrow(tower)
})

parallel::stopCluster(cl)

# sample notower slices proportional to tower count

