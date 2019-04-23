library(stringr)
library(raster)
library(sf)
library(magick)

set.seed(1)

# PARAMETER: total number randomly selected notower images to use
num_notower <- 10000

# where images should live
base_dir <- "data/model-training-data/slices_curated_2019-04-22"

for (i in c("train", "validation", "test")) {

  dir.create(file.path(base_dir, i))
  dir.create(file.path(base_dir, i, "tower"))
  dir.create(file.path(base_dir, i, "notower"))

}

dir.create(file.path(base_dir, "notower"))

#### NYC preparation --------------------------------------------------------------------
# get obstructed tower slice list
nyc_obstructed <- readr::read_csv(
  file.path("data/curated-training-slices/nyc/nyc-towers",
            "nyc_obstructed-tower-slice-list_2019-04-23.csv"))

nyc_obstructed$exclude <- TRUE

# get tower slices
nyc_tower_slices <- lapply(
  list.files("data/curated-training-slices/nyc/nyc-towers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

nyc_tower_slices <- do.call("rbind", nyc_tower_slices)

nyc_tower_slices$img_num <- str_match(nyc_tower_slices$source_img,
                                      "(.*/)(\\d*)\\.jp2")[, 3]

# remove obstructed towers
nyc_tower_slices <- merge(nyc_tower_slices,
                          nyc_obstructed[, c("img_num", "tile_id", "exclude")],
                          by = c("img_num", "tile_id"),
                          all.x = TRUE)

nyc_tower_slices <- nyc_tower_slices[is.na(nyc_tower_slices$exclude),]
nyc_tower_slices$exclude <- NULL

# assign train/validation/test status
nyc_tower_slices$status <- sample(c("train", "validation", "test"),
                                  size = nrow(nyc_tower_slices),
                                  prob = c(.7, .2, .1),
                                  replace = TRUE)

nyc_tower_slices$out_name <- paste0(base_dir, "/",
                                    nyc_tower_slices$status, "/",
                                    "tower/",
                                    nyc_tower_slices$img_num, "_",
                                    nyc_tower_slices$tile_id, ".png")

nyc_tower_index <- nyc_tower_slices[, c("img_num", "tile_id")]
nyc_tower_index$exclude <- TRUE

# count tower/img for notower sampling
nyc_tower_img_count <- table(nyc_tower_slices$img_num)
nyc_tower_img_count <- data.frame(
  img_num = names(nyc_tower_img_count),
  count = as.integer(nyc_tower_img_count),
  stringsAsFactors = FALSE
)

nyc_tower_img_count$prop <- nyc_tower_img_count$count /
  sum(nyc_tower_img_count$count)

# get high-prob notower slices
nyc_highprob <- lapply(
  list.files("data/curated-training-slices/nyc/nyc-highprob-notowers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

nyc_highprob <- do.call("rbind", nyc_highprob)

nyc_highprob$img_num <- str_match(nyc_highprob$source_img,
                                  "(.*/)(\\d*)\\.jp2")[, 3]

nyc_highprob$status <- sample(c("train", "validation", "test"),
                              size = nrow(nyc_highprob),
                              prob = c(.7, .2, .1),
                              replace = TRUE)

nyc_highprob$out_name <- paste0(base_dir, "/",
                                nyc_highprob$status, "/",
                                "notower/",
                                nyc_highprob$img_num, "_",
                                nyc_highprob$tile_id, ".png")

#### Philly preparation --------------------------------------------------------------------
# get tower slices
philly_tower_slices <- lapply(
  list.files("data/curated-training-slices/philly/philly-towers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

philly_tower_slices <- do.call("rbind", philly_tower_slices)

philly_tower_slices$img_num <- str_match(philly_tower_slices$source_img,
                                         "(.*/)(.*)\\.tif")[, 3]

# assign train/validation/test status
philly_tower_slices$status <- sample(c("train", "validation", "test"),
                                     size = nrow(philly_tower_slices),
                                     prob = c(.7, .2, .1),
                                     replace = TRUE)

philly_tower_slices$out_name <- paste0(base_dir, "/",
                                       philly_tower_slices$status, "/",
                                       "tower/",
                                       philly_tower_slices$img_num, "_",
                                       philly_tower_slices$tile_id, ".png")

# count tower/img for notower sampling
philly_tower_img_count <- table(philly_tower_slices$img_num)
philly_tower_img_count <- data.frame(
  img_num = names(philly_tower_img_count),
  count = as.integer(philly_tower_img_count),
  stringsAsFactors = FALSE
)

philly_tower_img_count$prop <- philly_tower_img_count$count /
  sum(philly_tower_img_count$count)

# get high-prob notower slices
philly_highprob <- lapply(
  list.files("data/curated-training-slices/philly/philly-highprob-notowers",
             full.names = TRUE, pattern = "\\.rds"),
  readRDS
)

philly_highprob <- do.call("rbind", philly_highprob)

philly_highprob$img_num <- str_match(philly_highprob$source_img,
                                  "(.*/)(\\d*)\\.tif")[, 3]

philly_highprob$status <- sample(c("train", "validation", "test"),
                                 size = nrow(philly_highprob),
                                 prob = c(.7, .2, .1),
                                 replace = TRUE)

philly_highprob$out_name <- paste0(base_dir, "/",
                                   philly_highprob$status, "/",
                                   "notower/",
                                   philly_highprob$img_num, "_",
                                   philly_highprob$tile_id, ".png")

#### write images ---------------------------------------------------------------------
# setup cluster
ncores <- parallel::detectCores() - 1

cl <- parallel::makeCluster(ncores, outfile = "c:/users/wfu3/desktop/log.txt")

parallel::clusterEvalQ(cl, {
  library(stringr)
  library(sf)
  library(magick)
})

parallel::clusterExport(cl, c("slice_exclude", "base_dir",
                              "tower_shp", "water_shp"))

# tower images
## nyc
tower_index <- rep(1:ncores, each = nrow(nyc_tower_slices) %/% ncores)
tower_index <- c(
  tower_index,
  rep(tail(tower_index, 1), nrow(nyc_tower_slices) - length(tower_index))
)

nyc_tower_list <- split(nyc_tower_slices, tower_index)

pbapply::pblapply(X = nyc_tower_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

## philly
tower_index <- rep(1:ncores, each = nrow(philly_tower_slices) %/% ncores)
tower_index <- c(
  tower_index,
  rep(tail(tower_index, 1), nrow(philly_tower_slices) - length(tower_index))
)

philly_tower_list <- split(philly_tower_slices, tower_index)

pbapply::pblapply(X = philly_tower_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

# highprob notower images
## nyc
highprob_index <- rep(1:ncores, each = nrow(nyc_highprob) %/% ncores)
highprob_index <- c(
  highprob_index,
  rep(tail(highprob_index, 1), nrow(nyc_highprob) - length(highprob_index))
)

nyc_highprob_list <- split(nyc_highprob, highprob_index)

pbapply::pblapply(X = nyc_highprob_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

## philly
highprob_index <- rep(1:ncores, each = nrow(philly_highprob) %/% ncores)
highprob_index <- c(
  highprob_index,
  rep(tail(highprob_index, 1), nrow(philly_highprob) - length(highprob_index))
)

philly_highprob_list <- split(philly_highprob, highprob_index)

pbapply::pblapply(X = philly_highprob_list, cl = cl, FUN = function(x) {
  for (i in seq_len(nrow(x))) {
    magick::image_write(
      image = image_read(drop(x[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = x[i, "out_name", drop = TRUE])
  }
})

# sample and write notower images
tower_props <- c(nyc = nrow(nyc_tower_slices),
                 philly = nrow(philly_tower_slices))

tower_props <- tower_props / sum(tower_props)

## nyc
nyc_tower_img_count$num_to_write <- ceiling(
  nyc_tower_img_count$prop * num_notower * tower_props[["nyc"]]
)

nyc_tower_img_count_list <- split(
  nyc_tower_img_count,
  nyc_tower_img_count$img_num
  )

pbapply::pblapply(X = nyc_tower_img_count_list, cl = cl, FUN = function(rds) {
  working_name <- file.path("data/curated-training-slices/nyc/nyc-slices",
                            paste0(unique(x$img_num), "_slices.rds"))

  working <- readRDS(working_name)

  working$img_num <- str_match(working$source_img,
                               "(.*/)(\\d*)\\.jp2")[, 3]

  working <- merge(working,
                   nyc_tower_index,
                   by = c("img_num", "tile_id"),
                   all.x = TRUE)
  working <- working[is.na(working$exclude), ]

  num_to_write <- nyc_tower_img_count$num_to_write[nyc_tower_img_count$img_num %in% x]

  working <- working[sample(seq_len(nrow(working)), num_to_write), ]

  working$status <- sample(c("train", "validation", "test"),
                                   size = nrow(working),
                                   prob = c(.7, .2, .1),
                                   replace = TRUE)

  working$out_name <- paste0(base_dir, "/",
                             working$status, "/",
                             "notower/",
                             working$img_num, "_",
                             working$tile_id, ".png")

  for (i in seq_len(nrow(working))) {
    magick::image_write(
      image = image_read(drop(working[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = working[i, "out_name", drop = TRUE])
  }
})

## philly
philly_tower_img_count$num_to_write <- ceiling(
  philly_tower_img_count$prop * num_notower * tower_props[["philly"]]
)

philly_tower_img_count_list <- split(
  philly_tower_img_count,
  philly_tower_img_count$img_num
)

pbapply::pblapply(X = philly_tower_img_count_list, cl = cl, FUN = function(rds) {
  working_name <- file.path("data/curated-training-slices/philly/philly-slices",
                            paste0(unique(x$img_num), "_slices.rds"))

  working <- readRDS(working_name)

  working$img_num <- str_match(working$source_img,
                               "(.*/)(\\d*)\\.tif")[, 3]

  working <- merge(working,
                   philly_tower_index,
                   by = c("img_num", "tile_id"),
                   all.x = TRUE)
  working <- working[is.na(working$exclude), ]

  num_to_write <- philly_tower_img_count$num_to_write[
    philly_tower_img_count$img_num %in% x
    ]

  working <- working[sample(seq_len(nrow(working)), num_to_write), ]

  working$status <- sample(c("train", "validation", "test"),
                           size = nrow(working),
                           prob = c(.7, .2, .1),
                           replace = TRUE)

  working$out_name <- paste0(base_dir, "/",
                             working$status, "/",
                             "notower/",
                             working$img_num, "_",
                             working$tile_id, ".png")

  for (i in seq_len(nrow(working))) {
    magick::image_write(
      image = image_read(drop(working[i, "tile_array", drop = TRUE][[1]]) / 255),
      path = working[i, "out_name", drop = TRUE])
  }
})

parallel::stopCluster(cl)
