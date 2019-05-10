library(raster)
library(sf)
library(data.table)
library(magick)

scores <- readRDS("output/2019-05-07/chi-scores/chi-scores-gt0417-lte1.rds")
class(scores) <- c("data.frame", "sf")

scores_gt995 <- scores[scores$predicted_probs > .98 & scores$predicted_probs <= .985, ]

scores_gt995$img_num <- stringr::str_match(scores_gt995$img_name,
                                           "(.*/)(\\d{1,4})_.*$")[, 3]
scores_gt995$img_stub <- paste0(scores_gt995$img_num, "-",
                                scores_gt995$slice_id, ".png")

scores_list <- split(scores_gt995[, c("img_num", "slice_id", "img_stub")],
                     scores_gt995$img_num)

cl <- parallel::makeCluster(parallel::detectCores() - 1)

parallel::clusterEvalQ(cl, {
  library(raster)
  library(sf)
  library(magick)
})

parallel::parLapplyLB(X = scores_list, cl = cl, fun = function(to_score) {
  img_slices <- readRDS(
    paste0("data/source_from-chi-website/chi-slices/",
           unique(to_score$img_num),
           "_slices.rds")
    )

  img_slices <- img_slices[img_slices$slice_id %in% to_score$slice_id,]

  for (i in 1:nrow(img_slices)) {
    image_write(
      image_read(drop(img_slices$slice_array[[i]]) / 255),
      paste0("c:/users/wfu3/desktop/temp/", to_score$img_stub[[i]])
    )
  }
})

parallel::stopCluster(cl)
