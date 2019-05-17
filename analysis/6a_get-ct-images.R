library(sf)
library(stringr)

ct_index <- st_read("data/source_from-ct-website/ct-images/ct-image-index")

img_names <- str_match(ct_index$DESCRIPTIO, "(.*)\\.tif")[,2]

for (i in seq_len(img_names)) {
  Sys.sleep(1)

  download.file(
    url = paste0(
      "http://www.cteco.uconn.edu/download/aerial/2016/tiles/sid4/",
      img_names[i],
      ".zip"),
    destfile = paste0("data/source_from-ct-website/ct-images/", img_names[i], ".zip"),
    mode = "wb",
    quiet = TRUE
    )
}



