library(stringr)
library(sf)

temp <- st_read("c:/users/wfu3/desktop/0tileindex")

counties <- tigris::counties("nj")
counties <- st_as_sf(counties)
counties <- counties[counties$NAME == "Union",]

temp <- st_transform(temp, st_crs(counties))

temp_int <- st_intersects(temp, counties)

nj_images <- temp[sapply(temp_int, function(x) length(x) > 0),]
nj_images$file_name <- str_match(nj_images$URL, "(.*/)*(.*)\\.tif$")[, 3]
nj_images$url_base <- str_match(nj_images$URL, "(.*/)*(.*)\\.tif$")[, 2]

dest_dir <- "c:/users/wfu3/desktop/temp2"

for (i in 2:nrow(nj_images)) {
  Sys.sleep(2)

  download.file(
    paste0(nj_images$url_base[i], nj_images$file_name[i], ".tif"),
    file.path(dest_dir, paste0(nj_images$file_name[i], ".tif")),
    mode = "wb",
    quiet = TRUE
  )

  download.file(
    paste0(nj_images$url_base[i], nj_images$file_name[i], ".tfw"),
    file.path(dest_dir, paste0(nj_images$file_name[i], ".tfw")),
    quiet = TRUE
  )
}



