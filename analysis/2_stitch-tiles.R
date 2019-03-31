library(stringr)
library(magick)

# setup
tile1_dim <- 250
time1_overlap <- 0

tile2_dim <- 50
tile2_overlap <- 10

source_dir <- "data/tiles/splits/orig"

# parse file names
files <- data.frame(
  file_name = list.files(file.path(source_dir), recursive = TRUE), 
  file_path = list.files(file.path(source_dir), recursive = TRUE, full.names = TRUE),
  stringsAsFactors = FALSE)

img_name_regex <- ".*?(\\d*)_(\\d*).*?tile(\\d{1,2}).*?\\.png$"

files$is_image <- str_detect(files$file_name, img_name_regex)

files <- files[files$is_image,]

image_nums <- str_match(files$file_name, img_name_regex)
image_nums <- image_nums[, 2:4]
storage.mode(image_nums) <- "numeric"

files$source_img_num <- image_nums[, 1]
files$img_tile1_num <- image_nums[, 2]
files$img_tile2_num <- image_nums[, 3]

files <- files[order(files$source_img_num, files$img_tile1_num, files$img_tile2_num), ]

tile2_list <- split(files, paste(files$source_img_num, files$img_tile1_num, sep = "_"))

# stitch level 2 tiles
x_offsets <- seq(0, tile1_dim, by = tile2_dim - tile2_overlap)
x_offsets <- x_offsets[x_offsets + tile2_dim <= tile1_dim]

y_offsets <- seq(0, tile1_dim, by = tile2_dim - tile2_overlap)
y_offsets <- y_offsets[y_offsets + tile2_dim <= tile1_dim]

offsets <- expand.grid(x = x_offsets, y = y_offsets)

my_image1 <- image_read(temp$file_path[1])
my_image2 <- image_read(temp$file_path[2])

out_image <- image_blank(tile1_dim, tile1_dim, color = "white")

for (i in seq_len(nrow(offsets))) {
  out_image <- image_composite(
    out_image, 
    image_read(temp$file_path[i]), 
    offset = geometry_point(offsets$x[i], offsets$y[i]))
}

plot(image_composite(base_image, my_image1, offset = geometry_point(offsets$x[i], offsets$y[1])))
