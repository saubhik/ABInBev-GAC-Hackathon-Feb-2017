rm(list=ls())
require(magick)
require(tesseract)
require(stringr)

options(warn = -1) #suppress warnings

# creating the price_list
price_list <- read.csv("price_list.csv")
price_list_mod <- read.csv("price_list_mod.csv")
week_var1 <- data.frame()
week_var2 <- data.frame()
j <- 1
for (i in 1:104) { 
  week_var1 <- rbind(week_var1, data.frame(rep(j, 22)))
  j = j+1
}
j <- 1
for (i in 1:104) { 
  week_var2 <- rbind(week_var2, data.frame(rep(j, 20)))
  j = j+1
}
price_list_mod <- data.frame(price_list_mod[rep(seq_len(nrow(price_list_mod)), 104), ])
rownames(price_list_mod) <- c(1:nrow(price_list_mod))
price_list$Key <- rep(1:20, 104)
price_list <- cbind(price_list, data.frame(week_var2))
price_list_mod <- cbind(price_list_mod, data.frame(week_var1))
names(price_list)[6] <- "Week"
names(price_list_mod)[4] <- "Week"
price_list <- merge(price_list_mod, price_list[, c("Key", "Week", "Recommended.Price")], all.x = T, sort = FALSE, by = c("Key", "Week"))
price_list$"tag_Brand" <- ""
price_list$"tag_PackSize" <- ""
price_list$"tag_Price" <- ""
price_list$Key <- NULL

# creating brand_list
brand_list <- c(as.character(unique(price_list$Brand)))

#index to store where the scanner will enter information in the price_list
index <- 1

# scanner function
scanner <- function (img, ind) {
  tag_width <- 187
  info <- image_info(img)
  start <- 0
  end <- start + tag_width
  
  while (end <= info$width) {
    
    #creating crop
    geom <- paste(end - start, 'x', info$height, '+', start, sep = '')
    part <- image_crop(img, geometry = geom)
    
    #extract text from cropped image
    im <- image_blur(image_scale(part, "x300"), 10, 1.5) #tune values for better ocr extraction
    image_write(im, "img.tiff")
    extract <- ocr("img.tiff")
    
    brand_name <- extract
    pack_size <- NA
    price <- extract
    
    for (br_name in brand_list) {
      if (grepl(br_name, extract)) {
        brand_name <- br_name
      }
    }
    if (grepl("6 Pk 12", extract)) pack_size <- "6 Pk 12 Oz Glass"
    if (grepl("6 Pk 11", extract)) pack_size <- "6 Pk 11.2 Oz Glass"
    if (grepl("12 Pk", extract) & grepl("Can", extract)) pack_size <- "12 Pk 12 Oz Can"
    if (grepl("12 Pk", extract) & grepl("Glass", extract)) pack_size <- "12 Pk 12 Oz Glass"
    price <- as.character(str_match(extract, "\\$\\d\\d\\s\\d\\d"))
    price <- gsub("\\$", "", price)
    price <- paste(unlist(strsplit(price, split = " "))[1], ".", unlist(strsplit(price, split = " "))[2], sep = "")
    
    if (!is.na(brand_name)) {
      if (brand_name %in% brand_list) {
        price_list$tag_Brand[ind] <<- brand_name
        price_list$tag_PackSize[ind] <<- pack_size
        price_list$tag_Price[ind] <<- price
        print(brand_name) #printing just to test if it's working
        print(pack_size) #same as above!
        print(price) #same as above!
        ind = ind + 1
        start = end
        end = end + tag_width
      }
      else {
        start = start + 10 #increase to higher number (but not too high) to speeden up
        end = end + 10 #whatever increment you put up above must also be put here!
      }
    }
    else {
      start = end
      end = start + tag_width
    }
  }
}

# function to scan each image (uses scanner function)
scanImage <- function (im, index) {
  # splitting image into 6 horizontal price tag holders
  info <- image_info(im)
  width1 <- 2292
  width2 <- info$width - 2292
  height1 <- 76
  height2 <- 79
  height3 <- 78
  height4 <- 75
  height5 <- 80
  height6 <- 77
  x_offset <- width1
  part1_y_offset <- 733
  part2_y_offset <- 726
  part3_y_offset <- 1789
  part4_y_offset <- 1795
  part5_y_offset <- 2822
  part6_y_offset <- 2828
  geom1 <- paste(width1, 'x', height1, '+', 0, '+', part1_y_offset, sep = '')
  geom2 <- paste(width2, 'x', height2, '+', x_offset, '+', part2_y_offset, sep = '')
  geom3 <- paste(width1, 'x', height3, '+', 0, '+', part3_y_offset, sep = '')
  geom4 <- paste(width2, 'x', height4, '+', x_offset, '+', part4_y_offset, sep = '')
  geom5 <- paste(width1, 'x', height5, '+', 0, '+', part5_y_offset, sep = '')
  geom6 <- paste(width2, 'x', height6, '+', x_offset, '+', part6_y_offset, sep = '')
  part1 <- image_crop(im, geometry = geom1)
  part2 <- image_crop(im, geometry = geom2)
  part3 <- image_crop(im, geometry = geom3)
  part4 <- image_crop(im, geometry = geom4)
  part5 <- image_crop(im, geometry = geom5)
  part6 <- image_crop(im, geometry = geom6)
  image_write(part1, "part1.png")
  image_write(part2, "part2.png")
  image_write(part3, "part3.png")
  image_write(part4, "part4.png")
  image_write(part5, "part5.png")
  image_write(part6, "part6.png")
  
  # scanning the 6 parts - calling the scanner function
  part1 <- image_read("part1.png")
  scanner(part1, index)
  part2 <- image_read("part2.png")
  scanner(part2, index + 4)
  part3 <- image_read("part3.png")
  scanner(part3, index + 3)
  part4 <- image_read("part4.png")
  scanner(part4, index + 4)
  part5 <- image_read("part5.png")
  scanner(part5, index + 4)
  part6 <- image_read("part6.png")
  scanner(part6, index + 4)
}

# Calling scanImage for each of the 104 images (change path here!)
files <- list.files(path = "d:/hackathon_Feb17/data/Focus Area - Image Processing/Shelf Image Dataset/", pattern = "*.jpg", full.names = TRUE)
for (file in files) {
  im <- image_read(file)
  scanImage(im, index)
  index <- index + 22
}

# final comparisons - see price_list - that's the final data frame
criterion1 <- ((price_list$tag_Brand == price_list$Brand) & (price_list$SKU == price_list$tag_PackSize))
price_list$tag_manual_mismatch <- ifelse(criterion1, "No Manual Error", "Manual Error")
criterion2 <- ((price_list$Recommended.Price != price_list$tag_Price) & (price_list$tag_manual_mismatch == "No Manual Error"))
price_list$tag_price_different <- ifelse(criterion2, "Price Different", "-")

write.csv(price_list, "final.csv")
