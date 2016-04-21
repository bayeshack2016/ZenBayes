rm(list = ls())
graphics.off()
set.seed(10)

library(plyr, dplyr)

url.df <- read.csv("onet-url.csv", stringsAsFactors = FALSE)
url.df <- unique(url.df) # just in case

# Extract filenames from URLs
url.df$filename <- vapply(seq_len(nrow(url.df)), function(i) {
  x <- strsplit(url.df$url[i], "/")[[1]]
  return(x[length(x)])
}, "character scalar")

# Download the files, if necessary
# d_ply(url.df, c("month", "year"), function(df) {
#   wget <- paste0("wget -c ", df$url)
#   print(wget)
#   system(wget)
# })

# Extract all zip files and store as one giant list
onet.all.years <- dlply(url.df, c("month", "year"), function(df) {
  # Unzip files
  folder.name <- paste0(df$month, df$year)
  print(folder.name)
  system(paste0("rm -rf ", folder.name))
  system(paste0("mkdir ", folder.name))
  unzip(df$filename[1], exdir = paste0("./", folder.name))
  file.vec <- list.files(paste0("./", folder.name), recursive = TRUE)

  # Delete README files
  readme <- grep("read\\s*me\\.txt$", file.vec, ignore.case=TRUE, value=TRUE)
  print(readme)
  print(paste0("rm -rf './", folder.name, "/", readme, "'"))
  file.vec <- setdiff(file.vec, readme)
  print(file.vec)

  # Read CSV files
  file.vec.full <- paste0("./", folder.name, "/", file.vec)
  files <- lapply(file.vec.full, read.csv, fill = TRUE, header = TRUE, sep = '\t',
              stringsAsFactors = FALSE)
  names(files) <- file.vec
  return(files)
})


saveRDS(onet.all.years, "../processed/onet_all_years.RDS")