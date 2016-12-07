preProcess <- function(df) {
  df$item_id <- NULL
  df$Product.Long.Description <- NULL
  df$Product.Short.Description <- NULL
  df$Short.Description <- NULL
  df$Product.Name <- NULL
  df$Synopsis <- NULL
  df$Artist.ID <- NULL
  df$Genre.ID <- NULL
  df$ISBN <- NULL
  
  Unknown <- "Unknown"  
  for (i in 1: ncol(df)) {
    levels(df[,i]) <- c(levels(df[,i]), Unknown)
    df[,i][df[,i] == '' ] <- Unknown 
    df[,i] <- droplevels(df[,i])
  }
  
  remove(i); remove(Unknown); gc()
  
  return (df)
}

#train$tag <- paste('[',train$tag,']', sep = '') 