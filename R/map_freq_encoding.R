#' Frequency encoding
#'
#' Creates a list of frequency encoded data.frames for each feature provided.
#'
#' @param data [Required | data.frame] Dataset containing categorical features
#' @param x [Required | character] A vector of categorical or numerical feature names present in the dataset
#' @param progress [Optional | logical] Display a progress bar
#' @return List of data frames containing engineered mapping features
#' @export
#' @examples
#' res <- map.freq.encoding(iris, names(iris))
#' @author
#' Xander Horn
map.freq.encoding <- function(data, x, progress = TRUE){

  library(sqldf)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No categorical features specified in arg 'x'")
  }

  data <- as.data.frame(data)

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  mappings <- list()
  for(i in 1:length(x)){
    mappings[[i]] <- sqldf(paste0("select `", x[i], "` as level, count(`",x[i],"`) as count from data group by `",x[i],"`"))

    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  names(mappings) <- x
  cat(" \n")
  return(mappings)
}
