#' Maximum value scaler
#'
#' Scales all numerical feautes to be between 0 and 1.
#'
#' @param data [Required | data.frame] Dataset containing numeric features
#' @param x [Required | character] A vector of numeric feature names present in the dataset
#' @param progress [Optional | logical] Display a progress bar
#' @return Data frame with scaled features
#' @export
#' @examples
#' res <- max.scaler(data = iris, x = names(iris)[1:4])
#' @author
#' Xander Horn
max.scaler <- function(data, x, progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No numeric features specified in arg 'x'")
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  for(i in 1:length(x)){
    if(max(data[, x[i]]) == -1){
      data[, x[i]] <- data[, x[i]] / max(data[, x[i]])
    } else {
      data[, x[i]] <- data[, x[i]] / (max(data[, x[i]]) + 1)
    }


    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }
  return(data)
}
