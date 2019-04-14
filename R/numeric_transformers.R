#' Numeric feature transformations
#'
#' Applies different transformations to numeric features such as sqrt, log and boxcox transforms.
#'
#' @param data [Required | data.frame] Dataset containing numeric features
#' @param x [Required | character] A vector of numeric feature names present in the dataset
#' @param progress [Optional | logical] Display a progress bar
#' @return Data frame containing transformed features
#' @export
#' @examples
#' res <- numeric.transformers(data = iris, x = "Sepal.Length")
#' @author
#' Xander Horn
numeric.transformers <- function(data, x, progress = TRUE){

  library(MASS)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No categorical features specified in arg 'x'")
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  temp <- as.data.frame(data[, x])
  if(length(x) == 1){
    names(temp) <- x
  }

  for(i in 1:length(x)){

    if(class(temp[,x[i]]) %in% c("numeric", "integer")){

      var <- temp[, x[i]]
      box <- MASS::boxcox(var ~ 1, lambda = seq(-6,6,0.1), plotit = FALSE)
      cox = data.frame(box$x, box$y)
      cox <- cox[with(cox, order(-cox$box.y)),]
      lambda <- cox[1, "box.x"]

      temp[, paste0("log.", names(data)[i])] <- log((temp[, 1] + 1))
      temp[, paste0("sqrt.", names(data)[i])] <- sqrt(temp[, 1])
      temp[, paste0("boxcox.", names(data)[i])] <- (temp[, 1] ^ lambda - 1) / lambda

      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }

    }
  }

  temp <- temp[,setdiff(names(temp), x)]
  cat(" \n")
return(temp)
}
