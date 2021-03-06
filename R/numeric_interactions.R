#' Numeric feature interactions
#'
#' Computes two way feature interactions by summing, deducting, multiplying and dividing features.
#'
#' @param data [Required | data.frame] Dataset containing categorical features
#' @param x [Required | character] A vector of numeric feature names present in the dataset
#' @param progress [Optional | logical] Display a progress bar
#'
#' @return Data frame or vector of interacted features
#' @export
#'
#' @examples
#' res <- numeric.interactions(iris, setdiff(names(iris), "Species"))
#' @author
#' Xander Horn
numeric.interactions <- function(data, x, progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No numeric features specified in arg 'x'")
  }

  if(length(x) < 2){
    stop("Require more than one feature to compute interactions")
  }

  temp <- as.data.frame(data[, x])

  comb <- as.data.frame(t(combn(x, 2)), stringsAsFactors = FALSE)

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(comb), style = 3)
  }

  for(i in 1:nrow(comb)){

    temp[,paste0("lazy.interaction.",paste0(comb[i,], collapse = "."),".sum")] <- temp[, comb[i, 1]] + temp[, comb[i, 2]]
    temp[,paste0("lazy.interaction.",paste0(comb[i,], collapse = "."),".deduct")] <- temp[, comb[i, 1]] - temp[, comb[i, 2]]
    temp[,paste0("lazy.interaction.",paste0(comb[i,], collapse = "."),".multiply")] <- temp[, comb[i, 1]] * temp[, comb[i, 2]]
    temp[,paste0("lazy.interaction.",paste0(comb[i,], collapse = "."),".divide")] <- temp[, comb[i, 1]] / (temp[, comb[i, 2]] + 1)

    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  temp <- temp[,setdiff(names(temp), x)]
  cat(" \n")
  return(temp)
}
