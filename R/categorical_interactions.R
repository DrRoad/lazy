#' Categorical feature interactions
#'
#' Computes categorical feature interactions by joining categories together.
#'
#' @param data [Required | data.frame] Dataset containing numeric features
#' @param x [Required | character] A vector of categorical feature names present in the dataset
#' @param n.interactions [Optional | numeric] Number of features to interact, defaults to 2
#' @param progress [Optional | logical] Display a progress bar
#' @return Data frame or vector of interacted features
#' @export
#' @examples
#' res <- categorical.interactions(data = as.data.frame(Titanic), x = c('Class','Sex','Age'))
#' @author
#' Xander Horn
categorical.interactions <- function(data, x, n.interactions = 2,progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(n.interactions < 2){
    stop("Interactions require at least 2 levels")
  }

  if(length(x) < n.interactions){
    stop("Require more than one feature to compute interactions")
  }

  comb <- as.data.frame(t(combn(x, n.interactions)),
                        stringsAsFactors = FALSE)

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = nrow(comb), style = 3)
  }

  temp <- as.data.frame(data[, x])

  for(i in 1:nrow(comb)){

    temp[,paste0("interaction.",paste0(comb[i,], collapse = "."))] <- do.call(paste, as.data.frame(temp[,paste0(comb[i,])], stringsAsFactors=FALSE))

    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  temp <- temp[,setdiff(names(temp), x)]
  return(temp)
}
