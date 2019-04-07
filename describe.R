#' Tabular exploratory data analysis
#'
#' Provides a high level overview of a dataset by providing some summary statistics.
#'
#' @param data [Required | data.frame] Dataset containing categorical features
#' @param sample.size [Optional | numeric] Percentage to down sample data for decreased computation time
#' @param seed [Optional | integer] Random number seed for reproducable results
#' @param progress [Optional | logical] Display a progress bar 
#' @return Data frame containing statistics of features
#' @export
#' @examples
#' res <- describe(iris)
#' @author 
#' Xander Horn
describe <- function(data, sample.size = 0.3, seed = 1234, progress = TRUE){

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }
  
  if (progress == TRUE) {
    pb <- txtProgressBar(min = 0, max = ncol(data), style = 3)
  }
  
  if(sample.size > 1 | sample.size <= 0){
    warning("sample.size restricted between 0 and 1, defaulting to 0.3")
    sample.size <- 0.3
  }
  
  library(moments)
  
  data <- data[sample(nrow(data), sample.size * nrow(data), replace = F), ]
  
  res <- data.frame(feature = names(data),
                    observations = nrow(data),
                    features = ncol(data),
                    type = NA,
                    missing = NA,
                    unique = NA,
                    constant = NA,
                    all.na = NA,
                    lower.outlier.value = NA,
                    upper.outlier.value = NA,
                    skewness = NA,
                    median = NA,
                    mean = NA)
  
  for(i in 1:nrow(res)){
    
    res[i, "type"] <- class(data[,i])
    res[i, "missing"] <- sum(is.na(data[,i]))
    res[i, "unique"] <- length(unique(data[,i]))
    res[i, "constant"] <- ifelse(res[i, "unique"] == 1, 1, 0)
    res[i, "all.na"] <- ifelse(res[i, "missing"] == res[i, "observations"], 1, 0)
    
    if(res[i, "type"] %in% c("integer","numeric")){
      
      res[i, "lower.outlier.value"] <- quantile(data[, i], probs = 0.25, na.rm = TRUE)[[1]] - (1.5 * IQR(data[, i], na.rm = TRUE))
      res[i, "upper.outlier.value"] <- quantile(data[, i], probs = 0.75, na.rm = TRUE)[[1]] + (1.5 * IQR(data[, i], na.rm = TRUE))
      res[i, "skewness"] <- moments::skewness(data[, i], na.rm = TRUE)
      res[i, "median"] <- median(data[, i], na.rm = TRUE)
      res[i, "mean"] <- mean(data[, i], na.rm = TRUE)
    
    }
    
    if (progress == TRUE) {
      setTxtProgressBar(pb, i)
    }
  }
  
  cat(" \n")
  return(res)
}
