#' Apply mapping tables
#'
#' Applies mapping tables created by functions map.categorical.encoding, map.kmeans.features and map.freq.encoding to a new dataset
#'
#' @param data [Required | data.frame] Dataset containing categorical features
#' @param kmeans.mappings [Optional | list] Output from function map.kmeans.features
#' @param categorial.mappings [Optional | list] Output from function map.categorical.encoding
#' @param freq.mappings [Optional | list] Output from function map.freq.encoding
#' @param progress [Optional | logical] Display a progress bar
#' @return Data frame with newly added features
#' @export
#' @examples
#' km <- map.kmeans.features(data = iris, x = setdiff(names(iris), "Species"))
#' new_iris <- apply.mappings(data = iris, kmeans.mappings = km)
#' @author
#' Xander Horn
apply.mappings <- function(data, kmeans.mappings = NULL, categorial.mappings = NULL, freq.mappings = NULL, progress = TRUE){

  library(sqldf)
  n <- ncol(data)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  max <- length(kmeans.mappings) + length(categorial.mappings) + length(freq.mappings)
  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = max, style = 3)
  }

  if(is.null(kmeans.mappings) == FALSE){
    for(i in 1:length(kmeans.mappings)){
      temp <- kmeans.mappings[[i]]

      if(sum(is.na(temp$min)) == 0 & sum(is.na(temp$max)) == 0){
        qry <- paste0("select `",names(kmeans.mappings)[i],"`,  temp.center from data left join temp on data.`",names(kmeans.mappings)[i],"` > temp.min and data.`",names(kmeans.mappings)[i], "` <= temp.max")
        temp <- sqldf(qry)
        temp$dist <- (temp[, 1] - temp[, 2])   / temp[, 1]
        data[, paste0("kmeans.dist.",names(kmeans.mappings)[i])] <- ifelse(is.na(temp$dist) == TRUE, -1, temp$dist)
      }

    }

    if(progress == TRUE){
      setTxtProgressBar(pb, length(kmeans.mappings))
    }
  }

  if(is.null(categorial.mappings) == FALSE){
    for(i in 1:length(categorial.mappings)){
      temp <- categorial.mappings[[i]]
      data <- merge(x = data,
                    y = temp,
                    by.x = names(categorial.mappings)[i],
                    all.x = TRUE)
    }

    if(progress == TRUE){
      setTxtProgressBar(pb, length(kmeans.mappings) + length(categorial.mappings))
    }
  }

  if(is.null(freq.mappings) == FALSE){
    for(i in 1:length(freq.mappings)){
      temp <- freq.mappings[[i]]
      data <- merge(x = data,
                    y = temp,
                    by.x = names(freq.mappings)[i],
                    all.x = TRUE)
    }

    if(progress == TRUE){
      setTxtProgressBar(pb, length(kmeans.mappings) + length(categorial.mappings) + length(freq.mappings))
    }
  }

  for(i in (n+1):ncol(data)){
    if(class(data[,i]) %in% c("integer","numeric")){
      data[,i] <- ifelse(is.na(data[,i]) == TRUE, median(data[,i], na.rm = TRUE), data[,i])
    } else {
      data[,i] <- ifelse(is.na(data[,i]) == TRUE, "Unknown", data[,i])
    }

  }

  return(data)
}
