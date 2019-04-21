#' Apply kmeans mapping tables
#'
#' Applies the mappings tables of kmeans distance features to a new data frame to create kmeans distance to cluster center features.
#'
#' @param data [Required | data.frame] Dataset containing categorical features
#' @param mapping.list [Required | list] List of data frames created by the function 'map.kmeans.features'
#' @param progress [Optional | logical] Display a progress bar
#' @return Data frame with original features and newly generated features
#' @export
#' @examples
#' res <- map.kmeans.features(data = iris, x = setdiff(names(iris), "Species"))
#' new_iris <- apply.kmeans.mappings(data=iris, mapping.list = res)
#' @author
#' Xander Horn
apply.kmeans.mappings <- function(data, mapping.list, progress = TRUE){

  library(sqldf)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(mapping.list)){
    stop("No mapping list provided to function in arg 'mapping.list'")
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(mapping.list), style = 3)
  }

  for(i in 1:length(mapping.list)){
    temp <- mapping.list[[i]]

    if(sum(is.na(temp$min)) == 0 & sum(is.na(temp$max)) == 0){
      qry <- paste0("select `",x[i],"`,  temp.center from data left join temp on data.`",x[i],"` > temp.min and data.`",x[i], "` <= temp.max")
      temp <- sqldf(qry)
      temp$dist <- (temp[, 1] - temp[, 2])   / temp[, 1]
      data[, paste0("kmeans.dist.",x[i])] <- ifelse(is.na(temp$dist) == TRUE, -1, temp$dist)

      if(progress == TRUE){
        setTxtProgressBar(pb, i)
      }
    }
  }

  cat(" \n")
  return(data)
}
