#' Kmeans distance to center feature mappings
#'
#' Creates mapping tables for each numerical feature containing the center for each feature's min and max value.
#'
#' @param data [Required | data.frame] Dataset containing categorical features
#' @param x [Required | character] A vector of categorical feature names present in the dataset
#' @param clusters [Optional | integer | Default 3] The number of clusters to create in each feature
#' @param seed [Optional | integer| Default NULL] The random number seed for reproducable results
#' @param progress [Optional | logical] Display a progress bar
#' @return List of data frames containing mapping tables
#' @export
#' @examples
#' res <- kmeans.features(data = iris, x = setdiff(names(iris), "Species"))
#' @author
#' Xander Horn
map.kmeans.features <- function(data, x, clusters = 3, seed = NULL, progress = TRUE){

  library(sqldf)

  if(missing(data)){
    stop("No data provided to function in arg 'data'")
  }

  if(missing(x)){
    stop("No numerical features specified in arg 'x'")
  }

  if(clusters < 2){
    stop("Clusters need to be 2 or more")
  }

  if(is.null(seed) == FALSE){
    set.seed(seed)
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  temp <- as.data.frame(data[, x])

  mappings <- list()
  for(i in 1:length(x)){

    if(clusters > length(unique(temp[, x]))){
      clusters <- length(unique(temp[x]))
    }

    clst <- kmeans(x = temp[, x[i]], centers = clusters)
    lookup <- as.data.frame(temp[, x[i]])
    names(lookup) <- x[i]
    lookup$cluster <- clst$cluster
    centers <- data.frame(cluster = row.names(clst$centers),
                          center = clst$centers)
    lookup <- merge(x = lookup,
                    y = centers,
                    by.x = "cluster",
                    all.x = TRUE)
    lookup <- sqldf(paste0("select min(`",x[i],"`) as min, max(`",x[i],"`) as max, center from lookup group by center"))
    names(lookup) <- paste0(x[i],".",names(lookup))
    mappings[[i]] <- lookup
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  names(mappings) <- x
  cat(" \n")
  return(mappings)
}
