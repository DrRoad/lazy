#' Kmeans distance to center feature mappings
#'
#' Creates mapping tables for each numerical feature containing the center for each feature's min and max value. These tables can then be applied using the function 'apply.kmeans.mappings' to calculate the distance to cluster center for each feature.
#'
#' @param data [Required | data.frame] Dataset containing categorical features
#' @param x [Required | character] A vector of categorical feature names present in the dataset
#' @param clusters [Optional | integer | Default 3] The number of clusters to create in each feature
#' @param seed [Optional | integer| Default NULL] The random number seed for reproducable results
#' @param sample.size [Optional | numeric | Default 0.3] Percentage to down sample data for decreased computation time
#' @param progress [Optional | logical] Display a progress bar
#' @return List of data frames containing mapping tables
#' @export
#' @examples
#' res <- map.kmeans.features(data = iris, x = setdiff(names(iris), "Species"))
#' @author
#' Xander Horn
map.kmeans.features <- function(data, x, clusters = 3, sample.size = 0.3, seed = NULL, progress = TRUE){

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

  if(sample.size > 1 | sample.size <= 0){
    warning("sample.size restricted between 0 and 1, defaulting to 0.3")
    sample.size <- 0.3
  }

  if(is.null(seed) == FALSE){
    set.seed(seed)
  }

  if(progress == TRUE){
    pb <- txtProgressBar(min = 0, max = length(x), style = 3)
  }

  temp <- as.data.frame(data[, x])
  temp <- temp[sample(nrow(temp), sample.size * nrow(temp), replace = F), ]

  mappings <- list()
  for(i in 1:length(x)){

    if(clusters > length(unique(temp[, x]))){
      clusters <- length(unique(temp[x]))
    }

    toCluster <- temp[, x[i]] / max(temp[, x[i]] + 1, na.rm = TRUE)

    clst <- kmeans(x = toCluster, centers = clusters)
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
    lookup[, 3] <- lookup[, 3] * lookup[, 2]
    lookup[2:nrow(lookup), 'min'] <- lookup[1:(nrow(lookup)-1), 'max']
    lookup[1, 'min'] <- lookup[1, 'min'] * -100
    lookup[nrow(lookup), 'max'] <- lookup[nrow(lookup), 'max'] * 100
    mappings[[i]] <- lookup
    if(progress == TRUE){
      setTxtProgressBar(pb, i)
    }
  }

  names(mappings) <- x
  cat(" \n")
  return(mappings)
}
