data.leak <- function(train, test, id.feats = NULL, sample.size = 0.3, seed = 1991){
  
  if(missing(train)){
    stop("Provide training set")
  }
  
  if(missing(test)){
    stop("Provide testing set")
  }
  
  if(sample.size <= 0 | sample.size > 1){
    sample.size = 0.3
    warning("sample_size boundries between 0 and 1, defaulting to 0.3")
  }
  
  library(rpart)
  library(caret)
  library(pROC)
  
  train$data.leak.target <- 0
  test$data.leak.target <- 1
  
  test <- test[,names(train)]
  
  train <- train[sample(nrow(train), sample.size * nrow(train), replace = F), ]
  
  if(is.null(id.feats) == FALSE){
    train[,id.feats] <- as.numeric(as.factor(train[,id.feats]))
    test[,id.feats] <- as.numeric(as.factor(test[,id.feats]))
  }
  
  combined <- rbind(train, test)
  
  out <- data.frame(feature = setdiff(names(combined), "data.leak.target"),
                    auc = NA)
  
  pb <- txtProgressBar(min = 0, max = nrow(out), style = 3)
  for(i in 1:nrow(out)){
    form <- as.formula(paste0("data.leak.target ~ ", out[i,"feature"]))
    tree <- rpart(formula = form,
                  data = combined,
                  control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001))
    tree_min <- tree$cptable[which.min(tree$cptable[ , "xerror"]), "CP"]
    tree <- prune(tree, cp = tree_min)
    out[i,"auc"] <- pROC::auc(response = combined$data.leak.target,
                              predictor = predict(tree, combined))
    setTxtProgressBar(pb, i)
  }
  out$auc <- round(out$auc, 3)
  out$leak <- ifelse(out$auc <= 0.5, "no leak",
                     ifelse(out$auc > 0.5 & out$auc <= 0.65, "weak leak",
                            ifelse(out$auc > 0.65 & out$auc <= 0.8, "moderate leak", "strong leak")))
  return(out)
}