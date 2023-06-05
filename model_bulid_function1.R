library(glmnet) 
library(data.table)

library(pROC)
library(survivalROC)
library(ROCR)


data_process_function <- function(model_nume) {
  
  my.read.table <- function(filename, ...) {
    cat("reading", basename(filename), "... ")
    ## read in tab-delimited spreadsheet
    x <- fread(
      filename,
      header = T,
      stringsAsFactors = F,
      sep = "\t",
      ...
    )
    ## remove any duplicate rows (identified by the first column)
    x <- x[match(unique(x[[1]]), x[[1]]), ]
    ## make the first column the rownames of the data frame
    x <- data.frame(x, row.names = 1, stringsAsFactors = F)
    cat(nrow(x), "x", ncol(x), "\n")
    x
  }
  
  ##############################################
  ## training
  filenames_train <- c()
  filenames_test<- c()
  if ("model1" == model_nume) {
    filenames_train[1] <-paste0( "/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/", "clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","protein.txt")
    filenames_test[1] <-paste0( "/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/", "clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","protein.txt")
  } else if ("model2" == model_nume) {
    filenames_train[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","methylation.txt")
    filenames_test[1] <-paste0( "/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/", "clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","methylation.txt")
  } else if ("model3" == model_nume) {
    filenames_train[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","protein.txt")
    filenames_test[1] <-paste0( "/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/", "clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","protein.txt")
  } else if ("model4" == model_nume) {
    filenames_train[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mirna.txt")
    filenames_test[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mirna.txt")
  } else if ("model5" == model_nume) {
    filenames_train[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mrna.txt")
    filenames_test[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mrna.txt")
  } else if ("model6" == model_nume) {
    filenames_train[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mutation.txt")
    filenames_test[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mutation.txt")
  } else if ("model7" == model_nume) {
    filenames_train[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mirna.txt")
    filenames_train[3] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mrna.txt")
    filenames_train[4] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mutation.txt")
    filenames_train[5] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","protein.txt")
    
    filenames_test[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mirna.txt")
    filenames_test[3] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mrna.txt")
    filenames_test[4] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mutation.txt")
    filenames_test[5] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","protein.txt")
    
  } else if ("model8" == model_nume) {
    filenames_train[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","clinical.txt")
    filenames_train[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","methylation.txt")
    filenames_train[3] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mirna.txt")
    filenames_train[4] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mrna.txt")
    filenames_train[5] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","mutation.txt")
    filenames_train[6] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Training_dataset/","protein.txt")
    
    filenames_test[1] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","clinical.txt")
    filenames_test[2] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","methylation.txt")
    filenames_test[3] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mirna.txt")
    filenames_test[4] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mrna.txt")
    filenames_test[5] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","mutation.txt")
    filenames_test[6] <-paste0("/Users/mac/Documents/2022Health Data Science/Machine Learning/Machine/code/Testing_dataset/","protein.txt")
  }
  
  ## load the data files into a list
  dat <- lapply(filenames_train, my.read.table)
  
  ## name the items of the list by the filename
  names(dat) <- sub(".txt", "", basename(filenames_train))
  
  ## remove the clinical data from the list,
  ## it's a bit different than the other data files
  clinical <- dat$clinical
  dat$clinical <- NULL
  
  ## outcome if progression-free interval
  outcome.var <- c("pfi.time", "pfi")
  
  ## clinical variables that might be informative about outcome
  clinical.vars <- c(
    "age.at.diagnosis",
    "estrogen.receptor.status",
    "progesterone.receptor.status",
    "lymphocyte.infiltration",
    "necrosis.percent"
  )
  
  ## remove features with no variance or all missing values
  for (name in names(dat)) {
    feature.var <- apply(dat[[name]], 1, var, na.rm = T)
    dat[[name]] <- dat[[name]][which(feature.var > 2e-16), ]
  }
  
  
  ## identify top univariate predictors in each data type
  library(limma)
  univariate.predictors <- sapply(names(dat), function(name) {
    ## for each data type ...
    cat(date(), "testing", name, "...\n")
    ## prepare to test, for each feature, feature ~ outcome
    outcome <- clinical$pfi[match(colnames(dat[[name]]), rownames(clinical))]
    design <- model.matrix( ~ outcome)
    ## fit linear model for each feature
    fit <- lmFit(dat[[name]], design)
    ## calculate p-values
    fit <- eBayes(fit)
    ## identify the top 100 associations
    idx <- order(fit$p.value[, "outcome"], decreasing = F)[c(1:220)]
    ## return the names of the features with the top 25 associations
    rownames(dat[[name]])[idx]
  })
  names(univariate.predictors) <- names(dat)
  
  
  
  ## convert clinical data to a numeric matrix
  ##  (i.e. create dummy variables for categorical variables)
  clinical <- model.matrix( ~ 0 + ., clinical[, c(outcome.var, clinical.vars)])
  
  ## identify participants with data for all data types
  common.ids <- rownames(clinical)
  for (i in 1:length(dat)) {
    common.ids <- intersect(common.ids, colnames(dat[[i]]))
  }
  ## construct a dataset including individuals with data for all data types
  ## and limit to features with strong univariate associations
  univariate.dat <- sapply(names(dat), function(name) {
    t(dat[[name]][univariate.predictors[, name], common.ids])
  }, simplify = F)
  univariate.dat$clinical <-
    clinical[common.ids, setdiff(colnames(clinical), outcome.var)]
  
  ## merge data for each data type into a single matrix
  univariate.dat.combine <- do.call(cbind, univariate.dat)
  
  ## impute missing values with the median value for the feature
  idx <- which(is.na(univariate.dat.combine), arr.ind = T)
  median.values <- apply(univariate.dat.combine, 2, median, na.rm = T)
  univariate.dat.combine[idx] <- median.values[idx[, 2]]
  
  ## outcome variable
  outcome <- clinical[common.ids, outcome.var]
  
  
  ## standardize features
  feature.var <- apply(univariate.dat.combine, 2, var)
  univariate.dat.combine <-
    univariate.dat.combine[, feature.var > 2e-16]
  univariate.dat.combine <- scale(univariate.dat.combine)
  outcome[, 1][which(outcome[, 1] == 0)] <- 1
  colnames(outcome) <- c('time', 'status')
  
  ################test data
  
  test.dat <- lapply(filenames_test, my.read.table)
  names(test.dat) <- sub(".txt", "", basename(filenames_test))
  
  ## extract the clinical data
  test.clinical <- test.dat$clinical
  test.dat$clinical <- NULL
  
  ## convert clinical data to a numeric matrix
  ## (this mainly means replacing categorical variables with dummy variables)
  test.clinical <- model.matrix(~0+., test.clinical[,c(outcome.var, clinical.vars)])
  
  ## identify participants with data for all data types
  common.ids <- rownames(test.clinical)
  for (i in 1:length(test.dat)){
    common.ids <- intersect(common.ids, colnames(test.dat[[i]]))
  }
  ## restrict test data to univariate predictor features
  ## identified in training and to individuals data for all data types
  test.dat <- sapply(names(test.dat), function(name) {
    t(test.dat[[name]][univariate.predictors[,name],common.ids])
  }, simplify=F)
  test.dat$clinical <- test.clinical[common.ids,setdiff(colnames(test.clinical),outcome.var)]
  
  ## merge data types into a single data matrix
  test.dat <- do.call(cbind, test.dat)
  
  ## standardize features
  feature.medians <- apply(test.dat,2,median,na.rm=T)
  features.var <- apply(test.dat, 2, var, na.rm=T)
  test.dat <- scale(test.dat)
  
  ## impute missing values with the median value for the feature
  idx <- which(is.na(test.dat),arr.ind=T)
  test.dat[idx] <- feature.medians[idx[,2]]
  test.outcome <- test.clinical[common.ids,outcome.var]
  
  
  if ("model1" == model_nume) {
    set.seed(123) 
    ## fit elastic net model
    cvfit <- cv.glmnet(
      univariate.dat.combine[,c(ncol(univariate.dat.combine)-0:5 )],
      outcome[,2],
      nfolds = 10, alpha = 0.5,
      family="binomial") ## the outcome variable is binary
    
    ## apply model in the training dataset
    train.predicts <- predict(
      cvfit,
      univariate.dat.combine[,c(ncol(univariate.dat.combine)-0:5 )],
      s="lambda.min",
      type="response")
    
    ## apply trained model in the test dataset
    test.predicts <- predict(
      cvfit,
      newx = test.dat[,colnames(univariate.dat.combine)[c(ncol(univariate.dat.combine)-0:5 )]],
      s = "lambda.min",
      type = "response")
    
    # a <- test.dat[,colnames(univariate.dat.combine)]
    ## evaluate performance
    
    
    ###model performance in training dataset
    pred.train <- prediction(train.predicts, outcome[,2])
    AUC.train <- auc(outcome[,2], as.vector(train.predicts))
    perf.train <- performance(pred.train,'tpr','fpr')
    
    ###model performance in testing dataset
    pred.test <- prediction(test.predicts, test.outcome[,2])
    AUC.test <- auc(test.outcome[,2],as.vector(test.predicts))
    perf.test <- performance(pred.test,'tpr','fpr')
    
  } else if (model_nume %in% c("model2","model3","model4","model5","model6")) {
    set.seed(123)
    ## fit elastic net model
    cvfit <- cv.glmnet(
      univariate.dat.combine[,-c(ncol(univariate.dat.combine)-0:5 )],
      outcome[,2],
      nfolds = 10, alpha = 0.5,
      family="binomial") ## the outcome variable is binary
    
    
    ## apply model in the training dataset
    train.predicts <- predict(
      cvfit,
      univariate.dat.combine[,-c(ncol(univariate.dat.combine)-0:5 )],
      s="lambda.min",
      type="response")
    
    ## apply trained model in the test dataset
    test.predicts <- predict(
      cvfit,
      newx = test.dat[,colnames(univariate.dat.combine)[-c(ncol(univariate.dat.combine)-0:5 )]],
      s = "lambda.min",
      type = "response")
    
    ###model performance in training dataset
    pred.train <- prediction(train.predicts, outcome[,2])
    AUC.train <- auc(outcome[,2], as.vector(train.predicts))
    perf.train <- performance(pred.train,'tpr','fpr')
    
    ###model performance in testing dataset
    pred.test <- prediction(test.predicts, test.outcome[,2])
    AUC.test <- auc(test.outcome[,2],as.vector(test.predicts))
    perf.test <- performance(pred.test,'tpr','fpr')
    
  } else if (model_nume %in% "model7") {
    set.seed(123)
    ## fit elastic net model
    cvfit <- cv.glmnet(
      univariate.dat.combine,
      outcome[,2],
      nfolds = 10, alpha = 0.5,
      family="binomial") ## the outcome variable is binary
    
    ## apply model in the training dataset
    train.predicts <- predict(
      cvfit,
      newx=univariate.dat.combine,
      s="lambda.min",
      type="response")
    
    test.predicts <- predict(
      cvfit,
      newx = test.dat[,colnames(univariate.dat.combine)],
      s = "lambda.min",
      type = "response")
    
    ## outcome to predict
    
    ###model performance in training dataset
    pred.train <- prediction(train.predicts, outcome[,2])
    AUC.train <- auc(outcome[,2], as.vector(train.predicts))
    perf.train <- performance(pred.train,'tpr','fpr')
    
    ###model performance in testing dataset
    pred.test <- prediction(test.predicts, test.outcome[,2])
    AUC.test <- auc(test.outcome[,2],as.vector(test.predicts))
    perf.test <- performance(pred.test,'tpr','fpr')
    
  } else if (model_nume %in% "model8") {
    set.seed(123)
    ## fit elastic net model
    cvfit <- cv.glmnet(
      univariate.dat.combine[,-c(ncol(univariate.dat.combine)-0:5 )],
      outcome[,2],
      nfolds = 10, alpha = 0.5,
      family="binomial") ## the outcome variable is binary
    
    ## apply model in the training dataset
    train.predicts <- predict(
      cvfit,
      newx=univariate.dat.combine[,-c(ncol(univariate.dat.combine)-0:5 )],
      s="lambda.min",
      type="response")
    
    test.predicts <- predict(
      cvfit,
      newx = test.dat[,colnames(univariate.dat.combine)[-c(ncol(univariate.dat.combine)-0:5 )]],
      s = "lambda.min",
      type = "response")
    
    ## outcome to predict
    
    ###model performance in training dataset
    pred.train <- prediction(train.predicts, outcome[,2])
    AUC.train <- auc(outcome[,2], as.vector(train.predicts))
    perf.train <- performance(pred.train,'tpr','fpr')
    
    ###model performance in testing dataset
    pred.test <- prediction(test.predicts, test.outcome[,2])
    AUC.test <- auc(test.outcome[,2],as.vector(test.predicts))
    perf.test <- performance(pred.test,'tpr','fpr')
  } 
  
  return(list(cvfit, train.predicts, perf.train, perf.test, AUC.train, AUC.test, data.frame(outcome, univariate.dat.combine), data.frame(test.outcome,test.dat[,colnames(univariate.dat.combine)]) ))
  
}



