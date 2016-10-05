
####################### ALGORITHMS FOR SPAM CLASSIFICATION #############################

#
#	@description
#		Loads libraries and source with needed functions. Also sets seed.
#
algorithm__prepareForPredictions <- function()
{
  source("data.r")
  library(tm) # Text mining: Corpus and Document Term Matrix
  library(kknn) # KNN model
  library(adabag) # Adaboost model
  library(class) # another knn model
  library(e1071) # Naive Bayes model
  library(CORElearn)
  library(ROCR) # drawing ROC curves
  set.seed(100)			# Set seed for reproducible results
}



#
#	@description
#		Uses algforithm KNN form "class" package to make classifiation for test dataset
#		
#	@input
#		@fun__prepare
#			Function Type: look at algorithm__prepareForPredictions
#			Description:	function to load libraries and set seed
#		@param_neigbours
#			Checks best number of neigbours used in KNN algorithm
#		@cost_FP
#			Number Type: cost for false positive error
#		@cost_FN
#			Number Type: cost for false negative error
#		@addInfo
#			prints predictions and draws ROC curve
#
algorithm__knn__class <- function(
  fun_prepare = algorithm__prepareForPredictions,
  param_neighbours = 5,
  cost_FP = 100,
  cost_FN = 1
)
{
  # function that loads libraries and sets seed
  fun_prepare()
  #import training and test data
  t = load__testingo_treningo_1()
  train <- t[[1]]
  test <- t[[2]]
  # add known classes to m_test matrix
  m_train <- train[,!colnames(train) %in% "CATEGORY_"]
  m_test <- test[,!colnames(train) %in% "CATEGORY_"]

  # Create model: training set, test set, training set classifier
  knn.pred <- knn(m_train, m_test, train$CATEGORY_, k=param_neighbours, prob=TRUE)
  conf.mat <- table("Predictions" = knn.pred, Actual = test$CATEGORY_)
  print(conf.mat)
  
  # ROC curves
  pred <- prediction(as.double(rep.int(1,dim(test)[1]))-as.double(as.vector(as.data.frame(unlist(attributes(knn.pred)))[c(-1,-2,-3),1])), test$CATEGORY_)
  perf <- performance(pred,"tpr","fpr")
  #perf2 <- performance(pred,"fpr","fnr")
  plot(perf, colorize = T, print.cutoffs.at=c(0,1), text.adj=c(1.2,1.2))
  auc_rdock <- performance(pred, "auc")
  print(unlist(auc_rdock@y.values))
  
  print(conf.mat[2,1]*cost_FN+conf.mat[1,2]*cost_FP)
  #x = attr(perf2, "x.values")[[1]]
  #y = attr(perf2, "y.values")[[1]]
  #cuttoff = attr(perf2, "alpha.values")[[1]]
  #best_performance = Inf
  #ibest = 1
  
  #all_negative = conf.mat[1,1] + conf.mat[2,1]
  #all_positive = conf.mat[1,2] + conf.mat[2,2]
  
  #for (i in 2:(length(x)-1)) {
  #    performance <- all_negative*x[i]*cost_FN + all_positive*y[i]*cost_FP
  #    if(performance < best_performance) {
  #      best_performance <- performance
  #      print(all_negative*x[i]*cost_FN)
  #      print(all_positive*y[i]*cost_FP)
  #      print(best_performance)
    #    ibest <- i
    #  }
    #}
    
    #print(cuttoff)
    #print(cuttoff[ibest])
    
    #prob <- attributes(knn.pred)$prob
    #new_class <- prob
    #for(i in 1:length(prob))
    #{
    #  if (prob[i]<cuttoff[ibest]) new_class[i] <- 0
    #  else new_class[i] <- 1
    #}
    #print(prob)
    #print(new_class)
    #conf.mat <- table("Predictions" = new_class, Actual = test$CATEGORY_)
    #print(conf.mat)
    #print(conf.mat[])
    
    #if(best_performance < conf.mat[2,1]*cost_FN + conf.mat[1,2]*cost_FP)
    #{
    #  conf.mat[2,1] = x[ibest] * all_negative	#FN
    #  conf.mat[1,2] = y[ibest] * all_positive	#FP
    #  conf.mat[2,2] = all_negative - conf.mat[2,1] 	#TN
    #  conf.mat[1,1] = all_positive- conf.mat[1,2] 	#TP
    #}
    
    #print(conf.mat)
}


#
#	@description
#		Test number of neighbours for algoritkm KNN form "class" package
#		
#	@input
#		@fun__prepare
#			Function Type: look at algorithm__prepareForPredictions
#			Description:	function to load libraries and set seed
#		@cost_FP
#			Number Type: cost for false positive error
#		@cost_FN
#			Number Type: cost for false negative error
#		@start_neigh
#			Number Type: number from with the number of neighbours is being tested
#		@stop_neigh
#			Number Type: number to with the number of neighbours is being tested
#		@addInfo
#			prints predictions and draws ROC curve
#
algorithm__knn__class__test <- function(
  fun_prepare = algorithm__prepareForPredictions,
  cost_FP = 100,
  cost_FN = 1,
  start_neigh = 1,
  stop_neigh =20
)
{
  # function that loads libraries and sets seed
  fun_prepare()
  #import training and test data
  t = load__testingo_treningo_1()
  train <- t[[1]]
  test <- t[[2]]
  # add known classes to m_test matrix
  m_train <- train[,!colnames(train) %in% "CATEGORY_"]
  m_test <- test[,!colnames(test) %in% "CATEGORY_"]
  
  #the biggest cost that can only be decreased
  cost <- 11000
  best_neighbours <- 0
  
  for (NEIGHBOURS in start_neigh:stop_neigh)
  {
    # Create model: training set, test set, training set classifier
    knn.pred <- knn(m_train, m_test, train$CATEGORY_, k=NEIGHBOURS)
    conf.mat <- table("Predictions" = knn.pred, Actual = test$CATEGORY_)
    #updates best_neigbours parameter
    cost_tmp <- conf.mat[2,1]*cost_FN + conf.mat[1,2]*cost_FP
    if(cost_tmp < cost)
    {
      best_neighbours <- NEIGHBOURS
      cost <- cost_tmp
    }
  }
  
  #classify with best parameter
  knn.pred <- knn(m_train, m_test, train$CATEGORY_, k=best_neighbours, prob=TRUE)
  conf.mat <- table("Predictions" = knn.pred, Actual = test$CATEGORY_)
  print(conf.mat)
  print(best_neighbours)
  
  # ROC curves
  pred <- prediction(as.double(rep.int(1,dim(test)[1]))-as.double(as.vector(as.data.frame(unlist(attributes(knn.pred)))[c(-1,-2,-3),1])), test$CATEGORY_)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, colorize = T, print.cutoffs.at=c(0,1), text.adj=c(1.2,1.2))
  auc_rdock <- performance(pred, "auc")
  print(unlist(auc_rdock@y.values))
}


#
#	@description
#		Uses algforithm KNN from "kknn" package to make classifiation for test dataset
#		
#	@input
#		@fun__prepare
#			Function Type: look at algorithm__prepareForPredictions
#			Description:	function to load libraries and set seed
#		@minkowski_dist
#			Number Type: minkowski distance - number determining computed norm
#		@param_kernel
#			String Type: Kernel to use from ("rectangular" - standard unweighted knn
#                                      "triangular", "epanechnikov" (or beta(2,2)), "biweight" (or beta(3,3))
#                                      "triweight" (or beta(4,4)), "cos", "inv", "gaussian", "rank", "optimal")
#		@param_k
#			Number Type: number of neighbours considered
#		@cost_FP
#			Number Type: cost for false positive error
#		@cost_FN
#			Number Type: cost for false negative error
#		@addInfo
#			prints predictions and draws ROC curve
#
algorithm__knn__kknn <- function(
fun_prepare = algorithm__prepareForPredictions,
minkowski_dist = 1,
param_kernel = "optimal",
param_k = 10,
cost_FP = 100,
cost_FN = 1
)
{
  # function that loads libraries and sets seed
  fun_prepare()
  #import training and test data
  t = load__testingo_treningo_1()
  train <- t[[1]]
  test <- t[[2]]
  # add known classes to m_test matrix
  m_test <- test[,!colnames(test) %in% "CATEGORY_"]
  #sets formula for classifier
  formula <- as.formula(paste('CATEGORY_ ~',paste0(setdiff(colnames(train),c('type','oct','jalapeno')),collapse=' + ')))
  # Create model: training set, test set, training set classifier
  knn.pred <- kknn(CATEGORY_~., train, m_test, k= param_k ,distance = minkowski_dist, kernel = param_kernel)
  conf.mat <- table(knn.pred$fitted.values, test$CATEGORY_)
  print(conf.mat)
  
  # ROC curves
  pred <- prediction(as.double(as.vector(knn.pred$prob[,1])), test$CATEGORY_)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, colorize = T, print.cutoffs.at=c(0,1), text.adj=c(1.2,1.2))
  auc_rdock <- performance(pred, "auc")
  print(unlist(auc_rdock@y.values))
  
  ################# modyfikacja progu prawdopodobienstw przy zadanej funkcji kosztu ######################
  
  perf2 <- performance(pred,"fpr","fnr")
  x = attr(perf2, "x.values")[[1]]
  y = attr(perf2, "y.values")[[1]]
  cuttoff = attr(perf2, "alpha.values")[[1]]
  best_performance = Inf
  ibest = 1
  
  cost_FN = 1
  cost_FP = 100
  all_negative = conf.mat[1,1] + conf.mat[2,1]
  all_positive = conf.mat[1,2] + conf.mat[2,2]
  
  for (i in 2:(length(x)-1)) {
    performance <- all_negative*x[i]*cost_FN + all_positive*y[i]*cost_FP
    if(performance < best_performance) {
      best_performance <- performance
      ibest <- i
    }
  }
  
  prob <- knn.pred$prob[,1]
  new_class <- prob
  for(i in 1:length(prob))
  {
    if (prob[i]<cuttoff[ibest]) new_class[i] <- 0
    else new_class[i] <- 1
  }

  conf.mat <- table(new_class, test$CATEGORY_)
  print(conf.mat)
  print(conf.mat[1,1]*cost_FN+conf.mat[2,2]*cost_FP)
  }


#
#	@description
#		Tests parameters for algorithm KNN from "kknn" package to make classifiation for test dataset
#		
#	@input
#		@fun__prepare
#			Function Type: look at algorithm__prepareForPredictions
#			Description:	function to load libraries and set seed
#		@mink_start
#			Number Type: minimal minkowski distance to be tested
#		@mink_stop
#			Number Type: maximalminkowski distance to be tested
#		@k_start
#			Number Type: minimal number of neighbours to be testes
#		@k_stop
#			Number Type: maximal number of neighbours to be tested
#		@cost_FP
#			Number Type: cost for false positive error
#		@cost_FN
#			Number Type: cost for false negative error
#		@addInfo
#			prints predictions and draws ROC curve
#
algorithm__knn__kknn__test <- function(
  fun_prepare = algorithm__prepareForPredictions,
  mink_start = 1,
  mink_stop =3,
  k_start = 15,
  k_stop = 20,
  cost_FP = 100,
  cost_FN = 1
)
{
  # function that loads libraries and sets seed
  fun_prepare()
  #import training and test data
  t = load__testingo_treningo_1()
  train <- t[[1]]
  test <- t[[2]]
  # add known classes to m_test matrix
  m_test <- test[,!colnames(test) %in% "CATEGORY_"]
  #sets formula for classifier
  formula <- as.formula(paste('CATEGORY_ ~',paste0(setdiff(colnames(train),c('type','oct','jalapeno')),collapse=' + ')))
  #the biggest cost that can only be decreased
  cost <- 11000
  best_output <- ""
  param_mink <- 0
  param_k <- 0
  param_kernel <- ""
  
  clist <- c('optimal')#, 'rectangular', 'triangular', 'epanechnikov', 'biweight', 'triweight', 'cos', 'inv', 'gaussian', 'rank')
  
  # loop which is testing parameters
  for (i in clist)
  {
    for(j in mink_start:mink_stop)
    {
      for(k in k_start:k_stop)
      # Create model: training set, test set, training set classifier
      knn.pred <- kknn(CATEGORY_~., train, m_test, k= k ,distance = j, kernel = i)
      tab <- table(knn.pred$fitted.values, test$CATEGORY_)
      # computes actual cost and updates best parameters
      cost_tmp <- tab[2,1]*cost_FN + tab[1,2]*cost_FP
      if(cost_tmp < cost)
      {
        param_kernel <- i
        param_mink <- j
        param_k <- k
        best_output <- tab
        cost <- cost_tmp
      }
    }
  }
  
  knn.pred <- kknn(CATEGORY_~., train, m_test, k= param_k ,distance = param_mink, kernel = param_kernel)
  print(table(knn.pred$fitted.values, test$CATEGORY_))
  print(param_kernel)
  print(param_mink)
  print(param_k)
  
  # ROC curves
  pred <- prediction(as.double(as.vector(knn.pred$prob[,1])), test$CATEGORY_)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, colorize = T, print.cutoffs.at=c(0,1), text.adj=c(1.2,1.2))
  auc_rdock <- performance(pred, "auc")
  print(unlist(auc_rdock@y.values))
}


#
#	@description
#		Uses algforithm Naive Bayes to make classifiation for test dataset
#		
#	@input
#		@fun__prepare
#			Function Type: look at algorithm__prepareForPredictions
#			Description:	function to load libraries and set seed
#   @param_laplace
#			wsp wygladzania Laplaca, addytywne wygladzanie (domy slnie 0 - bez wygladzania)
#		@param_treshold
#			prog zastepujacy gdy prawdopienstwo sa w otoczeniu epsilonowy
#     (jak jest za malo danych, to sie dodaje pewne odch stand)
#		@param_epsilon
#			epsilonowe otoczenie do wygladzania laplace'a
#		@cost_FP
#			Number Type: cost for false positive error
#		@cost_FN
#			Number Type: cost for false negative error
#		@addInfo
#     as default best parameters found
#			prints predictions and draws ROC curve
#
algorithm__naiveBayes <- function(
fun_prepare = algorithm__prepareForPredictions,
param_laplace = 100,
param_treshold = 0,
param_epsilon = 0,
cost_FP = 100,
cost_FN = 1
)
{
  # function that loads libraries and sets seed
  fun_prepare()
  #import training and test data
  t = load__testingo_treningo_1()
  train <- t[[1]]
  test <- t[[2]]
  # add known classes to m_test matrix
  m_test <- test[,!colnames(test) %in% "CATEGORY_"]
  #sets formula for classifier
  formula <- as.formula(paste('CATEGORY_ ~',paste0(setdiff(colnames(train),c('type','oct','jalapeno')),collapse=' + ')))
  
  tune.control <- tune.control(random = FALSE, nrepeat = 1, repeat.aggregate = min,
                               sampling = c("cross"), sampling.aggregate = mean,
                               cross = 10, best.model = TRUE, performances = TRUE) 
  
  #classifier defined on training data - laplace 100
  classifier <- naiveBayes(formula, data=train, laplace = param_laplace, type = c("class", "raw"), treshold = param_treshold, eps=param_epsilon, tune.control)
  #class(classifier)
  test_pred <- predict(classifier, newdata=m_test, type = "class")
  test_pred2 <- predict(classifier, newdata=m_test, type="raw")
  #check the predictions
  conf.mat <- table(test_pred, test$CATEGORY_)
  print(conf.mat)
  # ROC curves
  pred <- prediction(as.double(as.vector(test_pred2[,1])), test$CATEGORY_)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, colorize = T, print.cutoffs.at=c(0,1), text.adj=c(1.2,1.2))
  auc_rdock <- performance(pred, "auc")
  print(unlist(auc_rdock@y.values))
 
  ############################## modyfikacje progu zgodnie z funkcja kosztu ###################### 
  
  perf2 <- performance(pred,"fpr","fnr")
  x = attr(perf2, "x.values")[[1]]
  y = attr(perf2, "y.values")[[1]]
  cuttoff = attr(perf2, "alpha.values")[[1]]
  best_performance = Inf
  ibest = 1
  
  cost_FN = 1
  cost_FP = 100
  all_negative = conf.mat[1,1] + conf.mat[2,1]
  all_positive = conf.mat[1,2] + conf.mat[2,2]
  
  for (i in 2:(length(x)-1)) {
    performance <- all_negative*x[i]*cost_FN + all_positive*y[i]*cost_FP
    if(performance < best_performance) {
      best_performance <- performance
      ibest <- i
    }
  }
  
  prob <- test_pred2[,1]
  new_class <- prob
  for(i in 1:length(prob))
  {
    if (prob[i]<cuttoff[ibest]) new_class[i] <- 0
    else new_class[i] <- 1
  }

  conf.mat <- table(new_class, test$CATEGORY_)
  print(conf.mat)
  print(conf.mat[1,1]*cost_FN+conf.mat[2,2]*cost_FP)  
}


#
#	@description
#		Uses algforithm Adaboost to make classifiation for test dataset
#		
#	@input
#		@fun__prepare
#			Function Type: look at algorithm__prepareForPredictions
#			Description:	function to load libraries and set seed
#   @param_coef
#			method of learning coeficients (Brieman, Freund or Zhu);
#		@param_mfinal
#			number of rounds
#		@cost_FP
#			Number Type: cost for false positive error
#		@cost_FN
#			Number Type: cost for false negative error
#		@addInfo
#			prints predictions and draws ROC curve
#
algorithm__adaboost <- function(
fun_prepare =  algorithm__prepareForPredictions,
param_coef = 'Breiman',
param_mfinal = 100,
cost_FP = 100,
cost_FN = 1
)
{
  # function that loads libraries and sets seed
  fun_prepare()
  #import training and test data
  t = load__testingo_treningo_1()
  train <- t[[1]]
  test <- t[[2]]
  # add known classes to m_test matrix
  m_test <- test[,!colnames(test) %in% "CATEGORY_"]
  #sets formula for classifier
  formula <- as.formula(paste('CATEGORY_ ~',paste0(setdiff(colnames(train),c('type','oct','jalapeno')),collapse=' + ')))
  # clasifier
  adaboost<-boosting(formula, data=train, boos=TRUE, mfinal=param_mfinal, coeflearn=param_coef)
  test_pred <- predict(adaboost, newdata=m_test)
  # table with predictions
  conf.mat <- table(test_pred$class, test$CATEGORY_)
  print(conf.mat)
  # paint ROC curve
  pred <- prediction(as.double(as.vector(test_pred$prob[,1])), test$CATEGORY_)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, colorize = T, print.cutoffs.at=c(0,1), text.adj=c(1.2,1.2))
  auc_rdock <- performance(pred, "auc")
  print(unlist(auc_rdock@y.values))
  
  ############################## modyfikacje progu zgodnie z funkcja kosztu ###################### 
  
  perf2 <- performance(pred,"fpr","fnr")
  x = attr(perf2, "x.values")[[1]]
  y = attr(perf2, "y.values")[[1]]
  cuttoff = attr(perf2, "alpha.values")[[1]]
  best_performance = Inf
  ibest = 1
  
  all_negative = conf.mat[1,1] + conf.mat[2,1]
  all_positive = conf.mat[1,2] + conf.mat[2,2]
  
  for (i in 2:(length(x)-1)) {
    performance <- all_negative*x[i]*cost_FN + all_positive*y[i]*cost_FP
    if(performance < best_performance) {
      best_performance <- performance
      ibest <- i
    }
  }
  
  prob <- test_pred$prob[,1]
  new_class <- prob
  for(i in 1:length(prob))
  {
    if (prob[i]<cuttoff[ibest]) new_class[i] <- 0
    else new_class[i] <- 1
  }
  
  conf.mat <- table(new_class, test$CATEGORY_)
  print(conf.mat)
  print(conf.mat[1,1]*cost_FN+conf.mat[2,2]*cost_FP)
}


#
#	@description
#		Function that test parameters for Adaboost algorithm;
#		it tests method of learning coeficients (Brieman, Freund or Zhu)
#		and the most efficient number of rounds - parameter mfinal.
#   For set costs of false negative ad false positive error
#   it computes the best parameter set, for test data.
#		
#	@input
#		@fun__prepare
#			Function Type: look at algorithm__prepareForPredictions
#			Description:	function to load libraries and set seed
#		@cost_FP
#			Number Type: cost for false positive error
#		@cost_FN
#			Number Type: cost for false negative error
#		@start_mfinal
#			Number Type: number from with the number of iteratios is being tested
#		@stop_mfinal
#			Number Type: number to with the number of iteratios is being tested
#		@addInfo
#			it shows predictions for best set of parameters and draws ROC curves
#
algorithm__adaboost__test <- function(
fun_prepare = algorithm__prepareForPredictions,
cost_FP = 100,
cos_FN = 1,
start_mfinal = 30,
stop_mfinal = 35
)
{
  # function that loads libraries and sets seed
  fun_prepare()
  #import training and test data
  t = load__testingo_treningo_1()
  train <- t[[1]]
  test <- t[[2]]
  # add known classes to m_test matrix
  m_test <- test[,!colnames(test) %in% "CATEGORY_"]
  #sets formula for classifier
  formula <- as.formula(paste('CATEGORY_ ~',paste0(setdiff(colnames(train),c('type','oct','jalapeno')),collapse=' + ')))
  #the biggest cost that can only be decreased
  cost <- 11000
  best_output <- ""
  param_coef <- ""
  param_mfinal <- 0
  # list of coeficient algorithms to be tested
  clist <- c('Breiman','Freund','Zhu')
  
  # loop which is testing parameters
  for (i in clist)
  {
    for(j in start_mfinal:stop_mfinal)
    {
      adaboost<-boosting(formula, data=train, boos=TRUE, mfinal=j, coeflearn=i)
      test_pred <- predict(adaboost, newdata=m_test)
      tab <- table(test_pred$class, test$CATEGORY_)
      # computes actual cost and updates best parameters
      cost_tmp <- tab[1,1]*cost_FN + tab[2,2]*cost_FP
      if(cost_tmp < cost)
      {
        param_coef <- i
        param_mfinal <- j
        best_output <- tab
        cost <- cost_tmp
      }
    }
  }
  
  # ivoke algorithm on best parameter set
  adaboost<-boosting(formula, data=train, boos=TRUE, mfinal=param_mfinal, coeflearn=param_coef)
  test_pred <- predict(adaboost, newdata=m_test)
  # table with predictions
  print(table(test_pred$class, test$CATEGORY_))
  # print best parameters
  print(param_coef)
  print(param_mfinal)
  # paint ROC curve
  pred <- prediction(as.double(as.vector(test_pred$prob[,1])), test$CATEGORY_)
  perf <- performance(pred,"tpr","fpr")
  plot(perf, colorize = T, print.cutoffs.at=c(0,1), text.adj=c(1.2,1.2))
  auc_rdock <- performance(pred, "auc")
  print(unlist(auc_rdock@y.values))
}
  