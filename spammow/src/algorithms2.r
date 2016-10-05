#------------------------------------------------------------------
#
#	Notes
#
#	Confusion should be given or returned between functions
#	in format:
#  true
# pred    0   1
#    0    TN  FN
#    1    FP  TP

# costFunction = tab[1,2]*cost_FN + tab[2,1]*cost_FP
#tab[1,2] = fnr * allnegative_testingo		#FN
#tab[2,1] = fpr * allpositive_testingo		#FP
#tab[1,1] = allnegative_testingo - tab[1,2] 	#TN
#tab[2,2] = allpositive_testingo - tab[2,1] 	#TP
#------------------------------------------------------------------





#------------------------------------------------------------------
#
##
###
#	Init
###
##
#



library(ROCR) # drawing ROC curves
library(e1071)
library(tree)
source("data.r")
tt = load__testingo_treningo_1()
treningo  = tt[[1]]
testingo = tt[[2]]
allpositive_testingo = sum(testingo$CATEGORY_ == 1)
allnegative_testingo = sum(testingo$CATEGORY_ == 0)
cost_FN = 1
cost_FP = 100
#------------------------------------------------------------------





#------------------------------------------------------------------
#
##
###
#	Putting blocks together
###
##
#

all <- function () {
	algorithm__svm_test()
	algorithm__decisionTree_test()
	algorithm__neuralNetwork_test()
}

algorithm__svm_test <- function () {
	params_ = list( 
		gamma = 10^(-4:1), 
		cost  = 10^(1:2))
	myTune(
		params_ = params_,
		fun_traintest = algorithm__svm
	)
}

algorithm__decisionTree_test <- function () {
	params_ = list( empty = 1 )
	myTune(
		params_ = params_,
		fun_traintest = algorithm__decisionTree
	)
}

algorithm__neuralNetwork_test <- function () {
	params_ = list( 
		hidden = c(10, 50, 150, 500), algorithm = c("backprop", "rprop+"), learningrate = 0.3 )
	myTune(
		params_ = params_,
		fun_traintest = algorithm__neuralNetwork
	)
}

#------------------------------------------------------------------





#------------------------------------------------------------------
#
##
###
#	Helper
###
##
#



#
#	@description
#		Draw ROC curve
#		
#	@input
#		@decisionValues
#			@Class: numeric (vector of numbers)
#			@Description: probabilites P(SPAM|document)
#
#	@return
#		nothing
#
drawROCcurve <- function (
	probabilities_SPAM_I_d 
) 
{
	# ROC curves
	pred <- prediction(probabilities_SPAM_I_d, testingo$CATEGORY_ )
	perf <- performance(pred,"tpr","fpr")
	# plot(perf)
	plot(perf, text.adj=c(1.2,1.2))
}

findMinimalPerformance <- function (
	probabilities_SPAM_I_d
)
{
	pred <- prediction(probabilities_SPAM_I_d, testingo$CATEGORY_ )
	perf <- performance(pred,"fnr","fpr")
	x = attr(perf, "x.values")[[1]]
	y = attr(perf, "y.values")[[1]]
	cuttoff = attr(perf, "alpha.values")[[1]]

	best_performance = Inf
	ibest = 1
	for (i in 1:length(x)) {
		performance <- allnegative_testingo* x[i]*cost_FN +  allpositive_testingo* y[i]*cost_FP
		if(performance < best_performance) {
			best_performance <- performance
			ibest <- i
		}
	}

	list( 
		performance = best_performance, 
		fnr = x[ibest], 
		fpr = y[ibest],
		cuttoff = cuttoff[ibest]
	)
}

findAUC <- function (
	probabilities_SPAM_I_d
) 
{
	# ROC curves
	pred <- prediction(probabilities_SPAM_I_d, testingo$CATEGORY_ )
	auc.perf = performance(pred, measure = "auc")
	(auc.perf@y.values)[[1]]
}

myTune <- function (
	params_,
	fun_traintest
) 
{
	params_ = expand.grid(params_)
	best_params = list ()
	best_performance = Inf
	performances = matrix ( nrow = nrow(params_), ncol = 2 )
	colnames(performances)  <- c("performance", "auc")

	for ( iparams in 1:nrow(params_) ) {
		print(".")
		r = fun_traintest(params_[iparams, ])
		decisionValues = r$decisionValues
		tab = r$confusionMatrix

		#find minimal performance
		mo = findMinimalPerformance(decisionValues)
		#.

		#find AUC
		auc = findAUC(decisionValues)
		#

		#Print performance
		print(mo$performance)
		performances[iparams, 1] <- mo$performance
		print(params_[iparams, ])
		print(auc)
		performances[iparams, 2] <- auc
		#

		if(mo$performance < best_performance)
		{
			best_performance <- mo$performance
			best_params <- params_[iparams, ]

			tab[1,2] = mo$fnr * allnegative_testingo	#FN
			tab[2,1] = mo$fpr * allpositive_testingo	#FP
			tab[1,1] = allnegative_testingo - tab[1,2] 	#TN
			tab[2,2] = allpositive_testingo - tab[2,1] 	#TP
			
			best_tab = tab
			best_auc = auc
			best_decisionValues = decisionValues
		}
	}

	print ("---")
	print ("All params: ")
	print (params_)

	print ("---")
	print ("All performances: ")
	print (performances)
	print ("---")

	print ("(Best) performance: ")
	print (best_performance)
	print ("---")

	print ("(Best) params: ")
	print (best_params)
	print ("---")

	print ("(Best) Confusion matrix: ")
	print (best_tab)
	print ("---")

	print ("(Best) AUC of ROC: ")
	print (best_auc)
	print ("---")

	drawROCcurve(best_decisionValues)
}
#------------------------------------------------------------------





#------------------------------------------------------------------

algorithm__svm <- function ( params ) 
{
	model = svm(
		formula = CATEGORY_ ~ .,  
		data = treningo, 
		kernel = "radial",
		scale = FALSE,
		gamma = params$gamma,
		cost  = params$cost,
		probability = TRUE)
	prediction <- predict(
		model, 
		testingo[,-nrow(testingo)], 
		decision.values = TRUE)
	decisionValues = attr(prediction, "decision.values")
	tab <- table(pred = prediction, true = testingo$CATEGORY_)
	tab <- tab[, c(2,1)]
	tab <- tab[c(2,1), ]

	list( decisionValues = decisionValues, confusionMatrix = tab )
}

algorithm__decisionTree <- function ( params ) 
{
	model = tree(
		formula = CATEGORY_ ~ .,  
		data = treningo,


	)
	decisionValues <- predict(
		model, 
		testingo[,-nrow(testingo)]
	)

	predictionC = ifelse(decisionValues<0.5, 0, 1)[, 1]
	tab <- table(pred = predictionC, true = testingo$CATEGORY_)
	tab <- tab[, c(2,1)]

	list( decisionValues = decisionValues[, 1], confusionMatrix = tab )
}

algorithm__neuralNetwork <- function ( params ) 
{
	library(neuralnet)

	nntreningo <- treningo
	nntreningo$CATEGORY_ <- as.numeric(as.character(nntreningo$CATEGORY_))
	nntestingo <- testingo
	nntestingo$CATEGORY_ <- as.numeric(as.character(nntestingo$CATEGORY_))

	n <- names(nntreningo)
	f <- as.formula(
		paste("CATEGORY_ ~", 
			paste(n[!n %in% "CATEGORY_"], collapse = " + ")))

	model <- neuralnet(
		formula = f,
		data = nntreningo,
		hidden = c(params$hidden),
		linear.output=FALSE,
		algorithm=params$algorithm, 
		learningrate = params$learningrate,
		rep = 3
	)

	decisionValues = compute(model, nntestingo[, 1:(ncol(nntestingo)-1)])$net.result
	predictionC = ifelse(decisionValues<0.5, 0, 1)[, 1]
	tab <- table(pred = predictionC, true = testingo$CATEGORY_)
	tab <- tab[, c(2,1)]

	detach("package:neuralnet", unload=TRUE)

	list( decisionValues = decisionValues[, 1], confusionMatrix = tab )
}
#------------------------------------------------------------------










