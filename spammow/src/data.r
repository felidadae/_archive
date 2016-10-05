


#------------------------------------------------------------------
library(CORElearn)
library(tm) 		# Text mining: Corpus and Document Term Matrix
library(class) 		# KNN model
#------------------------------------------------------------------





#------------------------------------------------------------------
#
##
###
#	Init
###
##
#



set.seed(100)			# Set seed for reproducible results
source("../config.r")	# For get basepath to spam dataset
#------------------------------------------------------------------





#------------------------------------------------------------------
#
##
###
#	Load what prepareData_* produced from file
###
##
#

#
#	@Description:
#		trainingoPSize = 0.7,
# 		fun__createCorpuses = createCorpuses_fromAllData,
# 		keywordsN = 100,
# 		mindelta__P__w = 0.07
#
#		dim(treningo)
#		[1] 4232   67
#		dim(testingo)
#		[1] 1813   67
#
#		treningo$CATEGORY_ is {0==HAM or 1==SPAM}
#		testingo$CATEGORY_ ^^^^^^^^^^^^^^^^^^^^^^
#			($CATEGORY_ is last column)
#
#		Look at function >>prepareData_a<<
#
load__testingo_treningo_1 <- function() {
	load("../savedData/testingo_1.Rdata")
	load("../savedData/treningo_1.Rdata")
	(list(treningo, testingo))
}
#------------------------------------------------------------------





#------------------------------------------------------------------
#
##
###
#	Putting blocks together	
###
##
#

#
#	@description
#		Function return training and testing dataset;
#		
#	@input
#		@trainingoPSize
#			Description: How much of all available data
#				will constitue training set;   
#		
#	@return
#		Class of the object: List of two DataFrame objects
#		Description:	list(treningo, testingo)
#			Each row in each dataset represents 
#			document in keywords space; Each attribut 
#			is equal to frequency of a given word in 
#			a given document; last attribut should be
#			interpreted as class SPAM/HAM
#		Important note: column CATEGORY_ exists in dataframes:
#			1 -> SPAM
#			0 -> HAM	
#			
#
prepareData_a <- function (
	trainingoPSize = 0.7,
	fun__createCorpuses = createCorpuses_fromAllData,
	keywordsN = 100,
	mindelta__P__w = 0.07
) 
{
	corpuses   <- fun__createCorpuses()			#Call user function
	spamCorpus <- corpuses[[1]]
	hamCorpus  <- corpuses[[2]]	
	remove(corpuses)
	spamCorpus <- filterCorpus_standard(spamCorpus)	#Call user function
	hamCorpus  <- filterCorpus_standard(hamCorpus)	#Call user function

	keywords = chooseKeywords_a(
		spamCorpus 		= spamCorpus, 
		hamCorpus  		= hamCorpus, 
		keywordN 		= keywordsN, 
		mindelta__P__w 	= mindelta__P__w, 
		additionalInfo 	= TRUE
	)

	#
	#	returns two dataframes
	#
	listOfdocuments = castDocumentsToKeywords(
		spamCorpus 	= spamCorpus,
		hamCorpus 	= hamCorpus,
		keywords 	= keywords
	)

	spam 	= listOfdocuments[[1]]
	ham  	= listOfdocuments[[2]]

	Nspam	= dim(spam)[1]	
	Nham	= dim(ham)[1]

	all_am = rbind( spam, ham )
	all_am = all_am[sample(nrow(all_am)),]

	Nall_am <- nrow(all_am)
	Lall_am <- trainingoPSize * Nall_am #border element
	treningo <- all_am[1:Lall_am, ]
	testingo <- all_am[(Lall_am+1):Nall_am, ]

	(list(treningo, testingo)) #what a great pair!
}
#------------------------------------------------------------------





#------------------------------------------------------------------
#
##
###
#	Prepare keywords, prepare training and testing datasets
###
##
#

#
#	@description
#		Cast corpuses of documents to 
#		keywords vector representation
#		
#	@input
#		@spamCorpus
#			Class: 	Corpus
#			Description: corpus with spam data   
#		@hamCorpus
#			Class: 	Corpus
#			Description: corpus with ham data
#   	@keywords
#			Class: numeric?
#			vector of keywords
#		
#	@return
#		Class of the object: List of two DataFrame objects
#		Description:	list(spamDV_df, hamDV_df)
#		Important note: column CATEGORY_ exists in dataframes:
#			1 -> SPAM
#			0 -> HAM	
#			
#
castDocumentsToKeywords <- function(
	spamCorpus,
	hamCorpus,
	keywords
)
{
	#
	#	@dtmK 	<- document term matrix only for keywords
	#

	dtmK_spam <- DocumentTermMatrix(
		spamCorpus, 
		control=list(dictionary = keywords[,1]))
	dtmK_ham <- DocumentTermMatrix(
		hamCorpus, 
		control=list(dictionary = keywords[,1]))

	spam_df <- as.data.frame(
		data.matrix(dtmK_spam), 
		stringsAsfactors = FALSE)
	ham_df <- as.data.frame(
		data.matrix(dtmK_ham), 
		stringsAsfactors = FALSE)

	#add category 
	spam_df$CATEGORY_ = as.factor(1)
	ham_df$CATEGORY_  = as.factor(0)
	#spam_df$CATEGORY_ = factor(x=1, levels=c(0,1))
	#ham_df$CATEGORY_  = factor(x=0, levels=c(0,1))

	return ( list(spam_df, ham_df) )
}


#
#	@description
#		Returns keywords set;
#		It chooses the set of all words appearing in 
#		the spam and ham corpuses (input arguments) a
#		subset which will be used to represent documents.
#
#		Documents will be represented as vectors of
#		frequencies of that keywords;
#		
#	@input
#		@spamCorpus
#			corpus with spam data   
#		@hamCorpus
#			corpus with ham data
#		@keywordN
#			how many keywords you do want; 
#			may be less, that is maximum;
#		@mindelta__P__w
#			look at doc/report.{pdf|tex} -> |P(w|S)-P(w|H)|
#		@additionalInfo
#			if so additional columns are added to keywords
#			(to )
#
#	@return
#		object of class:	ClassName
#		description:		DescriptionOfOutput
#		useful operations:	UsefulOperations
#
chooseKeywords_a <- function(
	spamCorpus,
	hamCorpus,
	keywordN = 100,
	mindelta__P__w = 0.3,
	additionalInfo = FALSE
) 
{
	# Create P__w_
	create_P__w_ <- function (corpus) {
		dtm <- DocumentTermMatrix(corpus)
		dtm_df <- as.data.frame(
			data.matrix(dtm), 
			stringsAsfactors = FALSE)
		P__w_ = colSums(dtm_df>1)/dim(dtm_df)[1]
	}
	P__w_SPAM = create_P__w_(spamCorpus)
	P__w_HAM  = create_P__w_(hamCorpus)
	hash__P__w_SPAM <- new.env()	# HASHMAPS
	hash__P__w_HAM  <- new.env()	# HASHMAPS
	for (i in 1:length(P__w_SPAM)) {
		hash__P__w_SPAM[[attributes(P__w_SPAM)$names[i]]] <- i
	}
	for (i in 1:length(P__w_HAM)) {
		hash__P__w_HAM[[attributes(P__w_HAM)$names[i]]] <- i
	}


	allwords = unique(c(
		attributes(P__w_SPAM)$names, 
		attributes(P__w_HAM)$names))	

	wordsWithK = matrix(
		data = NA, 
		nrow = length(allwords), 
		ncol = 4)

	iadded = 0
	#----------------------------------------------
	Nallwords = length(allwords)
	# Nallwords = 1000 #debug
	for (iword in 1:Nallwords) {
		word = allwords[iword]
		idx__spam = hash__P__w_SPAM[[word]] # from HASHMAP
		idx__ham  = hash__P__w_HAM[[word]]  # from HASHMAP

		if ( !is.integer(idx__spam) ) {
			P_word_SPAM = 0.0
		} else {
			P_word_SPAM = P__w_SPAM[ idx__spam ] 
		}
		if ( !is.integer(idx__ham) ) {
			P_word_HAM = 0.0
		} else {
			P_word_HAM = P__w_HAM[ idx__ham ]
		}

		delta = abs(
			P_word_HAM - 
			P_word_SPAM); 

		if (delta > mindelta__P__w) {
			K = max( P_word_SPAM, P_word_HAM ) /
				min( P_word_SPAM, P_word_HAM )
			wordsWithK[iadded, 1] = word; 
			wordsWithK[iadded, 2] = round(K, digits=2);
			wordsWithK[iadded, 3] = 
				round(
					max( P_word_SPAM, P_word_HAM ),
					digits=2
				)
			if (P_word_SPAM>P_word_HAM) {
				wordsWithK[iadded, 4] = "SPAM"
			} else {
				wordsWithK[iadded, 4] = "HAM"
			}
			iadded <- iadded + 1;
		}	
	}
	#----------------------------------------------

	wordsWithK <- wordsWithK[1:(iadded-1), ]
	wordsWithK <- wordsWithK[ order(wordsWithK[,3], decreasing = TRUE), ]
	wordsWithK <- wordsWithK[ order(wordsWithK[,2], decreasing = TRUE), ]

	if (keywordN > iadded) {
		if (additionalInfo) {
			(wordsWithK)
		} else {
			(wordsWithK[,1])
		}			
	} else {
		if (additionalInfo) {
			(wordsWithK[1:keywordN, ])
		} else {
			(wordsWithK[1:keywordN, 1])
		} 
	}
}



#
#	@description
#		return documents paths from given directory of
#		spam dataset
#
#	@input
#		@dirnames
#			@Class: characters vector
#			description: which dirs from p__spam_dataset 
#				load;
#				possible values: {spam, easy_ham, ...}
#
# 	@return
#		object of class:	DirSource 
#		description: contains paths to all documents
#			object$filelist[idocument]	
#
getDocumentsPaths <- function( dirnames ) {
	source("../config.r")

	dirpaths = paste (
		p__spam_dataset, 
		paste("/", dirnames, sep=""), 
		sep=""
	)

	paths <- DirSource(
		dirpaths,
		encoding = "UTF-8",
		pattern = "^\\d.*"
	)

	return (paths)
} 



#
#	@description
#		Creates object of class Corpus
#		with documents which paths were
#		given into arg documentsPaths;
#		
#		
#	@input
#		@documentsPath
#			Class: DirSource
#		@n_start @n_stop
#			Range which documents take
#
#	@return
#		object of class:	Corpus
#		description:		container of documents
#		useful operations:	DocumentTermMatrix
#			can be created from Corpus object
#			dtm <- DocumentTermMatrix(corpus)
#
createCorpus <- function(
	documentsPaths
) 
{
	documentsMatrix <- matrix(data = NA, nrow = 0, ncol = 1)

	for(i in 1:documentsPaths$length)
	{
		document <- readLines(documentsPaths$filelist[i])
		document <- paste(document, collapse=" ")
		document <- iconv(document, "latin1", "ASCII", sub="")
		documentsMatrix <- rbind(documentsMatrix, c(document))
	}

	documentsCorp <- Corpus(VectorSource(documentsMatrix[,1]))
	return (documentsCorp)
}


#
#	@description
#		Return already created two corpuses
#		(of Class tm:Corpus) with spam and with ham;
#
#	@return
#		object of class:	List of two objects of class Corpus
#		description:		spam and ham corpuses
#		useful operations:	{corpuses   <- fun__createCorpuses()
#							spamCorpus <- corpuses[1]
#							hamCorpus  <- corpuses[2]}	
#
createCorpuses_standard <- function(
	spam_dirs = c("spam"),
	ham_dirs  = c("easy_ham")
) 
{
	ps_spam = getDocumentsPaths( spam_dirs )
	ps_ham  = getDocumentsPaths( ham_dirs )

	spamCorpus <- createCorpus(ps_spam)
	hamCorpus  <- createCorpus(ps_ham)

	return (list(spamCorpus, hamCorpus))
}
createCorpuses_fromAllData <- function() 
{
	createCorpuses_standard(
		spam_dirs = c("spam", "spam_2"),
		ham_dirs  = c("easy_ham", "easy_ham_2", "hard_ham")
	)
}


#
#	@description
#		Standard preprocessing of documents in corpus
#		lowercasing, removing HTML, etc.;
#		
#	@input
#		@corp
#			Class: Corpus
#			description: corpus in
#
#	@return
#		object of class:	Corpus
#		description:		corpus processed
#
filterCorpus_standard <- function( corp ) {
	corp <- tm_map(corp, removeHTMLTags)
	corp <- tm_map(corp, content_transformer(tolower))
	corp <- tm_map(corp, removeNumbers)
	corp <- tm_map(corp, removePunctuation)
	corp <- tm_map(corp, stripWhitespace)
	corp <- tm_map(corp, removeWords, stopwords("english"))
}



#
#	@description
#		Remove HTML Tags; 
#		According to function >>tm_map<<, so
#		corp <- tm_map(corp, removeHTMLTags) will work
#		
#	@input
#		@x
#			object of class:	PlainTextDocument
#			description:		document before processing
#
#	@return
#		object of class:	PlainTextDocument
#		description:		document after processing
#
removeHTMLTags <- function(x) {
	newcontent = (gsub("<.*?>", "", x$content, perl=TRUE))
	PlainTextDocument(x=newcontent,
					  id = meta(x, "id"),
					  language = meta(x, "language"))
}


#
#	@description
#		All words from corpus in decreasing order;
#		Note: input argument is DocumentTermMatrix not Corpus;
#		
#	@input
#		@dtm
#			Class:	DocumentTermMatrix
#		
#
#	@return
#		object of class:	ClassName
#		description:		DescriptionOfOutput
#		useful operations:	UsefulOperations
#
showWordsFrequencyInCorpusInOrder <- function(dtm) {
	dtm_df = dtm_df <- as.data.frame(
		data.matrix(dtm), 
		stringsAsfactors = FALSE)
	wordsOnly = dtm_df[0,]
	wordsWithFrequency 		= colSums(dtm_df)
	ord_wordsWithFrequency 	= sort(wordsN, decreasing=TRUE)
}
#------------------------------------------------------------------