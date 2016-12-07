#setwd("/home/mnaeem/r.codes/products-shelves-tagging-dataset/")
setwd('/media/mnaeem/17ce45c0-14f1-4c30-8549-6e095ba16867/products-shelves-tagging-dataset/')
train.data <- read.delim('train.tsv', header = TRUE)
test <- read.delim('test.tsv', header = TRUE)

source('preProcess.R')
train.data <- preProcess(train.data)
ids <- data.frame (test$item_id)
colnames(ids) <- c('item_id')
test <- preProcess(test)
remove(preProcess); gc()

library(stringr)

tags <- as.data.frame (str_split_fixed(train.data$tag, ",",10)) # idealy 10 be replaced by 32 (number of classes)

i <- 1
tags[,i] <- gsub('[[]', '', tags[,i])
for (i in 1:ncol(tags)) {
  tags[,i] <- gsub('[[]', '', tags[,i])
  tags[,i] <- gsub('[]]', '', tags[,i])
}

for (i in ncol(tags): 1) {
  un <- unique(tags[,i])
  un <- length(un)
  if (un == 1)
    tags[[i]] <- NULL
  print(paste(i , un, sep = ": ")); flush.console()
}

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
for (i in 1: ncol(tags) ) {
  tags[,i] <- trim(tags[,i])
}

#------------------------------------------------------

tag.list <- data.frame(unique(tags[,1]) )
colnames(tag.list) <- "tag"

for (i in ncol(tags): 2) {
  tg <- data.frame(unique(tags[,i]) )
  colnames(tg) <- "tag"
  tag.list <- rbind(tag.list, tg)
}  

tag.list <- data.frame(unique(tag.list) )
tag.list <- tag.list[!(tag.list[, ncol(tag.list)]==""), ]
tag.list <- data.frame(unique(tag.list) )
colnames(tag.list) <- "tag"

cn <-  c(as.character(tag.list$tag))   
remove(tg); gc()

library(data.table)
tag.list <- data.table(tag.list)
tg <- t(tag.list)
colnames(tg) <- cn 
tg <- data.frame(tg)
row.names(tg) <- NULL

tag.data <- tg 
for (i in 1:14) {
  tag.data <- rbind(tag.data, tag.data)  
}
tag.data <- tag.data [-c((nrow(train.data)+1): nrow(tag.data)), ]
colnames(tag.data) <- cn 

for (i in 1:ncol(tag.data)) {
  tag.data[,i] <- as.numeric(as.character(tag.data[,i] ))
  tag.data[,i] <- 0
  
}

for (i in 1:ncol(tags)) {
  tags[,i] <- as.numeric(tags[,i] )
}


for (i in 1:ncol(tag.data) ) {
  for (j in 1:ncol(tags) ) {
    index <- tags[,j] == tg[,i]
    tag.data[,i][index] <- 1  
  }
}



for (i in 1:ncol(tag.data)) {
  tag.data[,i] <- as.factor(tag.data[,i])
}

train.data$tag <- NULL
ul <- ncol(train.data)
train <- cbind(train.data, tag.data)

remove(train.data); remove(tag.list); remove(trim); remove(i); remove(j); remove(un); gc()
remove(tags); remove(tg); remove(tag.data); remove(index); remove(cn); gc()

#-----------------------------------------------------------------------------------------------
save(train, file = 'train')
save(test, file = 'test')

#-----------------------------------------------------------------------------------------
#setwd("/home/mnaeem/r.codes/products-shelves-tagging-dataset/")
setwd('/media/mnaeem/17ce45c0-14f1-4c30-8549-6e095ba16867/products-shelves-tagging-dataset/')
library(h2o)
load(file = 'train')
load(file = 'test')
load(file = 'ids')

#---------------------------------------------------------------
localH2O <- h2o.init(max_mem_size = '8g', nthreads = -1) 

train.h2o <- as.h2o(train)
partition <- 14; max.class <- ncol(train); indx <- c(1:(partition-1))
remove(train); gc()

test.h2o <- as.h2o(test); remove(test); gc()
source('messagerie.R');source('saveData.R') 
msg <- ""; 
for (ul in partition:max.class) {
#for (ul in 31: 36) {
  result <- ids;
  
#----------------------------------------------------------------------
m <- h2o.naiveBayes(indx, ul, train.h2o, ignore_const_cols = TRUE, laplace = 0, threshold = 0.001, eps = 0, 
                      nfolds = 5, fold_column = NULL, fold_assignment = 'Stratified', seed=1122, 
                      keep_cross_validation_predictions = FALSE,  keep_cross_validation_fold_assignment = FALSE, compute_metrics = TRUE,
                      max_runtime_secs = 0, model_id = 'nb.1')
msg <- messagerie (m, msg); result <- saveData (m, result) 

#----------------------------------------------------------------------
m <- h2o.gbm(indx, ul, train.h2o, distribution = 'bernoulli', ntrees = 1000, col_sample_rate = 0.5,
             seed = 1234, nfolds = 5, balance_classes = TRUE, model_id = 'gbm.1' )
msg <- messagerie (m, msg); result <- saveData (m, result) 
#---------------------------------------------------------------
m <- h2o.gbm(indx, ul, train.h2o, distribution = 'bernoulli', ntrees = 10, learn_rate=0.6, 
             seed = 1234, nfolds = 5, balance_classes = TRUE, model_id = 'gbm.2' )
msg <- messagerie (m, msg); result <- saveData (m, result) 
#---------------------------------------------------------------
m = h2o.randomForest(indx, ul, train.h2o, seed=1, balance_classes = TRUE, nfolds = 5, max_depth = 10, model_id = 'rf.1') 
msg <- messagerie (m, msg); result <- saveData (m, result) 
#----------------------------------------------------------------------
m <- h2o.randomForest(indx, ul, train.h2o, seed = 1234, ntrees = 500, nfolds = 5, balance_classes = TRUE, model_id = 'rf.2' )
msg <- messagerie (m, msg); result <- saveData (m, result) 
#---------------------------------------------------------------
m <-  h2o.deeplearning(indx,ul, train.h2o, distribution = "bernoulli", activation = "Rectifier", nfolds = 5, balance_classes = TRUE,
                       hidden = c(10,10,10),  input_dropout_ratio = 0.2, l1 = 1e-5, epochs = 100, model_id = 'dl.1')
msg <- messagerie (m, msg); result <- saveData (m, result) 
#---------------------------------------------------------------
m <-  h2o.deeplearning(indx,ul, train.h2o, distribution = "bernoulli", activation = "Rectifier", nfolds = 5, balance_classes = TRUE,
                       hidden = c(15,15,15),  input_dropout_ratio = 0.2, l1 = 1e-5, epochs = 50, model_id = 'dl.2' ) 
msg <- messagerie (m, msg); result <- saveData (m, result) 
#----------------------------------------------------------------------
print(paste('Completed Label ID: ', ul, sep = '' ) ); flush.console()
}

h2o.shutdown(prompt = FALSE); gc()
#==========================================================================================
#-----------------------------------------------------------------------------------------


###------- View Performance from messagerie
load(file = 'msg')
msg <-  strsplit(msg, "\n")[[1]]
msg <- data.frame (msg)

library(stringr)
msg <- as.data.frame (str_split_fixed(msg$msg, ":",6))
msg[,5] <- NULL
msg[,3] <- NULL

algo <- msg$V2; msg$V2 <- NULL
indx <- sapply(msg, is.factor)
msg[indx] <- lapply(msg[indx], function(x) as.numeric (as.character(x)) )
msg$algo <- algo; remove(algo)

load(file = 'train')
cn <- colnames(train)
remove(train); gc()

msg$Label <- paste('[',cn[msg$V1], ']', sep = '') 
msg <- msg[,c(5,4,2,3)]
colnames(msg) <- c('Label', 'Algo', 'Avg.Class.Error', 'Logloss' )

head(msg)

#---------------------------------------------------------------------------
#---- Plotting -------------------------------------------------------------
library(ggplot2)
ggplot(data = msg) + 
  geom_point(mapping = aes(x = Logloss, y = Avg.Class.Error, color = Algo) , cex=2 )


ggplot(data = msg, mapping = aes(x = Logloss, y = Avg.Class.Error)) + 
  geom_point(mapping = aes(color = Algo), cex=2) + 
  geom_smooth()

ggplot(data = msg, mapping = aes(x = Logloss, y = Avg.Class.Error)) + 
  geom_point(mapping = aes(color = Label), cex=2) + 
  geom_smooth()


mycolors = c('red','yellow','blue','green','orange','violet', 'grey', 'cyan', 'black' )

plot(factor(msg$Algo), msg$Logloss, xlab="Algo", ylab='Logloss', col = mycolors)
plot(factor(msg$Algo), msg$Avg.Class.Error, xlab="Algo", 
     ylab='Average Class Error', col = mycolors)
plot(factor(msg$Label), msg$Avg.Class.Error, xlab="", las =2 , 
     ylab='Average Class Error', col = mycolors)
plot(factor(msg$Label), msg$Avg.Class.Error, xlab="", 
     ylab='Average Class Error', las=2, col = mycolors )

remove(mycolors); remove(indx); remove(msg); gc()

#==========================================================================

load(file = 'train')
cn <- colnames(train)
remove(train); gc()

i <- 14
load(file = paste('result', i, sep = '.') )
submission= data.frame(item_id = result$item_id) 

label.count <- 17 
for (i in 14:label.count) {
  
  load(file = paste('result', i, sep = '.') )
  
  indx <- sapply(result, is.factor)
  result[indx] <- lapply(result[indx], function(x) as.numeric(as.character(x)))
  result <- data.frame(tag=rowSums(result[,2: 8]))
  
  result[1][result[1] < 4] <- 0 
  result[1][result[1] > 3] <- as.numeric(cn[i]) 
  
  result$tag <- as.character(result$tag)
  result[1][result[1] == '0'] <- '' 
  
  submission <- cbind(submission, result[1])
}

#-------------------------------------------------------
# create a new column `added_products` with multiple columns collapsed together
cn <- c('item_id', cn[14:label.count])
colnames(submission) <- cn
submission$dernier <- apply( submission[ , cn[2:5] ] , 1 , paste , collapse = ", " )
submission <- data.frame(item_id=submission$item_id, tag=paste('[', submission$dernier, ']', sep = ''))

for (i in 1:32)
  submission <- lapply(submission, function(x) {gsub(", ,", ",", x) })

submission <- lapply(submission, function(x) {gsub("\\[, ", "[", x) })
submission <- lapply(submission, function(x) {gsub(", \\]", "]", x) })
submission <- lapply(submission, function(x) {gsub("\\[\\]", "", x) })
submission <- data.frame(submission)

#------------------------------------------------------------------------
cat("saving the submission file\n")
library("readr")
write_csv(submission, "sample_submission.csv")

head(submission, 10)
#================================ END =============================================#