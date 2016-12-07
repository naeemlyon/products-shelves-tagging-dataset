#setwd("/home/mnaeem/r.codes/products-shelves-tagging-dataset/")
setwd('/media/mnaeem/17ce45c0-14f1-4c30-8549-6e095ba16867/products-shelves-tagging-dataset/')
library(data.table)
library(h2o)
train.data <- read.delim('train.tsv', header = TRUE)
#test.data <- read.delim('test.tsv', header = TRUE)

train.data$item_id <- NULL
train.data$Product.Long.Description <- NULL
train.data$Product.Short.Description
train.data$Product.Name <- NULL
train.data$Synopsis <- NULL


library(stringr)

tags <- str_split_fixed(train.data$tag, ",",10)
tags <- as.data.frame(tags)

for (i in ncol(tags): 1) {
  un <- unique(tags[,i])
  un <- length(un)
  if (un == 1)
    tags[[i]] <- NULL
  print(paste(i , un, sep = ": ")); flush.console()
}

i <- 1
tags[,i] <- gsub('[[]', '', tags[,i])
for (i in 1:ncol(tags)) {
  tags[,i] <- gsub('[[]', '', tags[,i])
  tags[,i] <- gsub('[]]', '', tags[,i])
}



#-----------------------------------------------------------------------------------------------
train.data$tag <- NULL

i <- 1
train <- cbind(train.data, tag = tags[,i])
cn <- colnames(train)

for (i in 2:ncol(tags)) {
  print(paste('processing ', i, sep = ':')); flush.console()
  temp <- cbind(train.data, tags[,i])
  temp <- temp[!(temp[, ncol(temp)]==""), ]
  colnames(temp) <- cn
  train <- rbind(train, temp)
}

remove(temp); remove(tags); remove(train.data); gc()


# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
train$tag <- trim(train$tag)

ul <- ncol(train)
train$tag <- paste('[',train$tag,']', sep = '') 

train$tag <- as.factor(train$tag)



#---------------------------------------------------------------


#---------------------------------------------------------------
localH2O <- h2o.init(max_mem_size = '8g', nthreads = -1) 

train.h2o <- as.h2o(train)
ul <- ncol(train); indx <- c(1:(ul-1))
remove(train); gc()

#test.h2o <- as.h2o(test.data); remove(test.data); gc()

#----------------------------------------------------------------------
m = h2o.randomForest(indx, ul, train.h2o, model_id ="rf.ci", seed=1, balance_classes = TRUE) # validation_frame= eval.h2o, 
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)

#----------------------------------------------------------------------
m <- h2o.gbm(indx, ul, training_frame = train.h2o, balance_classes = TRUE, nfolds = 5,  distribution = 'multinomial') 
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)
#----------------------------------------------------------------------

#---------------------------------------------------------------
m <- h2o.glm(indx, ul, train.h2o, family = 'multinomial', seed = 1122, lambda = 0.9, alpha = 0.9, nfolds = 5) # validation_frame= eval.h2o)
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)

#---------------------------------------------------------------
m <- h2o.gbm(indx, ul, train.h2o, distribution = 'multinomial', ntrees = 50,  learn_rate=0.02,  seed = 1234, nfolds = 5 )# , validation_frame= eval.h2o)

#---------------------------------------------------------------
m <- h2o.gbm(indx, ul, train.h2o, distribution = 'multinomial', ntrees = 50, nfolds = 5, seed = 1234, col_sample_rate = 1.0,
             learn_rate = 0.1 )
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)
#---------------------------------------------------------------

m <- h2o.gbm(indx, ul, train.h2o, distribution = 'multinomial', ntrees = 1000, col_sample_rate = 0.5,
             seed = 1234, nfolds = 5 )# , validation_frame= eval.h2o)
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)
#---------------------------------------------------------------
m <- h2o.gbm(indx, ul, train.h2o, distribution = 'multinomial', ntrees = 50, learn_rate=0.3,  seed = 1234, nfolds = 5 )# , validation_frame= eval.h2o)
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)

#---------------------------------------------------------------
m <- h2o.randomForest(indx, ul, train.h2o, seed = 1234, ntrees = 500, nfolds = 5 )
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)

#---------------------------------------------------------------
m <-  h2o.deeplearning(indx,ul, train.h2o, distribution = "multinomial", activation = "Rectifier", nfolds = 5,
                       hidden = c(10,10,10),  input_dropout_ratio = 0.2, l1 = 1e-5, epochs = 100) #  variable_importances=T
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)

#---------------------------------------------------------------
m <-  h2o.deeplearning(indx,ul, train.h2o, distribution = "huber", activation = "Rectifier", nfolds = 5,
                       hidden = c(15,15,15),  input_dropout_ratio = 0.2, l1 = 1e-5, epochs = 50) #  variable_importances=T
perf <- h2o.performance(m)@metrics$mean_per_class_error; print(perf)

#----------------------------------------------------------------------
h2o.shutdown(prompt = FALSE); gc()


#-----------------------------------------------------------------------------------------------

