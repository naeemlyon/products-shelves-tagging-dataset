saveData <- function(m, result) {
  #---- obtain results
  pred <- as.data.frame(h2o.predict(m, newdata=test.h2o))
  cn <- colnames(result)
  result <- cbind(result,  pred$predict)
  cn <- c(cn, m@model_id)
  colnames(result) <- cn
  save(result, file= paste('result', ul, sep = '.') )
  remove(cn); remove(pred); gc()
  return (result)
}
