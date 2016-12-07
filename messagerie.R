messagerie <- function(m, msg) {
  
  perf <- h2o.performance(m, xval = TRUE)@metrics
  err <- perf$mean_per_class_error
  logloss <- perf$logloss
  
  msg <- paste(msg, ul, ":" , 
               m@model_id , 
               ":err:", round(err,2), 
               ":logloss:" , round(logloss, 2), 
               "\n", sep = "")
  save(msg, file='msg') 
  
  return (msg)
}
