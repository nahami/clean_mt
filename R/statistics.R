mse <- function(testY,testX,model){
  mean((testY-predict(model,testX))^2)
}

rmse <- function(testY,testX,model){
  sqrt(mean((testY-predict(model,testX))^2))
}

rsquared <- function(testY,testX,model){
  residuals <- testY - predict(model,testX)
  teller<-sum(residuals^2)
  noemer<-sum((testY-mean(testY))^2)
  r2 <- 1-teller/noemer
  return("R2" = r2)
}
