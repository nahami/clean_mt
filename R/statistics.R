
MSE<-function(testY,testX,model){

mean((testY-predict(model,testX))^2)
}

