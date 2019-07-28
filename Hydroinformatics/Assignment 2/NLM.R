data=c(3.195,6.194,6.778,0.5177,14.743,5.849,18.954,0.795,0.254,4.311)
ex=function(th,x){
  return(-sum(log(th)-th*x))
}
out=nlm(ex,median(data),x=data)
