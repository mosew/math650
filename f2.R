f2<-function(x,n){return(1-4*x/n+4*x*(x-1)/(n*(n-1)))}
f23<-function(x){return(f2(x,3))}
# plot(f23,from=1, to=50, xlab="x", ylab="y")