x<-c(1,2,3,8)
y<-c(9,3,4,1)
length(x)
length(y)
#x+y
#ls()
#rm(x,y)
#rm(list=ls())
#x<-matrix(data=c(1,2,3,4),nrow=2,ncol=2)
#x<-matrix(c(1,2,4,5),2,2)
#x<-matrix(c(1,2,4,5),2,2,byrow=TRUE)
#sqrt(x)
#x^2
#x=rnorm(5)
#y<-matrix(rnorm(10),2,5)
#set.seed(4)
#x<-rnorm(100)
#y<-rnorm(100)
#pdf("Figure.pdf")
#plot(x,y,col="green")
#dev.off()
#seq(1:10)
#seq(0,1,length=10)
##x=seq(-pi,pi,length=50)
#y=x
#f=outer(x,y,function(x,y)cos(y)/(1+x^2))
#contour(x,y,f)
#contour(x,y,f,nlevels=45, add=T)
#fa=(f-t(f))/2
#contour(x,y,fa,nlevels=15)
#image(x,y,fa)
#persp(x,y,fa)
#persp(x,y,fa,theta=30)
#persp(x,y,fa,theta=30,phi=20)

#A=matrix(1:16,4,4)
#A[2,3]
#A[c(1,3),c(2,4)]
#A[1:3,2:4]
#A[1,]
#A[,1]
#A[,1:2]
#A[-c(1,3),]

#Auto=read.csv("Auto.csv",header=T,na.strings="?")
#fix(Auto)  # see row 33
#dim(Auto)
#Auto=na.omit(Auto)
#dim(Auto)
#names(Auto)
#plot(Auto$cylinders,Auto$mpg)
#Auto$cylinders=as.factor(Auto$cylinders)
#plot(Auto$cylinders,Auto$mpg)
#plot(Auto$cylinders,Auto$mpg,col="red",varwidth=T,xlab="cylinders") # varwidth  ~ root sample size
#hist(Auto$mpg)
#pairs(Auto)
#pairs(~Auto$mpg + Auto$displacement + Auto$weight)
#plot(Auto$horsepower,Auto$mpg)
#summary(Auto)

