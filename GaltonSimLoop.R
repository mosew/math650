n=1078
m=1000
r=cor(z$V1,z$V2)
slopes=NULL
for(i in 1:m){
 u=rnorm(n,0,1)
 v=rnorm(n,0,1)
 w=rnorm(n,0,1)
 father.height=sqrt(r)*u+sqrt(1-r)*v
 son.height=sqrt(r)*u+sqrt(1-r)*w
 father.height=sd(z$V1)*father.height+mean(z$V1)
 son.height=sd(z$V2)*son.height+mean(z$V2)
 galton.lm=lm(son.height ~ father.height)
 slopes=c(slopes,summary(galton.lm)$coefficients[2])
}

hist(slopes)



