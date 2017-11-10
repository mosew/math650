n=1078
r=cor(z$V1,z$V2)
u=rnorm(n,0,1)
v=rnorm(n,0,1)
w=rnorm(n,0,1)
father.height=sqrt(r)*u+sqrt(1-r)*v
son.height=sqrt(r)*u+sqrt(1-r)*w
father.height=sd(z$V1)*father.height+mean(z$V1)
son.height=sd(z$V2)*son.height+mean(z$V2)
galtonsim.lm<-lm(son.height ~ father.height)
summary(galtonsim.lm)


# mean(father.height)
# mean(z$V1)
# cor(father.height,son.height)
# plot(father.height,son.height)
