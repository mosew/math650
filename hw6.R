################ HW 6 ##################

# 6c

# I'm gonna take the transpose here. Best practice is to have observations (tissue samples) as rows and predictors (genes) as columns

set.seed(32)
meta = data.frame(treatment = runif(100)>.5,
                   machine = runif(100)>.4)

X=matrix(0,100,1000)
X=apply(X,2,function(x)runif(100))

# Suppose there is a treatment effect on genes (whose index is) divisible by 46, that both machines are 30% likely to make mistakes
# in identification of genes divisible by 59, and machine B is 50% likely to make an error in identifying genes divisible by 79.

# I don't have time to do this part of this exercise...preparing for my oral exam! Also unclear if the tissue sample will be a binary
# string or the "level of expression" is a continuous variable.



# 7
u=USArrests

su=apply(u,2,function(x)scale(x))

( 1-cor(su[5,],su[1,]) ) / sum((u[5,]-u[1,])^2)

( 1-cor(su[2,],su[24,]) ) / sum((u[2,]-u[24,])^2 )

for(i in 1:dim(u)[1]){
  for(j in 1:i){
    print(paste((1-cor(su[i,],su[j,])) / sum((u[i,]-u[j,])^2) ))
  }
}

# 8a
pcau=prcomp(u,scale=T,center=T)
deva = pcau$sdev^2 / sum(pcau$sdev^2)
#b
devb = (apply( (as.matrix(scale(u)) %*% pcau$rotation)^2 / sum(scale(u)^2) , 2, function(x) sum(x)))

abs(deva-devb)<10^-10
# PC1  PC2  PC3  PC4 
# TRUE TRUE TRUE TRUE 

# 9a
hc.out=hclust(dist(u))
#b
cutree(hc.out,3)
#c
u.sc = apply(u,2,function(x)scale(x))
hc.sc.out=hclust(dist(u.sc))
plot(hc.sc.out)
#d
#Appears to balance the clustering and give each observation a fair amount of space between it and others,
#Seems like a good idea most of the time that clustering is appropriate, to distinguish between observations more clearly;
#otherwise the scale of the variable is ignored, so, for example, observations differing by thousands in one variable scaled in
#tens of thousands will be very far apart, though they may, perhaps importantly, differ by units or even agree exactly in another variable.