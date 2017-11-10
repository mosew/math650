library(ISLR)
library(caret)


#5
d=Default
d$default=as.numeric(d$default)-1
set.seed(539)
d.glmfit=glm(default~balance+income,data=d)

for(i in 1:3){
  d.ival=createDataPartition(1:dim(d)[1],p=.2,list=F)
  d.val=d[d.ival,]
  d.train=d[-d.ival,]
  
  # without student
  d.split.glmfit = train(as.factor(default)~balance+income, data=d.train, method="glm")
  preds=predict(d.split.glmfit, newdata=d.val)
  valerrorrate=sum(preds==d.val$default)/length(preds)
  #print(paste(valerrorrate))
  
  #with student
  d.split.glmfit2 = train(as.factor(default)~balance+income+student, data=d.train, method="glm")
  preds2=predict(d.split.glmfit2, newdata=d.val)
  valerrorrate2=sum(preds2==d.val$default)/length(preds)
  print(paste(valerrorrate2))
}

#6
set.seed(323)
#a)
summary(d.glmfit)

#b)
boot.fn = function(d, obs){
  return(glm(as.numeric(d$default)[obs]~d$balance[obs]+d$income[obs])$coefficients[c(2,3)])
}
#boot.fn(Default,1:8000)

#c)
library(boot)
b=boot(Default,boot.fn,R=500)
paste(c(sd(b$t[,1]),sd(b$t[,2])))
# [1] "6.33556278035414e-06" "1.35431026413757e-07"

#d)
# The standard errors are really small. The first is much smaller relative to the coefficient though.

#7
w=Weekly
w$Direction=as.numeric(w$Direction)-1
#a)
a=glm(Direction~Lag1+Lag2,data=w)
#b)
b=glm(Direction~Lag1+Lag2,data=w[-1,])
#c)
round(predict(b,newdata=w[1,]))==w[1,]$Direction
# FALSE
#d)
errors=rep(NA,dim(w)[1])
for(i in 1:dim(w)[1]){
  d=glm(Direction~Lag1+Lag2,data=w[-i,])
  pred=predict(d,newdata=w[i,])
  errors[i]= round(pred)!=w[i,]$Direction
}
#e)
LOOCV=mean(errors)
paste(LOOCV)
# Doesn't do a great job. Makes errors 44.8% of the time.

#8
#a)
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
#n=100, p=2 (x and x^2)

#b)
ggplot()+geom_point(aes(x=x,y=y))
# Looks like I expected, just approximating a downward facing parabola

#c)
data=data.frame(y,x,x^2,x^3,x^4)
set.seed(1)
errors=errors2=data.frame(m1=double(),
                  m2=double(),
                  m3=double(),
                  m4=double())
m1=lm(y~x,data=data[-i,])
m2=lm(y~x+x.2,data=data[-i,])
m3=lm(y~x+x.2+x.3,data=data[-i,])
m4=lm(y~x+x.2+x.3+x.4,data=data[-i,])


for(i in 1:length(x)){
  errors[i,1]=predict(m1,newdata=data[i,])-y[i]
  errors[i,2]=predict(m2,newdata=data[i,])-y[i]
  errors[i,3]=predict(m3,newdata=data[i,])-y[i]
  errors[i,4]=predict(m4,newdata=data[i,])-y[i]
}

#d)
set.seed(2)
for(i in 1:length(x)){
  errors2[i,1]=predict(m1,newdata=data[i,])-y[i]
  errors2[i,2]=predict(m2,newdata=data[i,])-y[i]
  errors2[i,3]=predict(m3,newdata=data[i,])-y[i]
  errors2[i,4]=predict(m4,newdata=data[i,])-y[i]
}
errors==errors2
#Yes, they are the same, because LOOCV is a deterministic function of the data.

#e)
names(errors)[abs(colSums(errors))==min(abs(colSums(errors)))]
# m4 with all the variables is best. This was not to be expected because the model is noisy second-order, not fourth order.

#f)
summary(m1)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.8278     0.2386  -7.661  1.4e-11 ***
#   x             0.2466     0.2491   0.990    0.325    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(m2)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.09149    0.13495  -0.678    0.499    
# x            0.89907    0.11356   7.917 4.26e-12 ***
#   x.2         -1.86802    0.09210 -20.282  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(m3)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.09481    0.13614  -0.696    0.488    
# x            0.95180    0.22314   4.266 4.71e-05 ***
#   x.2         -1.85509    0.10382 -17.868  < 2e-16 ***
#   x.3         -0.02337    0.08498  -0.275    0.784    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(m4)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.134950   0.161471  -0.836 0.405414    
# x            0.906541   0.244111   3.714 0.000346 ***
#   x.2         -1.730936   0.285489  -6.063 2.76e-08 ***
#   x.3          0.008293   0.108976   0.076 0.939498    
# x.4         -0.037790   0.080899  -0.467 0.641491    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# The model m2 appears best from this perspective since in all models 2 through 4
# the non-intercept x and x^2 coefficients are the only significant ones; the linear model has
# insignificant linear coefficient.