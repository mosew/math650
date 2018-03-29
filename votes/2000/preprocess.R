setwd("C:/Users/mose/Dropbox/math650/votes/2000")

library(plyr)
library(dplyr)
library(magrittr)
library(scales)



# numeric data in the spreadsheet are read in as character data; this function will fix that
tonumeric=function(y){
  c2 = suppressWarnings(data.frame(data.matrix(y)))
  numeric_columns = sapply(c2,function(x){mean(as.numeric(is.na(x)))<0.5})
  return(cbind(y[,!numeric_columns],c2[,numeric_columns]))
}

# Load census data.
# Limitation: based on sample data
full=read.csv("raw/csv/full.csv",header=T,stringsAsFactors = F,strip.white=T)[-c(1,2),] %>%
  
  # drop rows with empty county names, which give statewide or USA totals
  filter(countystate!=toupper(countystate)) %>%
  
  # get state and county from single countystate field
  mutate(state=regmatches(countystate,regexpr("([A-Z][A-Z]$)",countystate)),
         county=gsub("(, [A-Z][A-Z]$)","",countystate),
         countystate=NULL) %>%
  
  filter(state!="PR" & state!="AK") %>%
  mutate(county = tolower(gsub("( Parish)|( city)|( City)|( County)|(')| ","",county)))

latlong=read.csv("raw/csv/latlong.csv",header=T,stringsAsFactors = F,strip.white=T) %>%
  unique()%>%
  #get state and county
  filter(state!="PR" & state!="AK") %>%
  mutate(county = tolower(gsub("( Parish)|( city)|( City)|( County)|(')| ","",county)))

t=merge(tonumeric(full),tonumeric(latlong),by=c("state","county"),all=T)
t$fips = paste(sprintf("%02d",t$Statecode),sprintf("%03d",t$COUNTYCODE),sep="")


# For some reason Virginia is missing per capita income data, so I set each county's equal to the statewide avg
t$pci[t$state=="VA"]=23975

# Manually delete some (small) counties missing information

# Kalawao, HI
# Broomfield, CO
# Bullock, AL
# Oglala Lakota County, SD
# Loving, TX

#t$county[unique(which(is.na(t),arr.ind=T)[,1])]
X=t[-unique(which(is.na(t),arr.ind=T)[,1]),]
X=X[-which(X$medianhvalue==0,arr.ind=T),]
sum(is.na(X))
# > 0

# Get target
y = data.frame(rpct=X$rpct)

# Drop non-numeric and target columns
X = X[,-c(1,2,3,4,5,18,60,61,62,65,67)]

X$pop=X$totpop.x
X$other1 = X$other1+X$pacisl1+X$native1
X$nevermarried = (X$fnevermarried+X$mnevermarried)/X$pop*100
X$married = (X$fmarriednonsep+X$mmarriednonsep)/X$pop*100
X$sepdiv = (X$fseparated+X$fdivorced+X$mdivorced+X$mseparated)/X$pop*100
X$lnpop = as.numeric(log(X$pop))
X$lnpoppersqmi = as.numeric(log(X$poppersqmi))
X$age20_34 = (X$age20_24+X$age25_34)/X$pop*100
X$age35_54 = (X$age35_44+X$age45_54)/X$pop*100
X$age55_ov = (X$age55_59+X$age60_64+X$age65ov)/X$pop*100
X$vcper1000 = X$vc/X$pop*1000
X$homeaffordability = X$pci/X$medianhvalue*100
X$homeless = (X$totpop.x-X$inhomes)/X$pop*100
X$nohealthinsurance = X$nohealthinsurance/X$pop*100
X$urban = X$urbanpop/X$pop*100
X$farm = X$farmpop/X$pop*100
X$voterparticip = X$totvotes/X$pop*100
X$profoccs = X$profoccs/X$pop*100
X$farmoccs = X$farmoccs/X$pop*100
X$constructionoccs = X$constructionoccs/X$pop*100
X$salesoccs = X$salesoccs/X$pop*100
X$productionoccs = X$productionoccs/X$pop*100
X$serviceoccs = X$serviceoccs/X$pop*100
X$incollege = X$incollege/X$pop*100
X$houseperpop = X$tothous/X$pop
X$long[X$long>0]=X$long[X$long>0]-180

fips = X$fips
X=subset(X,select=-c(pacisl1,native1,fnevermarried,mnevermarried,fmarriednonsep,mmarriednonsep,mwidowed,fwidowed,
                     fseparated,fdivorced,mseparated,mdivorced,poppersqmi,totpop.x,pop,totvotes,tothous,
                     age20_24,age25_34,age35_44,age45_54,age55_59,age60_64,age65ov,vc,inhomes,landsqm,
                     urbanpop,farmpop,fips))



# Look for heavy tails (and heads) and deal with them

# First identify non-normal distributions by the Shapiro-Wilk test
shapiro.score=X%>%
  apply(2,shapiro.test)%>%
  sapply(function(x) x[[1]][[1]])

abnormvars=names(X)[shapiro.score<.92]

X$lnincollege = as.numeric(log(X$incollege))
X$lnba = as.numeric(log(X$ba))
X$lnpctmultiunitdwelling = as.numeric(log(X$pctmultiunitdwelling))
X$lnpctmultiunitdwelling[X$pctmultiunitdwelling==0]=min(X$lnpctmultiunitdwelling[X$lnpctmultiunitdwelling>-Inf])
X$lnmedianhvalue = as.numeric(log(X$medianhvalue))
X$lnmediangrossrent = as.numeric(log(X$mediangrossrent))
X$medianhhincome[X$medianhhincome==0]=min(X$medianhhincome[X$medianhhincome>0])
X$lnmedianhhincome = as.numeric(log(X$medianhhincome))
X$lnunemployed = as.numeric(log(X$unemployed+.1))
X$lnpci = as.numeric(log(X$pci))
X$lnforeignborn = as.numeric(log(X$foreignborn+.1))
X$lnnonenglish = as.numeric(log(X$esl+.1))
X$lnlandsqmi = as.numeric(log(X$landsqmi))
#X$lnvcper1000 = as.numeric(log(X$vcper1000))
X$lnhomeless = as.numeric(log(X$homeless+.1))
#X$lnhouseperpop = as.numeric(log(X$houseperpop))

X = subset(X,select=-c(ba,medianhvalue,mediangrossrent,landsqmi,incollege,pctmultiunitdwelling,medianhhincome,unemployed,pci,foreignborn,esl,homeless))

y.2 = factor(y>50,levels=c(TRUE,FALSE),labels=c("Bush","Gore"))
X.2 = X
X.2$rpct = y.2
X = cbind.data.frame(y,X)
