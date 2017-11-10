#######################
# Project for Math 650:
#
# Exploring County Demographics Most Predictive of Vote in 2000 Presidential Election
#
# Mose Wintner

# This file loads and prepares all the data



# Make sure this folder contains the csv files full1 and full2
setwd("C:/Users/mose/Dropbox/HW/math650/votes/2000")
library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)
require(caret)



#######################
# IMPORT AND CLEAN DATA
#
#
# Load voting data.
#
# Manually deleted several cities in Virginia listed as counties

# numeric data in the spreadsheet are read in as character data; this function will fix that
tonumeric=function(y){
  c2 = suppressWarnings(data.frame(data.matrix(y)))
  numeric_columns = sapply(c2,function(x){mean(as.numeric(is.na(x)))<0.5})
  return(cbind(y[,!numeric_columns],c2[,numeric_columns]))
}



# Load census data.
# Limitation: based on sample data
c=read.csv("full1.csv",header=T,stringsAsFactors = F,strip.white=T)[-c(1,2),] %>%
  
  # drop rows with empty county names, which give statewide or USA totals
  filter(countystate!=toupper(countystate)) %>%
  
  # get state and county from single countystate field
  mutate(state=regmatches(countystate,regexpr("([A-Z][A-Z]$)",countystate)),
         county=gsub("( [A-Z][A-Z]$)","",countystate),
         countystate=NULL) %>%
  
  filter(state!="PR" & state!="AK") %>%
  mutate(county = tolower(gsub("( Parish)|( city)|( City)|( County)|(')| ","",county)))

# Load various other county data, including votes

d=read.csv("full2_nova.csv",header=T,stringsAsFactors = F,strip.white=T)[-c(1,2),] %>%
  
  # drop rows with capital county names, which give statewide or USA totals
  filter(countystate!=toupper(countystate)) %>%

  #get state and county
  mutate(state=regmatches(countystate,regexpr("([A-Z][A-Z]$)",countystate)),
         county=tolower(gsub("(\\, [A-Z][A-Z]$)","",countystate)))%>%
  filter(state!="PR" & state!="AK") %>%
  mutate(county = tolower(gsub("( Parish)|( city)|( City)|( County)|(')| ","",county)),
                countystate=NULL)

## Load state code lookup table
#st=read.csv("state_table.csv",header=T,stringsAsFactors = F,strip.white=T)[,-3] #don't need census region for now

# merging
t=merge(tonumeric(d),tonumeric(c),by=c("state","county"),all=T)

latlong=read.csv("county2k.csv",header=T,stringsAsFactors=F,strip.white=T) %>%
  unique()%>%
  filter(state!="PR" & state!="AK")%>%
  mutate(county = tolower(gsub("( Parish)|( city)|( City)|( County)|(')| ","",county)))
         
t=merge(t,latlong,by=c("state","county"),all=T)

# cleanup
rm(c,d,latlong)


# To make the data easier to manipulate, we strip the "Pct" from variable names
# and make them lowercase
names(t)=sapply(names(t),function(x) tolower(gsub("(Pct)","",x)))


# Drop entries with missing data/NAs:
# Kalawao, HI or whatever it is, since it has no housing data and a tiny population anyway
# Loving, TX, which has no housing data and a tiny population
# In addition, the following counties are missing data about origins of their foreign-born populations:
# Glascock, GA
# Hooker, NE
# Mcpherson, NE
# Wheeler, NE
# Jones, SD
t=t[-which(is.na(t),arr.ind=T)[,1],]





#######################
# Now we deal with our large matrix t
#
# Notes on variables:
#
# URBAN/RURAL (PctInUAs, PctInUCs, PctRural)
#   For Census 2000, a UA [Urban Area] (resp. UC [Urban Cluster]) consists of contiguous,
#   densely settled census block groups (BGs) and census blocks that meet minimum population density requirements,
#   along with adjacent densely settled census blocks that together encompass a population of at least 50,000 people
#   (resp.2,500 people, but fewer than 50,000 people).
#
# We are interested in what characteristics of a county's residents might best explain their aggregated voting patterns
# Since in 2000, by far the most popular third party candidate was Ralph Nader, who leans left, we've restricted
# our attention to what characteristics of a county's residents might best explain the percentage of its votes going to 
# George W. Bush, in column "rpct". This is our variable of interest.

# First we make some new variables
t$lnpop=as.numeric(log(t$totpop100))
t$lnland=as.numeric(log(t$landsqmi))
t$lnpoppersqm=as.numeric(log(t$poppersqm))

t$gwagegap=t$avgmaleearnings-t$avgfemearnings
t$sepdiv = t$separated + t$divorced
t$urban=t$inuas+t$inucs
t$homeless=100-t$hhpop
t$longitude[t$longitude>0]=t$longitude[t$longitude>0]-180
t$poor=t$verypoor+t$poor+t$under185poor
t$englishlimited = t$englishlimited - t$englishonly #variable is for "english spoken less than 'very well'"
t$housingincomediscrep = t$pci/t$medianhvalue
t$over55=t$age55_59+t$age60_64+t$age65_74+t$age75_84+t$over85
t$age18to35=t$age18_19+t$age20_24+t$age25_34
t$european=t$foreignborn/100*t$fbeurope
t$asian=t$foreignborn/100*t$fbasia
t$latinamerican=t$foreignborn/100*(t$fblatinamerica)
#t$mexican=t$foreignborn/100*t$fbmexico/100
t$bornother=t$foreignborn/100*t$fbother
t$banksper1000pop=(t$cbanks+t$sbanks)/t$totpop100*1000
t$vcper1000=t$vc*1000/t$totpop100
t$grad=t$masters+t$profphd

#ndistancefromcentroid=t$latitude
#ndistancefromcentroid = ( ( (t$latitude[t$state!="HI"]-39.5)/max(t$latitude[t$state!="HI"]))^2 + 
#                           ((t$longitude[t$state!="HI"]+98.35)/max(t$longitude[t$state!="HI"]))^2)^.5
#ndistancefromcentroid[t$state=="HI"]=rep(max(ndistancefromcentroid),sum(t$state=="HI"))
#t$distancefromcentroid=ndistancefromcentroid

t$voterparticip=as.numeric(t$totalvotes/t$totpop100*100)
t$emplgov=t$emplgov/t$totpop100*100
t$emplfedgov=t$emplfedgov/t$totpop100*100
t$emplmilitary=t$emplmilitary/t$totpop100*100
t$emplstatelocalgov=t$emplstatelocalgov/t$totpop100*100
t$agricultureindus=t$agricultureindus/t$totpop100*100
t$constructionindus=t$constructionindus/t$totpop100*100
t$manufacturingindus=t$manufacturingindus/t$totpop100*100
t$wholesaleindus=t$wholesaleindus/t$totpop100*100
t$retailindus=t$retailindus/t$totpop100*100
t$transutilitiesindus=t$transutilitiesindus/t$totpop100*100
t$infoindus=t$infoindus/t$totpop100*100
t$financeindus=t$financeindus/t$totpop100*100
t$profindus=t$profindus/t$totpop100*100
t$eduhealthssindus=t$eduhealthssindus/t$totpop100*100
t$publicindus=t$publicindus/t$totpop100*100
t$artsaccomodationindus=t$artsaccomodationindus/t$totpop100*100
t$hospinsured=t$hospinsured/t$totpop100*100
t$nohealthinsurance=t$nohealthinsurance/t$totpop100*100

t$logrvotes = log(t$rvotes+1)

votes_pop_corr=paste(cor(t$rvotes,t$lnpop))



#Omit transformed and irrelevant variables
t=subset(t,select=-c(white1,englishlimited,poorinfam,verypoor,under185poor,vc,nearlypoor,
                      avghhinc,noncitizen,avggrossrent,avghvalue,medianhhinc,
                      totalvotes,landsqmi,poppersqm,
                      hhpop,avghhinclt200,ageunit50,builtbefore1940,
                      divorced,separated,
                      masters,profphd,
                     avgmaleearnings,avgfemearnings,
                     medianyearin,water4thermoelec,
                     emplfedgov,emplmilitary,
                     
                     
                     agricultureindus,
                     constructionindus,
                     manufacturingindus,
                     wholesaleindus,
                     retailindus,
                     transutilitiesindus,
                     infoindus,
                     financeindus,
                     profindus,
                     eduhealthssindus,
                     artsaccomodationindus,
                     
                     
                     
                     
                     
                     
                      #manprofoccs,serviceoccs,salesoffoccs,farmfishoccs,consoccs,transoccs,
                      otherindustry,bornother,othernonpublicindus,
                     
                      manufacturing,retailtrade,education,healthsa,armedforces, 
                      cbanks,sbanks,
                      inuas,inucs,
                      age18_19,age20_24,age25_34,
                      age45_54,age55_59,age60_64,age65_74,age75_84,over85,
                      fbeurope,fbasia,fblatinamerica,fbmexico,fbother,
                      totpop100
                      ))


# Pull numeric variables
n=subset(t,select=-c(county,state,rvotes,logrvotes))
m=subset(t,select=-c(county,state,rpct,logrvotes))


























###########################################
# EXPLORING THE DATA
#


## Any highly correlated variables? This informed some of the variable omissions above.
# hicor=which(abs(cor(n))>.93 & cor(n)<1,arr.ind=T)
# hicors=cor[hicor]
# hicorvars=data.frame(names(n)[hicor[,1]],
#                      names(n)[hicor[,2]])

# summary(n)
# 
# 
# n.tiny=n[n$lnpop<8.6,]
# mean(n.tiny$rpct) #67.84
# n.big=n[n$lnpop>13,]
# mean(n.big$rpct) #42
# 
# # MOST/LEAST POLITICALLY POLARIZED COUNTIES
# qplot(n$rpct,binwidth=1)
# 
# n.mostbush=n[with(n,order(-rpct)),][1:50,]
# n.leastbush=n[with(n,order(rpct)),][1:50,]
# 
# n.tossup = n[n$rpct<52.5 & n$rpct>47.5,]
# 
# #Generate descriptive statistics
# cor=cor(n)
# cov=cov(n)
# va=diag(cov)
# sd=sapply(va,function(x)sqrt(x))





# Look for heavy tails (and heads) and deal with them

# First identify non-normal distributions by the Shapiro-Wilk test
shapiro.score=n%>%
  apply(2,shapiro.test)%>%
  sapply(function(x) x[[1]][[1]])

abnormal=shapiro.score<.92
abnormvars=names(n)[abnormal]
abnormal[which(abnormvars=="rpct")]=FALSE #this one is our dependent variable.


yjabnorm=preProcess(n[,abnormal],method="YeoJohnson")

min(abs(sapply(yjabnorm$yj,function(x) x$lambda)))

yjtransf = function(x){
  z=x$y
  l=x$lambda
  if(l!=0){
    z[z>=0]=((z[z>=0]+1)^l-1)/l
  }
  else{
    z[z>=0]=log(z[z>=0]+1)
  }
  
  if(l!=2){
    z[z<0]=-((-z[z<0]+1)^(2-l)-1)/(2-l)
  }
  else{
    z[z<0]=-log(-z[z<0]+1)
  }
  z
}


np=n
np[,abnormal]=sapply(yjabnorm$yj,yjtransf)

lambdas = sapply(yjabnorm$yj,function(x) abs(x$lambda))
format(lambdas[order(-lambdas)],scientific=F)

# set up win/lose model
n.2=n
n.2$rpct=factor(n.2$rpct>50,levels=c(TRUE,FALSE),labels=c("Bush","Gore"))

np.2=np
np.2$rpct=factor(np.2$rpct>50,levels=c(TRUE,FALSE),labels=c("Bush","Gore"))
