setwd("C:/Users/mose/Dropbox/math650/votes/2000")

library(plyr)
library(dplyr)
library(magrittr)
library(scales)
library(openxlsx)


# numeric data in the spreadsheet are read in as character data; this function will fix that
tonumeric=function(y){
  c2 = suppressWarnings(data.frame(data.matrix(y)))
  numeric_columns = sapply(c2,function(x){mean(as.numeric(is.na(x)))<0.5})
  return(cbind(y[,!numeric_columns],c2[,numeric_columns]))
}

  

# Load census data.
# Limitation: based on sample data
full=read.csv("raw/csv/full2.csv",header=T,stringsAsFactors = F,strip.white=T)[-c(1,2),] %>%
  
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


# For some reason Virginia is missing per capita income data, so I set each county's equal to the statewide avg
t$pci[t$state=="VA"]=23975
t$fips = paste(sprintf("%02d",t$Statecode),sprintf("%03d",t$COUNTYCODE),sep="")


# Some (small) counties missing information

# Kalawao, HI
# Broomfield, CO
# Bullock, AL
# Oglala Lakota County, SD
# Loving, TX

t=t[-unique(which(is.na(t),arr.ind=T)[,1]),]
X=t[-which(t$medianhvalue==0,arr.ind=T),]
sum(is.na(X))
# > 0


topct = 100/X$totpop.x

X = X %>%
  mutate(pop=totpop.x,
         other1=other1+pacisl1+native1,
         nevermarried=(fnevermarried+mnevermarried)*topct,
         married=(fmarriednonsep+mmarriednonsep)*topct,
         sepdiv=(fseparated+fdivorced+mdivorced+mseparated)*topct,
         logpop=as.numeric(log(pop)),
         logpoppersqmi=as.numeric(log(poppersqmi)),
         age20_34=(age20_24+age25_34)*topct,
         age35_54=(age35_44+age45_54)*topct,
         age55_ov=(age55_59+age60_64+age65ov)*topct,
         vcperpop=vc/pop,
         homeaffordability=pci/medianhvalue*100,
         homeless=(pop-inhomes)*topct,
         nohealthinsurance=nohealthinsurance*topct,
         urban=urbanpop*topct,
         farm=farmpop*topct,
         voterparticip=totvotes*topct,
         bluecollaroccs=(farmoccs+constructionoccs+productionoccs)*topct,
         incollege=incollege*topct,
         houseperpop=tothous/pop)

X$bluecollaroccs[X$region==1011]=mean(X$bluecollaroccs[X$state=="AL"])
X$long[X$long>0]=X$long[X$long>0]-180
fips = X$fips


# Look for heavy tails (and heads) and deal with them

# First identify non-normal distributions by the Shapiro-Wilk test
#shapiro.score=X%>%
#  apply(2,shapiro.test)%>%
#  sapply(function(x) x[[1]][[1]])

#abnormvars=names(X)[shapiro.score<.92]

X$medianhhincome[X$medianhhincome==0]=median(X$medianhhincome[X$medianhhincome>0])
X$pci[X$pci==0]=median(X$pci)

X = X %>%
  mutate(logincollege=as.numeric(log(incollege)),
         logba=log(ba),
         logpctmultiunitdwelling=as.numeric(pctmultiunitdwelling),
         logmedianhvalue=as.numeric(log(medianhvalue)),
         logmediangrossrent=as.numeric(log(mediangrossrent)),
         logunemployed=as.numeric(log(unemployed+.1)),
         logpci=as.numeric(log(pci)),
         logforeignborn=as.numeric(log(foreignborn+.1)),
         lognonenglish=as.numeric(log(esl+.1)),
         loglandsqmi=as.numeric(log(landsqmi)),
         loghomeless=as.numeric(log(homeless+.1))
         )


X = subset(X,select=-c(pacisl1,native1,fnevermarried,mnevermarried,fmarriednonsep,mmarriednonsep,mwidowed,fwidowed,
                     fseparated,fdivorced,mseparated,mdivorced,poppersqmi,totpop.x,pop,totvotes,tothous,
                     age20_24,age25_34,age35_44,age45_54,age55_59,age60_64,age65ov,vc,inhomes,landsqm,
                     urbanpop,farmpop,fips))
X = subset(X,select=-c(ba,medianhvalue,mediangrossrent,landsqmi,incollege,pctmultiunitdwelling,medianhhincome,unemployed,
                       foreignborn,esl,homeless,watersqm,watersqmi,pci,black1,
                       county,Statecode,COUNTYCODE,totpop.y,statecode,
                       farmoccs,profoccs,serviceoccs,salesoccs,constructionoccs,productionoccs))

# Removes two columns with weird names
X = X[,-c(2,16)]

X.noca = X %>%
  filter(state!="CA")%>%
  mutate(state=NULL,
         rpct04=NULL,
         rpct08=NULL)
X.ca = X %>%
  filter(state=="CA") %>%
  mutate(state=NULL,
         rpct04=NULL,
         rpct08=NULL)

X.nofl = X %>%
  filter(state!="FL")%>%
  mutate(state=NULL,
         rpct04=NULL,
         rpct08=NULL)

X.fl = X %>%
  filter(state=="FL") %>%
  mutate(state=NULL,
         rpct04=NULL,
         rpct08=NULL)
X = mutate(X,state=NULL,
           rpct04=NULL,
           rpct08=NULL)


data("df_pop_county","df_pop_state")

t$region=as.integer(t$fips)
t=left_join(df_pop_county,t,by="region",all=F)
t.fl = filter(t,state=="FL")
t.ca = filter(t,state=="CA")
t.nofl = filter(t,state!="FL")
t.noca = filter(t,state!="CA")

states=df_pop_state$region
noak=states[-2] #Omit Alaska



