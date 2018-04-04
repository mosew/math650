###########################################
# EXPLORING THE DATA
#

library(corrplot)
library(ggplot2)
library(gridExtra)
library(choroplethr)
library(ggthemes)

source('C:/Users/mose/Dropbox/math650/votes/2000/preprocess.R')

data("df_pop_county","df_pop_state")

t$region=as.integer(t$fips)
t=left_join(df_pop_county,t,by="region",all=F)
t.fl = filter(t,state=="FL")
t.ca = filter(t,state=="CA")
t.nofl = filter(t,state!="FL")
t.noca = filter(t,state!="CA")

states=df_pop_state$region
noak=states[-2] #Omit Alaska


### CORRELATION PLOT
corrplot(cor(subset(X,select= -rpct)),type="lower",order="hclust",cl.pos="b",tl.col="black",tl.srt=30,tl.cex = 0.9)



### PLOT OF ACTUAL US ELECTION RESULTS


# Prepare variables to be plotted. X is already prepared for algorithms, so we use the old, large df t to prepare plots.
t$voterparticip=t$totvotes/t$totpop.x*100
t$voterparticip[t$region==48301]=30
t$married=(t$mmarriednonsep+t$fmarriednonsep)/t$totpop.x*100
t$pci[t$pci==0]=median(t$pci)
t$logpci = as.numeric(log(t$pci))
t$hs[t$hs==0] = median(t[!is.na(t$hs),]$hs)
t$age20_34=(t$age20_24+t$age25_34)/t$totpop.x*100
t$bluecollaroccs = (t$farmoccs+t$constructionoccs+t$productionoccs)/t$totpop.x*100
vv = t$state=="AL" & !is.na(t$bluecollaroccs)
t$bluecollaroccs[t$region==1011]=mean(t$bluecollaroccs[vv])

t.fl = filter(t,state=="FL")
t.ca = filter(t,state=="CA")

t$notwhiteorafam = 100-(t$white1nh + t$black1)/t$totpop.x*100


#### PLOT VOTE SPREAD FOR USA AND FL

## USA
q=cbind.data.frame(region=t$region,value=t$rpct)
names(q)=c("region","value")
county_choropleth(unique(q),num_colors=1,state_zoom=noak)+
  labs(fill="% voting Bush") +
  scale_fill_gradient2(low = "steelblue4", mid="white", midpoint=50, high = "red2", na.value="white") +
  theme_tufte()+
  theme(legend.position=c(.16,.2),legend.background = element_rect(color = "black",
                                                                   fill = "grey90", size = 1, linetype = "solid"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


## FL
q.fl=cbind.data.frame(region=t.fl$region,value=t.fl$rpct)
names(q)=c("region","value")
county_choropleth(unique(q.fl),num_colors=0,state_zoom="florida")+
  labs(fill="% voting Bush") +
  theme_tufte()+
    theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black", fill = "grey90", size = 1, linetype = "solid"))+
  scale_fill_gradient2(low = "steelblue4", mid="white", midpoint=50, high = "red2", na.value="white") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_color_ptol()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())


## CA
q.ca=cbind.data.frame(region=t.ca$region,value=t.ca$rpct)
names(q)=c("region","value")
county_choropleth(unique(q.ca),num_colors=0,state_zoom="california")+
  labs(title = "Bush v. Gore, 2000",fill="% voting Bush") +
  theme_tufte()+
  theme(legend.position=c(0.8,0.8),legend.background = element_rect(color = "black", fill = "grey90", size = 0, linetype = "solid"))+
  scale_fill_gradient2(low = "steelblue4", mid="white", midpoint=50, high = "red2", na.value="white") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  scale_color_ptol()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())



### PLOT INTERESTING QUANTITIES ACROSS USA, FL, AND CA

plot_us_variable = function(n,title){
  q=cbind.data.frame(region=t$region,value=t[,n])
  names(q)=c("region","value")
  county_choropleth(unique(q),num_colors=1,state_zoom=noak)+
    theme_tufte()+
    labs(title=paste(title))+
    theme(legend.position=c(0.11,0.2),
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
}


plot_fl_variable = function(n,title){
  q=cbind.data.frame(region=t.fl$region,value=t.fl[,n])
  names(q)=c("region","value")
  county_choropleth(unique(q),num_colors=1,state_zoom="florida")+
    theme_tufte()+
    theme(legend.position=c(0.2,0.5))+
    #labs(title=paste(title))+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.title=element_blank())
}

plot_ca_variable = function(n,title){
  q=cbind.data.frame(region=t.ca$region,value=t.ca[,n])
  names(q)=c("region","value")
  county_choropleth(unique(q),num_colors=1,state_zoom="california")+
    theme_tufte()+
    theme(legend.position=c(0.8,0.7))+
    labs(title=paste(title))+
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.title=element_blank())
}

### PLOTS OF INTERESTING VARIABLES

plot_us_variable("white1nh","% identifying white and non-Hispanic")
plot_fl_variable("white1nh","% identifying white and non-Hispanic")
plot_ca_variable("white1nh","% identifying white and non-Hispanic")

plot_us_variable("voterparticip","% voting")
plot_fl_variable("voterparticip","% voting")
plot_ca_variable("voterparticip","% voting")

plot_us_variable("married","% married and not separated in 2000")
plot_fl_variable("married","% married and not separated in 2000")
plot_ca_variable("married","% married and not separated in 2000")

plot_us_variable("hispanic","% Hispanic")
plot_fl_variable("hispanic","% Hispanic")
plot_ca_variable("hispanic","% Hispanic")

plot_us_variable("pci","individual per capita income in 2000 (USD)")
plot_fl_variable("pci","individual per capita income in 2000 (USD)")
plot_ca_variable("pci","individual per capita income in 2000 (USD)")

plot_us_variable("logpci","log(individual per capita income in 2000 (USD))")
plot_fl_variable("logpci","log(individual per capita income in 2000 (USD))")
plot_ca_variable("logpci","log(individual per capita income in 2000 (USD))")

plot_us_variable("hs","% with HS diploma")
plot_fl_variable("hs","% with HS diploma")
plot_ca_variable("hs","% with HS diploma")

plot_us_variable("bluecollaroccs","% blue collar occupations")
plot_fl_variable("bluecollaroccs","% blue collar occupations")
plot_ca_variable("bluecollaroccs","% blue collar occupations")

plot_us_variable("notwhiteorafam","")
