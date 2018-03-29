###########################################
# EXPLORING THE DATA
#

library(corrplot)
library(ggplot2)
library(gridExtra)
library(choroplethr)
library(ggthemes)

source('C:/Users/mose/Dropbox/math650/votes/2000/preprocess.R')

### CORRELATION PLOT
corrplot(cor(X),type="lower",order="AOE",cl.pos="b",tl.col="black",tl.srt=45)


### PLOT OF ACTUAL US ELECTION RESULTS

data("df_pop_county","df_pop_state")
t$region=as.integer(t$fips)
t=left_join(df_pop_county,t,by="region",all=F)

states=df_pop_state$region
noak=states[-2] #Omit Alaska

t.fl = filter(t,state=="FL")

#### PLOT VOTE SPREAD FOR USA AND FL

## USA
q=cbind.data.frame(region=t$region,value=t$rpct)
names(q)=c("region","value")
county_choropleth(unique(q),num_colors=0,state_zoom=noak)+
  labs(title = "Bush v. Gore, 2000",fill="% voting Bush") +
  scale_fill_gradient2(low = "blue", mid="white", midpoint=50, high = "red", na.value="white") +
  theme_tufte()+
  theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black", 
                                                                   fill = "grey90", size = 1, linetype = "solid"))+
  theme(panel.background = element_rect(fill = 'lightgoldenrod2', colour = 'black'))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# USA: WINNER TAKE ALL
q=cbind.data.frame(region=t$region,value=as.factor(t$rpct<=50))
names(q)=c("region","value")
county_choropleth(unique(q),num_colors=2,state_zoom=noak)+
  labs(title = "Bush v. Gore, 2000") +
  theme_tufte()+
  theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black", 
                                                                    fill = "grey90", size = 1, linetype = "solid"))+
  theme(panel.background = element_rect(fill = 'lightgoldenrod2', colour = 'black'))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("blue","red"))


## FL
q.fl=cbind.data.frame(region=t.fl$region,value=t.fl$rpct)
names(q)=c("region","value")
county_choropleth(unique(q.fl),num_colors=0,state_zoom="florida")+
  labs(title = "Bush v. Gore, 2000") +
  scale_fill_gradient2(low = "blue", mid="white", midpoint=50, high = "red", na.value="white") +
  theme_tufte()+
    theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black",                                                                     fill = "grey90", size = 1, linetype = "solid"))++
  theme(panel.background = element_rect(fill = 'lightgoldenrod2', colour = 'black'))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

# FL: WINNER TAKE ALL
q=cbind.data.frame(region=t.fl$region,value=as.factor(t.fl$rpct>50))
names(q)=c("region","value")
county_choropleth(unique(q),num_colors=2,state_zoom="florida")+
  labs(title = "Bush v. Gore, 2000") +
  theme_tufte()+
    theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black",                                                                     fill = "grey90", size = 1, linetype = "solid"))++
  theme(panel.background = element_rect(fill = 'lightgoldenrod2', colour = 'black'))+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  scale_fill_manual(values=c("blue","red"))



### PLOT INTERESTING QUANTITIES ACROSS USA AND FL

plot_us_variable = function(n,lolim,hilim){
  q=cbind.data.frame(region=t$region,value=t[,n])
  names(q)=c("region","value")
  county_choropleth(unique(q),num_colors=1,state_zoom=noak)+
    labs(title=paste(n),fill="") +
    scale_fill_gradient(low = "white", high="black",na.value="grey50",limits=c(lolim,hilim))+
    theme_tufte()+
      theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black",                                                                     fill = "grey90", size = 1, linetype = "solid"))++
    theme(axis.line=element_blank(),
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


plot_fl_variable = function(n,lolim,hilim){
  q=cbind.data.frame(region=t.fl$region,value=t.fl[,n])
  names(q)=c("region","value")
  county_choropleth(unique(q),num_colors=1,state_zoom="florida")+
    labs(title=paste(n),fill="") +
    scale_fill_gradient(low = "white", high="black",na.value="grey50",limits=c(lolim,hilim))+
    theme_tufte()+
    theme(legend.position="left")+
    theme(axis.line=element_blank(),
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







### PLOTS OF OTHER INTERESTING VARIABLES

plot_us_variable("poverty",0,43)
plot_fl_variable("poverty",0,43)

plot_us_variable("white1nh",0,100)
plot_fl_variable("white1nh",0,100)

t$voterparticip=t$totvotes/t$totpop.x*100
t$voterparticip[t$region==48301]=50
t.fl$voterparticip=filter(t,state=="FL")$voterparticip
plot_us_variable("voterparticip",0,80)
plot_fl_variable("voterparticip",0,80)


