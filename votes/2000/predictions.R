# Plot predictions

library(choroplethr)
library(ggthemes)

X$rpct04=NULL
X$rpct08=NULL
t.fl = filter(t,state=="FL")
t.ca = filter(t,state=="CA")


## USA
gbm.usa = predict(gbm.fit,newdata=X)
a=cbind.data.frame(cbind.data.frame(fips=as.integer(fips),X),gbm.usa)
tt = left_join(df_pop_county,a,by=c("region"="fips"),all=F)
tt = data.frame(unique(tt[!is.na(tt$gbm.usa),]))
q=unique(cbind.data.frame(region=tt$region,value=tt$gbm.usa))
names(q)=c("region","value")
q = filter(q,!duplicated(q$region))
county_choropleth(unique(q),num_colors=1,state_zoom=noak)+
  labs(title = "Predicted",fill="% voting Bush") +
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


gbm.fl = predict(gbm.fit.fl,newdata=X.fl)
q.fl=cbind.data.frame(region=t.fl$region,value=gbm.fl)
names(q)=c("region","value")
county_choropleth(unique(q.fl),num_colors=0,state_zoom="florida")+
  #labs(title = "Pr",fill="% voting Bush") +
  theme_tufte()+
  #theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black", fill = "grey90", size = 0, linetype = "solid"))+
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
        panel.grid.minor=element_blank(),
        legend.position = "none")

cor(gbm.fl,X.fl$rpct)^2

gbm.ca = predict(gbm.fit,newdata=X.ca)
t.ca = filter(t,state=="CA")
q.ca=cbind.data.frame(region=t.ca$region,value=gbm.ca)
names(q)=c("region","value")
county_choropleth(unique(q.ca),num_colors=0,state_zoom="california")+
  labs(title = "Predicted, 2000",fill="% voting Bush") +
  theme_tufte()+
  #theme(legend.position=c(0.7,0.8),legend.background = element_rect(color = "black", fill = "grey90", size = 0, linetype = "solid"))+
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
        panel.grid.minor=element_blank(),
        legend.position="none")


cor(gbm.ca,X.ca$rpct)^2