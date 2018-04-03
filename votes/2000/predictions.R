# Plot predictions

library(choroplethr)
library(ggthemes)


## USA
X$rpct=NULL
X$rpct08=NULL
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




## FL
# Lasso
lasso.fl = predict(lasso.fit,newdata=X.fl)
q.fl=cbind.data.frame(region=t.fl$region,value=lasso.fl)
names(q)=c("region","value")
county_choropleth(unique(q.fl),num_colors=0,state_zoom="florida")+
  labs(title = "Lasso",fill="% voting Bush") +
  theme_tufte()+
  theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black", fill = "grey90", size = 0, linetype = "solid"))+
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

rf.fl = predict(rf.fit,newdata=X.fl)
q.fl=cbind.data.frame(region=t.fl$region,value=rf.fl)
names(q)=c("region","value")
county_choropleth(unique(q.fl),num_colors=0,state_zoom="florida")+
  labs(title = "Random forest",fill="% voting Bush") +
  theme_tufte()+
  theme(legend.position=c(0.2,0.2),legend.background = element_rect(color = "black", fill = "grey90", size = 0, linetype = "solid"))+
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


gbm.fl = predict(gbm.fit,newdata=X.fl)
q.fl=cbind.data.frame(region=t.fl$region,value=gbm.fl)
names(q)=c("region","value")
county_choropleth(unique(q.fl),num_colors=0,state_zoom="florida")+
  labs(title = "Stochastic gradient boosting",fill="% voting Bush") +
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

## CA
# Lasso
lasso.ca = predict(lasso.fit,newdata=X.ca)
q.ca=cbind.data.frame(region=t.ca$region,value=lasso.ca)
names(q)=c("region","value")
county_choropleth(unique(q.ca),num_colors=0,state_zoom="california")+
  labs(title = "Lasso",fill="% voting Bush") +
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
        legend.position="none")

rf.ca = predict(rf.fit,newdata=X.ca)
q.ca=cbind.data.frame(region=t.ca$region,value=rf.ca)
names(q)=c("region","value")
county_choropleth(unique(q.ca),num_colors=0,state_zoom="california")+
  labs(title = "Random forest",fill="% voting Bush") +
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
        legend.position="none")


gbm.ca = predict(gbm.fit,newdata=X.ca)
q.ca=cbind.data.frame(region=t.ca$region,value=gbm.ca)
names(q)=c("region","value")
county_choropleth(unique(q.ca),num_colors=0,state_zoom="california")+
  labs(title = "Stochastic gradient boosting",fill="% voting Bush") +
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
