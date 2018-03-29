`%not in%` = function (x, table) is.na(match(x, table, nomatch=NA_integer_))

counties=map_data("county")
states=map_data("state")
counties=subset(counties,region %not in% c("alaska"))
states=subset(states,region %not in% c("alaska"))

base = ggplot(data=states, mapping = aes(x=long,y=lat,group=group))+
  coord_fixed(1.3) +
  geom_polygon(color="black",fill=NA)

sc = base + geom_polygon(data=counties,fill=NA,color="white")+
  geom_polygon(color="black",fill=NA)

counties$subregion=sapply(counties$subregion,function(x) gsub("\\.","",gsub(" ","",x)))
t$county=sapply(t$county,function(x) gsub("\\.","",gsub(" ","",x)))
t$statename=tolower(state.name[match(t$state,state.abb)])


votes=full_join(counties,t[,-c(3,4,5,68,69,60)],by=c("subregion"="county","region"="statename"))
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

election = sc + geom_polygon(data=votes, aes(fill=rpct),color="white")+
  geom_polygon(color="black",fill=NA)+
  theme_bw()+
  ditch_the_axes

election

