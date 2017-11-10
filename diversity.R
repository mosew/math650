div=read.csv("diversityindex.csv", stringsAsFactors = FALSE)

div$state = sapply(div$Location, function(x) strsplit(x,", ")[[1]][2])
div$county = sapply(div$Location, function(x) strsplit(x,", ")[[1]][1])

stateonly=sapply(div$state, function(x) is.na(x))

div=div[,c(11,10,2)]
