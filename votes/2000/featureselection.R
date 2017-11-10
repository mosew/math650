################# FEATURE SELECTION ################

source('C:/Users/mose/Dropbox/HW/math650/votes/2000/proj.R')

rfe.control=rfeControl(functions=caretFuncs, 
                       method = "cv",
                       repeats =1, number = 10)
set.seed(323)

less=subset(n,select=-c(married,white1nh,lnpoppersqm,vehperhouse,latitude,longitude,
                        emplgov,emplstatelocalgov,hospinsured,deathsper1000,naturalized,
                        multrace,widowed,asian,asian1))

c=rfe(rpct~.,data=less,sizes=1,rfeControl=rfe.control)