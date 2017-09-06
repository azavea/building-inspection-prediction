#Set our working directory. 
#This helps avoid confusion if our working directory is 
#not our site because of other projects we were 
#working on at the time. 
setwd("C:/files/data_analytics/building-inspection-prediction/docs")

#render site. 
rmarkdown::render_site()
