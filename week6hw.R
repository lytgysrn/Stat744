library(tidyverse)
library(ggplot2)
library(gganimate)


#Data preprocess
#Data comes from Global terrorism database supported by the University of Maryland
#https://www.start.umd.edu/gtd/
#I upload a subset to my repository for running the code.

data.terror <- read.csv("https://raw.githubusercontent.com/lytgysrn/Stat744/main/Terror.csv", header = TRUE)
attach(data.terror)
data.terror$country<-factor(country)
data.terror$country_txt<-factor(country_txt)
data.terror$region<-factor(region)
data.terror$region_txt<-factor(region_txt)
summary(data.terror)
#This data set is too large, I decide to choose a subset of it. I will use the top 5 countries' data.


sample<-c(95,153,4,92,45)
data.x<-data.terror[data.terror$country %in% sample,]
#Only consider data after 2000
data.x<-data.x%>%filter(iyear>=2000)
summary(data.x)

#Remove NA
data.x<-na.omit(data.x)
#Left 63722 observations.

#Build the new data set for the year scale.
for (i in sample){
  assign(paste("country_", i, sep = ""), data.x[data.x$country==i,])
}

dataTrans<-function(country){
  N_events_year<-sapply(2000:2018, function(x){length(which(country$iyear==x))})
  N_kills_year<-sapply(2000:2018, function(x){sum(country[country$iyear==x,]$nkill)})
  country_name<-rep(as.character(country$country_txt[1]),19)
  year<-2000:2018
  data<-cbind(N_events_year,N_kills_year,country_name,year)
  return(data)
}

df.new<-as.data.frame(rbind(dataTrans(country_153),dataTrans(country_4),dataTrans(country_45),dataTrans(country_92),dataTrans(country_95)))
df.new$N_events_year<-as.numeric(df.new$N_events_year)
df.new$N_kills_year<-as.numeric(df.new$N_kills_year)

#animation plots ggani

gg0 <- ggplot(df.new)+geom_point(aes(x = log(N_events_year),
                                     #log for aesthetics
                    y = log(N_kills_year),
                    color=country_name,shape=country_name),size=5)+
                    theme_classic()+
                   geom_text(aes(x = 2, y = 8, label = year),
            size = 12, color = 'grey')+
  labs(x = "Log # of terror attacks",
       y = "Log # of deaths",color='Country',shape='Country')

gg1 <- gg0+transition_states(year,state_length = 3)+ease_aes('cubic-in-out')



#save it
gg1.gif <- animate(gg1)
gg1.gif
anim_save("terror.gif")

#X axis represents the log number of terror attacks by year, 
#y axis represents the log number of deaths because of terror attacks by year

#Or use package plotly 

library('plotly')
plot.fig <- df.new %>%
  plot_ly(
    x = ~log(N_events_year), 
    y = ~log(N_kills_year), 
    color = ~country_name,
    text = ~country_name, 
    hoverinfo = "text",
    frame = ~year, 
    type = 'scatter',
    mode = 'markers',
    marker=list(size=8)
  )
plot.fig<-plot.fig%>%layout(xaxis=list(title="Log # of terror attacks"),yaxis=list(title='Log # of deaths'))

plot.fig
#Advantage is we can use mouse to choose the year we are interested in.

# The terrorism in Iraq is not serious before 2003, and then both the number of terror attacks and deaths of it soar.
# Overall, from 2000 to 2018, the terrorism in Afghanistan, India, Iraq and Pakistan seem to become more rampant, while in Colombia, the level of terrorism seem to be steady.

# P.S. In presentation, for interpretability, we can also show raw data without log.  