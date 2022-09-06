library(tidyverse)

urlfile="https://github.com/WJC-Data-Science/DTS350/raw/master/coral.csv"
dat <- read_csv(url(urlfile))

dat
dim(dat)
str(dat)
head(dat, n=10)
tail(dat)

#Australasia
ggplot(data = filter(dat, Entity == "Australasia"), mapping= aes(x = Year, y = Value)) + 
  geom_bar(mapping = aes(fill = Event), stat = "identity")
#Indian Ocean/Middle East
ggplot(data = filter(dat, Entity == "Indian Ocean/Middle East"), mapping= aes(x = Year, y = Value)) + 
  geom_bar(mapping = aes(fill = Event), stat = "identity")
#Pacific
ggplot(data = filter(dat, Entity == "Pacific"), mapping= aes(x = Year, y = Value)) + 
  geom_bar(mapping = aes(fill = Event), stat = "identity")
#West Atlantic
ggplot(data = filter(dat, Entity == "West Atlantic"), mapping= aes(x = Year, y = Value)) + 
  geom_bar(mapping = aes(fill = Event), stat = "identity")
#World
ggplot(data = filter(dat, Entity == "World"), mapping= aes(x = Year, y = Value)) + 
  geom_bar(mapping = aes(fill = Event), stat = "identity")

#other Graphic
ggplot(data = dat) +
  geom_smooth(mapping = aes(x = Year, y = Value, linetype = Event))  

#We can see that in the early 2000's moderate bleaching occured more often but in recent years severe bleaching events have became frequent

  