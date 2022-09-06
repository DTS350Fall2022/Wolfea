?iris
dat <- iris
dat
#1
ggplot(data = dat) + 
  geom_point(mapping = aes(x = Sepal.Length, y = Sepal.Width, shape = Species, color = Species))
#what Species have the longest sepals and which species have the greatest width
#Virginica has to longest Sepals while setosa has the widest sepals. Versicolor is the best mix of petal width and length of the three Species.
#2
ggplot(data = dat) +
  geom_point(mapping = aes(x = Petal.Width, y = Petal.Length, shape = Species, color =Species)) +
    facet_wrap(~ Species, nrow = 1)
#Which Species genrally has the largest and smallest flowers to help with identification of species
#Setosa has the smallest petals while Virginica has the largest petals. Once again Versicolor is in the middle of the pack being the second largest species.
#3  

graph<- ggplot(data= dat, mapping = aes(x= Petal.Length, y= Petal.Width)) +
    geom_point() +
    geom_smooth(method=lm)
graph + geom_point(aes(color = Species, shape = Species))
#Are all three Species petals length to width ratios similar?
#yes while they do vary in size the petals seem to follow a linear line with few outliers.
#4
#Which species has most difference in size by species?
#Visually Virginica has the most spread out data than the other two species. Setosa seems to have the least differential.
ggplot(data = dat, mapping = aes(x = Sepal.Length, fill = Species)) +
  geom_bar(alpha = 5/8) +
  geom_vline(xintercept = mean(Sepal.Length), linetype="dotted", 
             color = "black", size=1)
#Which species has most difference in size by species?
#Visually Virginica has the most spread out data than the other two species. Setosa seems to have the least differential.
#other code was not running for some reason so this is my theoretical avg code
ggplot(data = dat, mapping = aes(x = Sepal.Length, fill = Species)) +
  geom_bar(alpha = 5/8) +
  geom_vline(xintercept = 5.9, linetype="dotted", 
             color = "black", size=1)
  
 
