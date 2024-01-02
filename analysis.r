df <- read.csv("dating.csv")

#a
c1 <- cor(df$Icecream, df$Games) 
c1
# The correlation value between eating ice cream and playing games is 0.008874313
# There is a very weak positive correlation between ice cream and playing games

c2 <- cor(df$Games, df$Miles) 
c2
# The correlation value between traveling and playing games is 0.4658472
# There is a moderate positive correlation between traveling and playing games


#b
model <- lm(Games~Miles, data = df)
model
summary(model)

library(ggplot2)
ggplot(df,aes(x=Miles,y=Games)) + geom_point() + 
  stat_smooth(method="lm") + ylim (0,2)

#Line equation: Games = (9.003e-05)*Miles + 3.532


#c
library(datasets)
head(df)

library(ggplot2)
ggplot(df,aes(x=Miles,y=Games, color = Like)) + 
  geom_point() + ylim (0,2)

set.seed(20)
dfCluster <- kmeans(df[, 2:3], 3, nstart = 20)
dfCluster

ggplot(df, aes(x=Miles,y=Games, color = dfCluster$cluster)) + geom_point()

#After comparing the two plots, the plot of the clustered data seems more readable.
#In the plot of the clustered data, different groups of data can be easily distinguished.
