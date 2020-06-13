
rm(list = ls())

# Load libraries
library(tidyverse)
library(freqdist)
library(caret)
library(reshape2)

# Set working directory


### Q1
# ~~~~~~~

# import data1
d1 <- read.csv('data1.csv', header = T)

# gather variable columns in to value columns
d12 <- d1 %>% gather('task', 'time', 2:5)


## 1a : Stacked bar chart

ggplot(d12, aes(x = task, y = time, fill = location)) +
  geom_bar(stat = 'identity') +
  ggtitle("1.a) Stacked bar chart with locations along the vertical axis")+
  theme_bw()


## 1b : Clustered bar chart

ggplot(d12, aes(x = task, y = time, fill = location)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle("1.b) Clustered bar chart with locations along the vertical axis")+
  theme_bw()


## 1c : Multiple bar chart

ggplot(d12, aes(x = location, y = time, fill = task)) +
  geom_bar(stat = 'identity', position = 'fill') +
  ggtitle("1.c) Multiple bar charts where each location becomes a single bar chart showing the      percentage of time spent on tasks")+
  theme_bw()



### Q2
# ~~~~~~~

# import data2
d2 <- read.csv("data2.csv", header = T)
d2$sl <- seq(1, nrow(d2), by = 1)
colnames(d2)

# EDA

ggplot(d2) +
  geom_line(aes(x = sl, y = Overall), color = 'red', lwd = 1.5)+
  geom_line(aes(x = sl, y = Comfort), color = 'blue', lwd = 1)+
  geom_line(aes(x = sl, y = Amenities), color = 'green', lwd = 1)+
  geom_line(aes(x = sl, y = In.House.Dining), color = 'darkgrey', lwd = 1)+
  theme_classic()


## 2a : Multiple Linear Regression Equation

d22 <- d2[ , 2:5]  

MLinReg <- lm(Overall ~ . , d22)
MLinRegEquation <- as.data.frame(MLinReg$coefficients)
write.csv(MLinRegEquation, '2a_output.csv', row.names = T)


## 2b : F -test to determine the overall significance of regression

summary(MLinReg)

## 2c : t -test to determine significance of each indipendent variable

summary(MLinReg)


## 2d : remove non-significant variable and re-generate the Equation

d23 <- d22[ , c(1, 3, 4)]

MLinReg2 <- lm(Overall ~ . , d23)
MLinRegEquation2 <- as.data.frame(MLinReg2$coefficients)
write.csv(MLinRegEquation2, '2d_output.csv', row.names = T)


### Q3
# ~~~~~~~

# import data3
d3 <- read.csv("data3.csv", header = T)


## 3a : Frequency Distribution and Histogram

d32 <- table(d3$SAT_Score)
plot(d32)
title(main = 'Frequency Distribution')

ggplot(d3)+
  geom_histogram(aes(x = SAT_Score), 
                 binwidth = 200, fill = 'darkgreen', color = 'yellow')+
  xlim(c(800, 2400)) +
  ggtitle("Histogram")+
  theme_bw()



## 3b : shape of the distribution

h <- hist(d3$SAT_Score,
          freq = F,
          col = "green",
          main = "Histogram with Normal Curve"
)


curve(dnorm(x, mean = mean(d3$SAT_Score), sd= sd(d3$SAT_Score)),
      col = "red",
      lwd =5,
      add = TRUE
)


## 3c : Summary

summary(d3)

boxplot(d3)
title("Boxplot")



### Q4
# ~~~~~~~

# import data4
d4 <- read.csv("data4.csv", header = T)


## 4a : Frequency distribution and histogram

fnow <- freqdist(d4$live_now)
write.csv(fnow, '4a_output1.csv', row.names = T)

fideal <- freqdist(d4$live_ideal)
write.csv(fideal, '4a_output2.csv', row.names = T)


fnow$area <-  row.names(fnow)
fnow <- fnow[ -5 , ]
ggplot(fnow) +
  geom_bar(aes(x = area, y = frequencies),
           stat = 'identity', fill = 'blue') +
  ggtitle("Histogram(Bar chart) of Live Now") +
  theme_bw()


fideal$area <-  row.names(fideal)
fideal <- fideal[ -5 , ]
ggplot(fideal) +
  geom_bar(aes(x = area, y = frequencies),
           stat = 'identity', fill = 'red') +
  ggtitle("Histogram(Bar chart) of Live IDEAL") +
  theme_bw()


## 4b : Where are most adults living now?


fnow.max <- fnow %>% filter(frequencies == max(fnow$frequencies))
fnow.max


## 4c : Where do most adults consider the ideal community

fideal.max <- fideal %>% filter(frequencies == max(fideal$frequencies))
fideal.max



## 4D : changes in liven areas

cm <- confusionMatrix(d4$live_now, d4$live_ideal)

ctable <- table(d4$live_now, d4$live_ideal)

library(reshape2)

melted_ctable <- melt(ctable)


ggplot(data = melted_ctable, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  xlab("Live Now") +
  ylab("Live IDEAL")+
  theme_classic()





