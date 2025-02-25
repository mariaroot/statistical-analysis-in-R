install.packages("datarium") 
install.packages("tidyverse") 
install.packages("corrplot") 
install.packages("rcompanion")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("RVAideMemoire")

library(datarium)
library(tidyverse)
library(corrplot)
library(rcompanion)
library(qqplotr)
library(ggplot2)
library(RVAideMemoire)
library(readxl)

red_wine <- read_xlsx(path = 'red_wine.xlsx')
white_wine <- read_xlsx(path = 'white_wine.xlsx')

#EDA: top of both datasets and summary statistics 
head(red_wine)
summary(red_wine)
head(white_wine)
summary(white_wine)

#EDA: checking datatypes in both datasets 
str(red_wine)
str(white_wine)

#read in red and white wine data in one dataset, check datatypes
wine <- read_xlsx(path = 'wine.xlsx')
str(wine)

#EDA: boxplots of target variable (quality) to check for outliers 
boxplot(quality ~ colour , data=wine, names=c("R", "W"), 
        xlab="Wine colour", ylab="quality", main="Quality of red and white wine")


##CORRELATIONS

#remove discrete and categorical data (quality, colour):
wine_continuous <- wine %>% select(-quality, -colour)
#correlation for all continuous variables and rounded to 2 decimals
corrplot(cor(wine_continuous), method = "number", type = "upper")

#point biserial correlation coefficient for continuous and nominal variables 
wine_colour_codes <- read_xlsx(path = 'wine_colour_codes.xlsx')

cor.test(wine_colour_codes$`fixed acidity` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`volatile acidity` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`citric acid` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`residual sugar` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`chlorides` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`free sulfur dioxide` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`total sulfur dioxide` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`density` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`pH` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`sulphates` , wine_colour_codes$`colour code`)

cor.test(wine_colour_codes$`alcohol` , wine_colour_codes$`colour code`)

#Scatterplots for physiochemical properties with biggest correlation coefficients 
plot(wine_colour_codes$`volatile acidity` , wine_colour_codes$`colour code`)

plot(wine_colour_codes$`total sulfur dioxide` , wine_colour_codes$`colour code`)

#LOGISTIC REGRESSION
install.packages("car")
install.packages("corrplot")
install.packages("caret")
install.packages ("carData")
install.packages ("lattice")

library(car)
library(corrplot)
library(caret)
library(carData)
library(lattice)
library(readxl)

wine_grouped <- read_xlsx(path = 'wine_grouped.xlsx')
str(wine_grouped)

logistic_model_1 <- glm(quality_group ~ fixed_acidity + volatile_acidity + citric_acid + residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide + density + pH + sulphates + alcohol, data = wine_grouped, family = "binomial")
summary(logistic_model_1)

logistic_model_2 <- glm(quality_group ~ volatile_acidity + citric_acid + residual_sugar + free_sulfur_dioxide + total_sulfur_dioxide + pH + sulphates + alcohol, data = wine_grouped, family = "binomial")
summary(logistic_model_2)

logistic_model_3 <- glm(quality_group ~ volatile_acidity + residual_sugar + free_sulfur_dioxide + total_sulfur_dioxide + sulphates + alcohol, data = wine_grouped, family = "binomial")
summary(logistic_model_3)

Imp <- varImp(logistic_model_2, scale = FALSE)
Imp

logistic_model_4 <- glm(quality_group ~ volatile_acidity + alcohol, data = wine_grouped, family = "binomial")
summary(logistic_model_4)

#Checking assumptions of logistic regression
#Linearity assumption

probs <- predict(logistic_model_2, data=wine_grouped,type="response") 
wine_grouped$probs <- probs
logits <- log(probs/(1-probs)) 
wine_grouped$logits <- logits
data.frame(colnames(wine_grouped))

pairs(wine_grouped[,c(16,2,3,4,6,7,9,10,11)], lower.panel = NULL, upper.panel = panel.smooth, pch = 19,cex = 0.2)

#Influential values assumption
plot(logistic_model_2, which = 4, id.n = 3)

#Multicolinearity assumption 
vif(logistic_model_2)

#Trying model that excludes non-linear variables 
logistic_model_5 <- glm(quality_group ~ volatile_acidity + residual_sugar + free_sulfur_dioxide + alcohol, data = wine_grouped, family = "binomial")
summary(logistic_model_5)

##HYPOTHESIS TESTING 

##Independent two-sample t tests for physiochem properties vs wine colour

#Assessing normality of dependent numerical variables 
ggplot(mapping = aes(sample=red_wine$`fixed acidity`)) + stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=white_wine$`fixed acidity`)) + stat_qq_point(size = 2,color = "blue") + 
  stat_qq_line(color="orange") + xlab("Theoretical") + ylab("Sample")

shapiro.test(red_wine$`fixed acidity`)

shapiro.test(white_wine$`fixed acidity`)

shapiro.test(red_wine$`volatile acidity`)

shapiro.test(white_wine$`volatile acidity`)

shapiro.test(red_wine$`citric acid`)

shapiro.test(white_wine$`citric acid`)

shapiro.test(red_wine$`residual sugar`)

shapiro.test(white_wine$`residual sugar`)

shapiro.test(red_wine$chlorides)

shapiro.test(white_wine$chlorides)

shapiro.test(red_wine$`free sulfur dioxide`)

shapiro.test(white_wine$`free sulfur dioxide`)

shapiro.test(red_wine$`total sulfur dioxide`)

shapiro.test(white_wine$`total sulfur dioxide`)

shapiro.test(red_wine$density)

shapiro.test(white_wine$density)

shapiro.test(red_wine$pH)

shapiro.test(white_wine$pH)

shapiro.test(red_wine$sulphates)

shapiro.test(white_wine$sulphates)

shapiro.test(red_wine$alcohol)

shapiro.test(white_wine$alcohol)

#Independent two-sample t tests for physiochem properties vs wine colour
t.test(`fixed acidity` ~ colour, wine)

t.test(`volatile acidity` ~ colour, wine)

t.test(`citric acid` ~ colour, wine)

t.test(`residual sugar` ~ colour, wine)

t.test(chlorides ~ colour, wine)

t.test(`free sulfur dioxide` ~ colour, wine)

t.test(`total sulfur dioxide` ~ colour, wine)

t.test(density ~ colour, wine)

t.test(pH ~ colour, wine)

t.test(sulphates ~ colour, wine)

t.test(alcohol ~ colour, wine)

#Chi-squared test of independence for wine colour vs quality
#contingency table:
table(wine$colour, wine$quality)

#bar plots:
library(ggplot2)

ggplot(wine) +
  aes(x = quality, fill = colour) +
  geom_bar(position = "dodge")

ggplot(wine) +
  aes(x = quality, fill = colour) +
  geom_bar(position = "fill")

chisq_test <- chisq.test(table(wine$colour, wine$quality))

fisher_test <- fisher.test(table(wine$colour, wine$quality), simulate.p.value=TRUE)
fisher_test
