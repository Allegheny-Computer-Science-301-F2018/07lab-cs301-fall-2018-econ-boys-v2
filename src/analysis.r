##### Your Name: Econ Boys v2
##### Names of group members: Simon, Zach, Jerfenson
##### Date: 16 November 2018


##### Date source: 16 November 2018
#### World Bank's Online Data Base: Gender Statistics
###### https://datacatalog.worldbank.org/dataset/gender-statistics



###### Useful libraries:
library(dplyr)
library(tidyverse)
library(psych)



##### Begin by loading your csv file from the supplimental/ directory.

data <- read.table(file.choose(),fill = TRUE , header = TRUE, sep = ",")
Australia <- filter(data, Country.Name == "Australia")
Argentina <- filter(data, Country.Name = "Argentina")




#### Analysis of Questions


##### Question 1: The decline in marriage rates.
##### Analysis to respond to this research question.



##### Question 2: The narrowing gender wage gap
##### Analysis to respond to this research question.



##### Question 3: The preference (or cultural) shift towards market work
##### Analysis to respond to this research question.



##### Question 4: The change in women’s bargaining power within the household.
##### Analysis to respond to this research question.
