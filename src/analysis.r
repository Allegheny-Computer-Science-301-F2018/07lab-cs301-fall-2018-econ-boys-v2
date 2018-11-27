##### Your Name: Econ Boys v2
##### Names of group members: Simon, Zach, Jerfenson
##### Date: 16 November 2018


##### Date source: 16 November 2018
#### World Bank's Online Data Base: Gender Statistics
###### https://datacatalog.worldbank.org/dataset/gender-statistics



###### Useful libraries:vir
library(dplyr)
library(tidyverse)
library(psych)
library(readxl)



##### Begin by loading your csv file from the supplimental/ directory.

data <- read.table(file.choose(),fill = TRUE , header = TRUE, sep = ",")

my_data <- read_excel(file.choose())
Australia <- filter(my_data, Country == "Australia")
Canada <- filter(my_data, Country == "Canada")

names(my_data)[1] <- "Country"
names(my_data)[3] <- "IndicatorName"




#### Analysis of Questions


##### Question 1: The decline in marriage rates.
##### Analysis to respond to this research question.

CanadaAnalysis <- filter(Canada[c(25, 75, 77),])
AustraliaAnalysis <- filter(Australia[c(25, 75, 77),])
                          
                           #Canada 
# Time required to start a business, female (days
# Average number of hours spent on unpaid domestic work (housework and child care), female
# Contributing family workers, female (% of female employment)   


                          #Australia
# Time required to start a business, female (days
# Average number of hours spent on unpaid domestic work (housework and child care), female
# Contributing family workers, female (% of female employment)   





##### Question 2: The narrowing gender wage gap
##### Analysis to respond to this research question.

                         #Canada 
# Wage and salaried workers, female (% of female employment)
# Educational attainment, completed Bachelor's or equivalent, population 25+ years, female (%)
# Employment in industry, female (% of female employment)




                        #Australia
# Wage and salaried workers, female (% of female employment)
# Educational attainment, completed Bachelor's or equivalent, population 25+ years, female (%)
# Employment in industry, female (% of female employment)


##### Question 3: The preference (or cultural) shift towards market work
##### Analysis to respond to this research question.


                        #Canada
# Labor force participation rate, female (% of female population ages 15+) (national estimate)
# Employers, female (% of female employment)
# Labor force, female (% of total labor force)


                      #Australia

# Labor force participation rate, female (% of female population ages 15+) (national estimate)
# Employers, female (% of female employment)
# Labor force, female (% of total labor force)


##### Question 4: The change in woman bargaining power within the household.
##### Analysis to respond to this research question.


                      #Canada
# Contributing family workers, female (% of female employment) 
#Wage and salaried workers, female (% of female employment) comparing to males 
# elf-employed, female (% of female employment)

                    #Australia
# Contributing family workers, female (% of female employment)  
# Wage and salaried workers, female (% of female employment) comparing to males 
# elf-employed, female (% of female employment)
