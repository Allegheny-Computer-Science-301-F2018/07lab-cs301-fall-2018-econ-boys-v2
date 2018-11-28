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
library(varhandle)




##### Begin by loading your csv file from the supplimental/ directory.

#data <- read.table(file.choose(),fill = TRUE , header = TRUE, sep = ",")

my_data <- read_excel(file.choose())
names(my_data)[1] <- "Country"
names(my_data)[3] <- "IndicatorName"
attach(my_data)

Australia <- filter(my_data, Country == "Australia")
Canada <- filter(my_data, Country == "Canada")



#### Analysis of Questions


##### Question 1: The decline in marriage rates.
##### Analysis to respond to this research question.

CanadaAnalysis <- filter(Canada[c(75, 77, 218),])
AustraliaAnalysis <- filter(Australia[c(75, 77, 218),])

AustraliaAnalysis <- as.data.frame(t(AustraliaAnalysis))
AustraliaAnalysis <- AustraliaAnalysis[-c(1,2,4),]
colnames(AustraliaAnalysis) <- c("contributingA", "startupA", "total_laborA")
AustraliaAnalysis <- AustraliaAnalysis[-c(1),]
AustraliaAnalysis <- AustraliaAnalysis[-c(1:30), ]
AustraliaAnalysis <- cbind(rownames(AustraliaAnalysis), AustraliaAnalysis)
rownames(AustraliaAnalysis) <- NULL
names(AustraliaAnalysis)[1] <- "Year"
attach(AustraliaAnalysis)
#plot for contribution:
ggplot(AustraliaAnalysis) + geom_point(mapping = aes(x = Year, y = contributingA))
#plot for startup:
ggplot(AustraliaAnalysis) + geom_point(mapping = aes(x = Year, y = startupA))
#plot for total female labor as % of labor force:
ggplot(AustraliaAnalysis) + geom_point(mapping = aes(x = Year, y = total_laborA))
#To generate p-value:
Australia_Contribution <- unfactor(contributingA)
Australia_Startup <- unfactor(startupA)
Australia_Labor <- unfactor(total_laborA)
Australia_Mod1 <- lm(Australia_Labor ~ Australia_Contribution + Australia_Startup)
summary(Australia_Mod1)
#End of Australia Analysis

CanadaAnalysis <- as.data.frame(t(CanadaAnalysis))
CanadaAnalysis <- CanadaAnalysis[-c(1,2,4),]
colnames(CanadaAnalysis) <- c("contributingC", "startupC", "total_laborC")
CanadaAnalysis <- CanadaAnalysis[-c(1:31), ]
CanadaAnalysis <- cbind(rownames(CanadaAnalysis), CanadaAnalysis)
rownames(CanadaAnalysis) <- NULL
names(CanadaAnalysis)[1] <- "Year"
attach(CanadaAnalysis)
#plot for contribution:
ggplot(CanadaAnalysis) + geom_point(mapping = aes(x = Year, y = contributingC))
#plot for startup:
ggplot(CanadaAnalysis) + geom_point(mapping = aes(x = Year, y = startupC))
#plot for total female labor as % of labor force:
ggplot(CanadaAnalysis) + geom_point(mapping = aes(x = Year, y = total_laborC))
#TO generate p-value
Canada_Contribution <- unfactor(contributingC)
Canada_Startup <- unfactor(startupC)
Canada_Labor <- unfactor(total_laborC)
Canada_Mod1 <- lm(Canada_Labor ~ Canada_Contribution + Canada_Startup)
summary(Canada_Mod1)
#End of Canada Analysis

                           #Canada 
# Time required to start a business, female (days
# Contributing family workers, female (% of female employment)   

#mod <- lm(LungCap ~ Age + Height)
#summary(mod)


                          #Australia
# Time required to start a business, female (days
# Contributing family workers, female (% of female employment)   



##### Question 2: The narrowing gender wage gap
##### Analysis to respond to this research question.

CanadaAnalysisQ2 <- filter(Canada[c(121, 148, 498),])
AustraliaAnalysisQ2 <- filter(Australia[c(121, 148, 498),])

AustraliaAnalysisQ2 <- as.data.frame(t(AustraliaAnalysisQ2))
AustraliaAnalysisQ2 <- AustraliaAnalysisQ2[-c(1,2,4),]
colnames(AustraliaAnalysisQ2) <- c("Educational_BachelorA", "Employment_industryA", "Wage_salariedA")
AustraliaAnalysisQ2<- AustraliaAnalysisQ2[-c(1),]
AustraliaAnalysisQ2<- cbind(rownames(AustraliaAnalysisQ2), AustraliaAnalysisQ2)
rownames(AustraliaAnalysisQ2) <- NULL
names(AustraliaAnalysisQ2)[1] <- "Year"
attach(AustraliaAnalysisQ2)


#plot for Education:
ggplot(AustraliaAnalysisQ2) + geom_point(mapping = aes(x = Year, y = Educational_BachelorA))
#plot for Employment:
ggplot(AustraliaAnalysisQ2) + geom_point(mapping = aes(x = Year, y = Employment_industryA))
#plot for Wages and Salried:
ggplot(AustraliaAnalysisQ2) + geom_point(mapping = aes(x = Year, y = Wage_salariedA))

#To generate p-value:
Australia_Education <- unfactor(Educational_BachelorA)
Australia_Employment <- unfactor(Employment_industryA)
Australia_Wages_Salaries <- unfactor(Wage_salariedA)
AustraliaQ2_Mod1 <- lm(Australia_Employment ~ Australia_Education + Australia_Wages_Salaries)
summary(AustraliaQ2_Mod1)
#End of Australia Analysis




CanadaAnalysisQ2 <- as.data.frame(t(CanadaAnalysisQ2))
CanadaAnalysisQ2 <- CanadaAnalysisQ2[-c(1,2,4),]
colnames(CanadaAnalysisQ2) <- c("Educational_BachelorC", "Employment_industryC", "Wage_salariedC")
CanadaAnalysisQ2<- CanadaAnalysisQ2[-c(1),]
CanadaAnalysisQ2 <- cbind(rownames(CanadaAnalysisQ2), CanadaAnalysisQ2)
rownames(CanadaAnalysisQ2) <- NULL
names(CanadaAnalysisQ2)[1] <- "Year"
attach(CanadaAnalysisQ2)

#plot for Education:
ggplot(CanadaAnalysisQ2) + geom_point(mapping = aes(x = Year, y = Educational_BachelorC))
#plot for Employment:
ggplot(CanadaAnalysisQ2) + geom_point(mapping = aes(x = Year, y = Employment_industryC))
#plot for Wages and Salried:
ggplot(CanadaAnalysisQ2) + geom_point(mapping = aes(x = Year, y = Wage_salariedC))

#To generate p-value:
Canada_Education <- unfactor(Educational_BachelorC)
Canada_Employment <- unfactor(Employment_industryC)
Canada_Wages_Salaries <- unfactor(Wage_salariedC)
CanadaQ1_Mod1 <- lm(Canada_Employment ~ Canada_Education + Canada_Wages_Salaries)
summary(CanadaQ1_Mod1)
#End of Australia Analysis

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
