#### 1 - Introdução ####

# Resolver o problema para analisar quais fatores influenciam na questão de conflitos
# Objetivo é identificar as relações com regressão logística e não prever  se terá ou não.

# fonte dos dados
# IBM Developer
# https://developer.ibm.com/patterns/data-science-life-cycle-in-action-to-solve-employee-attrition-problem/

# Definindo diretório de trabalho
setwd("C:/FCD/R/people_analytics")
getwd()


#### 2 - Carga de Pacotes e Dados ####

# Carga de pacotes
library(caret)
library(ggplot2)
library(gridExtra)
library(data.table)
library(car)
library(caTools)
library(corrplot)
library(rpart)
library(rpart.plot)

# Carga dos dados
bd_rh <- read.csv('dados/people_data.csv')

#### 3 - Informações sobre o dataset ####
# Dimensões do dataset
dim(bd_rh) #23.058 linhas e 30 colunas

# Tipos de dados
str(bd_rh)

# Resumo estatístico
summary(bd_rh)

# Visualização do dataset
View(bd_rh)

#### 4 - Limpeza e Pré-Processmento ####

# Classifica os atributos como tipo categórico
bd_rh$Attrition <- as.factor(bd_rh$Attrition)
bd_rh$BusinessTravel <- as.factor(bd_rh$BusinessTravel)
bd_rh$Department <- as.factor(bd_rh$Department)
bd_rh$Education <- as.factor(bd_rh$Education)
bd_rh$EducationField <- as.factor(bd_rh$EducationField)
bd_rh$Employee.Source <- as.factor(bd_rh$Employee.Source)
bd_rh$EnvironmentSatisfaction <- as.factor(bd_rh$EnvironmentSatisfaction)
bd_rh$Gender <- as.factor(bd_rh$Gender)
bd_rh$JobInvolvement <- as.factor(bd_rh$JobInvolvement)
bd_rh$JobLevel <- as.factor(bd_rh$JobLevel)
bd_rh$JobRole <- as.factor(bd_rh$JobRole)
bd_rh$JobSatisfaction <- as.factor(bd_rh$JobSatisfaction)
bd_rh$MaritalStatus <- as.factor(bd_rh$MaritalStatus)
bd_rh$OverTime <- as.factor(bd_rh$OverTime)
bd_rh$PerformanceRating <- as.factor(bd_rh$PerformanceRating)
bd_rh$RelationshipSatisfaction <- as.factor(bd_rh$RelationshipSatisfaction)
bd_rh$StockOptionLevel <- as.factor(bd_rh$StockOptionLevel)
bd_rh$WorkLifeBalance <- as.factor(bd_rh$WorkLifeBalance)

# Confirma se os dados estão como categóricos
str(bd_rh)

# Drop dos níveis de fatores com 0 count
dados <- droplevels(bd_rh)
str(bd_rh)
summary(bd_rh)
View(bd_rh)
