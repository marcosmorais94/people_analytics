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

#### 5 - Engenharia de Atributos ####
# Nesta etapa vamos incluir alguns atributos que não foram identificadas na base original
# Contudo, são informações que podemos incluir a partir do dataset original

# Pior Year of Experience siginifica quantos anos o profissional tem de experiência profissional
bd_rh$PriorYearsOfExperience <- bd_rh$TotalWorkingYears - bd_rh$YearsAtCompany
View(bd_rh)

#Average Tenure é a estabilidade média do profissional no mesmo emprego
bd_rh$AverageTenure <- bd_rh$PriorYearsOfExperience / bd_rh$NumCompaniesWorked
View(bd_rh)

# A Average Tenure produz valores como Inf devido à natureza de sua derivação
# É possível identificar esse valores pelo summary, neste caso na média
summary(bd_rh$AverageTenure) 

# Substituímos para zero, onde tudo que for contrário de finito será igualado a 0.
bd_rh$AverageTenure[!is.finite(bd_rh$AverageTenure)] <- 0

# Confere se ainda há valores Inf
summary(bd_rh$AverageTenure)
View(bd_rh)

#### 6 - Análise Exploratória ####

# Subset dos dados como base na coluna Termination, que indica se o funcionário foi desligado da empresa.
bd_rh_1 <- bd_rh[bd_rh$Attrition != 'Termination',]
bd_rh_1 <- droplevels(bd_rh_1)
dim(bd_rh_1)
summary(bd_rh_1)

# Mesmo filtro anterior, mas agora por demissão voluntária
bd_rh_2 <- bd_rh[bd_rh$Attrition != 'Voluntary Resignation',]
bd_rh_2 <- droplevels(bd_rh_2)
dim(bd_rh_2)  
summary(bd_rh_2)

# Ambos os subsets serão usado posteriormente na análise exploratória dos dados 
# e com isso relevar insights importantes.

# Plots de análise univariada

# Contagem por genêro
# Aqui vemos que a base de dados temos mais homens que mulheres na base
ggplot(bd_rh) + geom_bar(aes(x = Gender))

# Idade dos profissionais da IBM
# A idade é em torno de 30 a 35 anos em suas grande maioria, 
# indica que temos um boa parte dos profissionais com uma idade média
ggplot(bd_rh) + geom_density(aes(x = Age))

# Situação atual dos profissionais da base de dados
# A grande maioria continua empregado, uma boa parcela escolheu a demissão voluntária
# Essa fatia da demissão voluntária pode ter respostas interessantes do motivo da saída dos profissinais
# A menor parcela são os demitidos, que pode ter insights interessantes também relacionados aos motivos da demissão em si
ggplot(bd_rh) + geom_bar(aes(x = Attrition))

# Contagem por Departamento
# Neste gráfico vemos que a maioria dos profissionais pertecem a área de pesquisa e desenvolvimento
# O que indica que temos na análise uma área que tende a ser muito estressante dentro da empresa
# Historicamente a área de Pesquisa e Desenvolvimento é muito cobrada por resultados
ggplot(bd_rh) + geom_bar(aes(x = Department))

# Contagem por Cargo
# A maioria dos profissionais é executivo de vendas seguido de pesquisadores
# Geralmente são cargos que possuem muita cobranças e prazos curtos de entrega, o que implica em stress
ggplot(bd_rh) + geom_bar(aes(x = JobRole))


ggplot(bd_rh) + geom_bar(aes(x = Education)) + facet_grid(~EducationField)

# Multiplot Grid
p.TotalWorkingYears       <- ggplot(bd_rh) + geom_density(aes(TotalWorkingYears))
p.YearsAtCompany          <- ggplot(bd_rh) + geom_density(aes(YearsAtCompany))
p.YearsSinceLastPromotion <- ggplot(bd_rh) + geom_density(aes(YearsSinceLastPromotion))
p.YearsWithCurrManager    <- ggplot(bd_rh) + geom_density(aes(YearsWithCurrManager))
p.YearsInCurrentRole      <- ggplot(bd_rh) + geom_density(aes(YearsInCurrentRole))
p.PriorYearsOfExperience  <- ggplot(bd_rh) + geom_density(aes(PriorYearsOfExperience))
