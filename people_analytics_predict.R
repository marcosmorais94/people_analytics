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

# Análise por Formação Acadêmica
# Neste gráfico vemos que a formação de Life Sciences, que engloba biotecnologia por exemplo, é a maior entre as demais
# Outro ponto importante é que a área médica tem muitos representantes na base e formação técnica possui um volume considerável de pessoas também.
ggplot(bd_rh) + geom_bar(aes(x = Education)) + facet_grid(~EducationField)

# Multiplot Grid
# Aqui vamos plotar uma série de gráfico sobre atributos relacionados ao tempo,
# como anos de experiência e tempo com o mesmo gestor
p.TotalWorkingYears       <- ggplot(bd_rh) + geom_density(aes(TotalWorkingYears))
p.YearsAtCompany          <- ggplot(bd_rh) + geom_density(aes(YearsAtCompany))
p.YearsSinceLastPromotion <- ggplot(bd_rh) + geom_density(aes(YearsSinceLastPromotion))
p.YearsWithCurrManager    <- ggplot(bd_rh) + geom_density(aes(YearsWithCurrManager))
p.YearsInCurrentRole      <- ggplot(bd_rh) + geom_density(aes(YearsInCurrentRole))
p.PriorYearsOfExperience  <- ggplot(bd_rh) + geom_density(aes(PriorYearsOfExperience))


# Organiza no grid
grid.arrange(p.TotalWorkingYears, 
             p.YearsAtCompany, 
             p.YearsSinceLastPromotion, 
             p.YearsWithCurrManager, 
             p.YearsInCurrentRole, 
             p.PriorYearsOfExperience, 
             nrow = 2, 
             ncol = 3)
# Alguns dados interessantes são que a medida que os gestores mudam, os cargo dos profissionais também mudam.
# Outro detalhe que chama a atenção são que temos um pico na casa dos 10 anos de trabalho na empresa, após
# esse tempo vemos uma queda que se mantém ao longo do período.

# Tempo de experiência anterior
# Vamos descobrir a proporção de funcionários com menos de alguns anos de experiência 
# (valores escolhidos: 1, 3, 5, 7, 10 anos)
length(which(bd_rh$PriorYearsOfExperience < 1)) / length(bd_rh$PriorYearsOfExperience)  
length(which(bd_rh$PriorYearsOfExperience < 3)) / length(bd_rh$PriorYearsOfExperience)   
length(which(bd_rh$PriorYearsOfExperience < 5)) / length(bd_rh$PriorYearsOfExperience)   
length(which(bd_rh$PriorYearsOfExperience < 7)) / length(bd_rh$PriorYearsOfExperience)   
length(which(bd_rh$PriorYearsOfExperience < 10)) / length(bd_rh$PriorYearsOfExperience)  

# 58% dos funcionários têm menos de 3 anos de experiência de trabalho antes de entrar na IBM
# Possíveis problemas: conjuntos de habilidades subdesenvolvidos, base de jovens funcionários, 
# mentalidade de "trabalho" imatura.

# Idade
# Apenas 22% dos funcionários têm menos de 30 anos, a base de funcionários não é exatamente 
# tão jovem como o esperado.
length(which(bd_rh$Age < 30)) / length(bd_rh$Age)

# # Educação
summary(dados_rh$Education)
length(which(bd_rh$Education == 3)) / length(bd_rh$Education)
length(which(bd_rh$Education == 4)) / length(bd_rh$Education)

# Cerca de 39% dos funcionários são graduados e 27% realizaram o mestrado.
# A busca pelo ensino superior pode ter levado a uma diminuição da experiência de trabalho.

# Verificando a diferença salarial entre homens e mulheres.
ggplot(data = subset(bd_rh, !is.na(Gender)), aes(Gender, MonthlyIncome, fill = Gender)) +
  geom_boxplot() + 
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 10)) +
  labs(x = "Gender", y = "Monthly Income", title = "Salário Mensal Entre Gêneros") +
  coord_flip()

# As mulheres ganham um pouco mais, em média, desconsiderando todos os outros fatores.

# Função
ggplot(data = subset(bd_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, MonthlyIncome)) +
  ggtitle("Salário Mensal Por Função")

ggplot(data = subset(bd_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, AgeStartedWorking)) +
  ggtitle("Idade Que Iniciou na Função")

ggplot(data = subset(bd_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, Age)) +
  ggtitle("Idade Por Função")

ggplot(data = subset(bd_rh, !is.na(JobRole))) + geom_boxplot(aes(JobRole, YearsAtCompany)) +
  ggtitle("Tempo de Empresa (em anos)")

ggplot(data = na.omit(bd_rh)) + geom_bar(aes(JobRole, fill = Education), position = "fill") +
  ggtitle("Nível de Educação Por Função") + 
  ylab("Proportion")

##### 7 - Modelagem Preditiva ##### 

# Vamos concentrar nosso trabalho em tentar ajudar o RH a recrutar melhor visando evitar atritos 
# e, consequentemente, demissões.

# Criaremos 5 versões do modelo e para cada um vamos explorar as opções e interpretar o resultado.

# Primeira versão do modelo com algumas variáveis
modelo_v1 <- glm(Attrition ~ Age + Department + DistanceFromHome + Employee.Source + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender + 
                   Education + EducationField, 
                 family = binomial, 
                 data = bd_rh)

summary(modelo_v1)
?vif
vif(modelo_v1)

# Vamos dividir os dados em treino e teste. Vamos trabalhar com os dados sem registros de demitidos.
set.seed(2004)
index_treino <- sample.split(Y = dados_rh_1$Attrition, SplitRatio = 0.7)
dados_rh_1_treino <- subset(dados_rh_1, train = T)
dados_rh_1_teste <- subset(dados_rh_1, train = F)

# Segunda versão do modelo com dados de treino
modelo_v2 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender + 
                   Education + EducationField, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v2)
vif(modelo_v2)

# Previsões
threshold <- 0.5
previsoes_v2 <- predict(modelo_v2, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v2 <- ifelse(previsoes_v2 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v2)

# Terceira versão do modelo com dados de treino e sem variáveis de educação
modelo_v3 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience + Gender, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v3)
vif(modelo_v3)

# Previsões
threshold <- 0.5
previsoes_v3 <- predict(modelo_v3, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v3 <- ifelse(previsoes_v3 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v3)

# Quarta versão do modelo com dados de treino e sem variáveis de educação e genero
modelo_v4 <- glm(Attrition ~ Age + Department + DistanceFromHome + `Employee Source` + 
                   JobRole + MaritalStatus + AverageTenure + PriorYearsOfExperience, 
                 family = binomial, 
                 data = dados_rh_1_treino)

summary(modelo_v4)
vif(modelo_v4)

# Previsões
threshold <- 0.5
previsoes_v4 <- predict(modelo_v4, type = 'response', newdata = dados_rh_1_teste)
previsoes_finais_v4 <- ifelse(previsoes_v4 > threshold, 'Voluntary Resignation', 'Current employee')
table(dados_rh_1_teste$Attrition, previsoes_finais_v4)

# Quinta versão do modelo com dados de treino e sem variáveis de educação, genero e outro algoritmo
?rpart
modelo_v5 <- rpart(Attrition ~ Age + Department + DistanceFromHome + JobRole + MaritalStatus + 
                     AverageTenure + PriorYearsOfExperience, 
                   method = "class", 
                   control = rpart.control(minsplit = 500, cp = 0),
                   data = dados_rh_1_treino)

summary(modelo_v5)
rpart.plot(modelo_v5)