![WeWork_PrivateOffice-1440x810](https://user-images.githubusercontent.com/91103250/215613322-1dfc6554-270f-4b02-bf7a-7d6cd7e255bd.jpg)

# Modelo Preditivo - RH Analytics
Essa é um proposta de análise preditiva com base no dataset RH Analytics da IBM. Com base neste dataset, foi feita uma análise preditiva com base no atibuto que identifica se o colaborador pediu demissão ou continua empregado na empresa. Com essas informações, foi possível identificar os atributos da base que possuem alguma correlação com a variável preditora.

## O que são as análises de People Analytics
People analytics é um processo de coleta e análise de dados voltado para a gestão de pessoas em empresas. O conceito nasce a partir da ideia de big data, que consiste na coleta, armazenamento e análise de um volume imenso de dados. Este tipo de análise permite contratações mais alinhadas com as políticas da empresa, identifica falhas nos processos de RH, renteção de talentos e etc.

Fonte dos dados: https://developer.ibm.com/patterns/data-science-life-cycle-in-action-to-solve-employee-attrition-problem/

## Dicionário de Dados
Crédito: @denistanjingyu

| Variável  | Descrição | Métrica
| ------------- | ------------- | ------------- |
| Age  | Idade do colaborador  |   |
| AgeStartedWorking | Idade na qual o colaborador começou a trabalhar  | (Age - TotalWorkingYears)  |
| Attrition  | Status do colaborador(Preditora) | Current Employee / Voluntary Resignation / Termination  |
| AverageTenure  | Tempo médio de permanência no trabalho  | (PriorYearsOfExperience / NumCompaniesWorked)  |
| BusinessTravel  | Indica viagens internacionais a trabalho  | Travel Frequently / Travel_Rarely / Non_Travel  |
| Department  | O departamento que o empregado atualmente trabalha para ou já trabalhou para no caso de encerrado/voluntário funcionários demitidos  | Human Resources / Research & Development / Sales  |
| DistanceFromHome  | Distância em KM entre trabalho e casa  |   |
| Education  | Nível de escolaridade  | 1: Below college 2: College 3: Bachelor 4: Master 5: Doctor  |
| EducationField  | Área de Atuação  | Human Resources / LifeSciences / Marketing / Medical / Technical Degree / Other  |
| Gender  | Gênero  | Female / Male  |
| JobRole  | Cargo  | Healthcare Representative / Human Resources / Laboratory Technician / Manager / Manufacturing Director / Research Director / Research Scientist / Sales Executive / Sales Representative  |
| MaritalStatus  | Estado civil  | Content Cell  |
| NumCompaniesWorked  | Número total de empresas o colaborador trabalhou para antes de seu trabalho atual  |   |
| PriorYearsOfExperience  | Número de anos de trabalho experiência antes do trabalho atual.  | TotalWorkingYears - YearsAtCompany  |
| TotalWorkingYears  | Ttoal de anos de experiência  |  |
| TrainingTimesLastYear  | Número de trabalho relacionado treinamentos frequentados pelo empregado no ano passado  |   |
| YearsAtCompany  | Total de anos na empresa atual |   |
| YearsInCurrentRole  | Total de ano no cargo atual  |   |
| YearsSinceLastPromotion  | Total de anos desde a última promoção  |   |
| YearsWithCurrManager  | Total de anos com o atual líder  |   |
| Employee.Source  | Fonte de recrutamento do colaborador  | Referral / Company Website / Seek / LinkedIn / Adzuna / Indeed / Glassdoor / Jora / Recruit.net  |
| DailyRate  | Taxa de pagamento por dia  |   |
| HourlyRate  | Taxa de pagamento por hora  |   |
| MonthlyIncome  | Salário mensal  |   |
| MonthlyRate  | Taxa de pagamento por Mês  |   |
| OverTime  | Indicador se houve Hora-Extra  | Yes/No  |
| PercentSalaryHike  | Percentual de aumento do salário comparado ao ano anterior  |   |
| StandardHours  | Horas trabalhadas de acordo com o contrato  |   |
| StockOptionLevel  | Proporção da renda que o colaborador gasta na compra de ações da empresa.  | 0-3 (0 indica que o funcionário não comprou as ações da empresa, um número maior significa uma proporção maior)  |
| EnvironmentSatisfaction  | Grau em que o colaborador está satisfeito com o ambiente de trabalho.  | 1-4 (um número maior indica maior satisfação)  |
| JobInvolvement  | Grau em que o colaborador se identifica com o seu trabalho.  | 1-4 (um número maior indica maior satisfação)  |
| JobLevel  | A avaliação do colaborador sobre a dificuldade de seu trabalho.  | 1-4 (um número maior indica maior satisfação)  |
| JobSatisfaction  | Grau de satisfação com o trabalho  | 1-4 (um número maior indica maior satisfação)  |
| PerformanceRating  | Nota dada ao colaborador por seu líder com base em seu desempenho no trabalho  | 1-4 (um número maior indica maior satisfação)  |
| RelationshipSatisfaction  | Grau de satisfação com o ambiente de trabalho  | 1-4 (um número maior indica maior satisfação)  |
| WorkLifeBalance  |Grau de satisfação para o work-life balance | 1-4 (um número maior indica maior satisfação)  |
