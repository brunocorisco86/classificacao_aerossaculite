# Definir Diretorio Padrão ----

setwd('C:/Users/user/Code/R/AERO')

# Instalar bibliotecas ----
install.packages("googlesheets4")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("stargazer")
install.packages("visreg")
install.packages("dplyr")
install.packages("jtools")
install.packages("ggstance")
install.packages("lmtest")
install.packages('DAAG')
install.packages("gvlma")
install.packages("corrplot")
install.packages("Hmisc")
install.packages("caret")


# bibliotecas -----
library(googlesheets4)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(ggplot)
library(visreg)
library(MASS)
library(jtools)
library(ggstance)
library(esquisse)
library(readxl)
library(lmtest)
library(DAAG)
library(gvlma)
library(Hmisc)
library(ggfortify)
library(corrplot)
library(caret)


# Import data ----

#data_aero <- read_sheet("https://docs.google.com/spreadsheets/d/1RBjMLGR9SW7pfzn55vJC8y096uFTUOk3cMlQ3kZGB_Q/edit?usp=sharing",
#                        sheet="Página1")  

data_aero<- read_excel("C:\\Users\\user\\Code\\R\\AERO\\dados2.xlsx", 
                        sheet="Planilha1")



data_aero2 <- drop_na(data_aero)

view(data_aero2)
# sample(data_aero, 5)

Q1 <- quantile(data_aero2$logaero, 0.25)
Q3 <- quantile(data_aero2$logaero, 0.75)
IQR <- Q3 - Q1
limite_inf <- Q1 - 1.5 * IQR
limite_sup <- Q3 + 1.5 * IQR
data_aero3 <- data_aero2 %>% filter(logaero >= limite_inf, logaero <= limite_sup)

Q1 <- quantile(data_aero3$logmort, 0.25)
Q3 <- quantile(data_aero3$logmort, 0.75)
IQR <- Q3 - Q1
limite_inf <- Q1 - 1.5 * IQR
limite_sup <- Q3 + 1.5 * IQR
data_aero5 <- data_aero3 %>% filter(logmort >= limite_inf, logmort <= limite_sup)

view(data_aero5)
boxplot(data_aero5$logaero)
max(data_aero5$logaero)

#dar um drop na coluna aero
data_aero5 <- subset( data_aero5, select = -aero )

#View(data_aero)
View(data_aero5)

esquisser(viewer = "browser", data_aero5)
library(ggplot2)
ggplot(data_aero5, aes(x=logmort, y=logaero)) +
  xlab('%mort recente')+
  ylab('condenação aero em %')+
  geom_point() +
  geom_smooth(method = 'lm', colour='red')

hist(data_aero5$logaero)

logaero<- log10(data_aero5$aero)
view(logaero)
hist(logaero)


# Matriz de Correlação ----

m<- cor(data_aero5)
?corrplot
corrplot(m)
corrplot(m, method="color") #cores
corrplot(m, method = 'circle', order="alphabet", diag = TRUE)
corrplot(m, order = 'AOE') # after 'AOE' reorder
corrplot(m, method="circle") #cores
corrplot(m, method="ellipse") #elípses
corrplot(m, method="shade") # tons
corrplot(m, method="number") #números
corrplot(m, order = 'hclust', method="circle",addrect = 5,  diag=TRUE)
corrplot(m,method='square', type="upper")
corrplot(m,type="lower",method = "number")

view(data_aero5)
regressao0<- lm(formula = logaero ~ . -1, data=data_aero5)
visreg(regressao0)
summary(regressao0)
anova(regressao0)
autoplot(regressao0, which = 1:6, ncol = 3, label.size = 3)
plot_summs(regressao0)

testRes = cor.mtest(data_aero5, conf.level = 0.95)
View(testRes)
#view(testRes)
corrplot(m, p.mat = testRes$p,method="square", sig.level = 0.05, order = 'AOE', addrect = 3)



# Testes de normalidade ----

# dados originais

shapiro.test(data_aero5$logaero)
hist(data_aero5$logaero, col='steelblue', main='logaero')

# logaritmic transformation
log_aero<- log10(data_aero5$aero)
view(log_aero)
hist(log_aero, col='coral2', main='%Aero - Log Transformed')
shapiro.test(log_aero)

# square root transformation
sqrt_aero<- sqrt(data_aero5$aero)
shapiro.test(sqrt_aero)
hist(sqrt_aero, col='coral2', main='Sqrt Transformed')



# Estimação ----

View(data_aero5)

set.seed(500)
fit <- lm(formula =  logaero ~ . - 1, data = data_aero5)

# fit com todas as variaveis com stepwise

step(fit, scale=0, trace = 1, direction="both")

regressao1<- lm(formula = logaero ~ g1+ g2 + g3 + g4 + a08 + d01 + d03 + d04 + f01 + f04 + f06 + c06 + i03 + logmort - 1, data = data_aero5)
autoplot(regressao1, which = 1:6, ncol = 3, label.size = 3)
plot(regressao1$residuals)
summary(regressao1)
round(summary(regressao1)$coef, 6)

library(lmtest)
library(car)
qqPlot(regressao1, main = "Q-Q Plot da Regressão 2")
plot(
  regressao1$fitted.values,
  resid(
    regressao1,
    xlab = "Valores Ajustados",
    ylab = "Resíduos",
    main = "Homoscedasticidade"
  )
)
dwtest(regressao1)

fit2 <- lm(regressao1, data = data_aero5)
step(fit2, scale=0, trace = 1, direction="backward")
plot(regressao1$residuals)
summary(regressao1)
anova(regressao1)

regressao2<- lm(formula = logaero ~ g1 + g2 + g3 + g4 + a08 + d01 + d03 + d04 + f01 + 
                  f04 + f06 + c06 + i12 + logmort - 1, data = data_aero5)
autoplot(regressao2, which = 1:6, ncol = 3, label.size = 3)
anova(regressao2)
summary(regressao2)
qqPlot(regressao2, main = "Q-Q Plot da Regressao 2")

# fit com composite score de Random Trees 
fit3 <- lm(formula = logaero ~ logmort + y06 + x02 + c15 + g4 + i12 + 
             d03 + d04 + f15 + g3 + d01 + a08 + f06 + g2 + g1 - 1, data = data_aero5)
step(fit3, scale=0, trace = 1, direction="both")

regressao3<-lm(formula = logaero ~ logmort + y06 + x02 + c15 + g4 + i12 + 
                 d03 + d04 + f15 + g3 + d01 + a08 + f06 + g2 + g1 - 1, data = data_aero5)
autoplot(regressao3, which = 1:6, ncol = 3, label.size = 3)
anova(regressao3)

summary(regressao3)
qqPlot(regressao3, main = "Q-Q Plot da Regressao 3")

# fit com %IncMSE

fit4 <- lm(formula = logaero ~ logmort + y06 + x02 + c15 + d03 + g4 + i12 + d04 + d01 + f15 +
           f03 +  g3 + a08 + g1 + f06 + i09 + g2 + a09 + d05 + f05 + f04
           - 1, data = data_aero5)
step(fit4, scale=0, trace = 1, direction="both")


regressao4<-lm(formula = logaero ~ logmort + y06 + x02 + c15 + d03 + g4 + 
                 d04 + d01 + f15 + g3 + a08 + f06 + i09 + a09 + f05 + g1 + g2 - 1, data = data_aero5)
autoplot(regressao4, which = 1:6, ncol = 3, label.size = 3)
plot(regressao4$residuals)

summary(regressao4)
qqPlot(regressao4, main = "Q-Q Plot da Regressão 4")

# fit com IncNode Purity
fit5 <- lm(formula = logaero ~ logmort + y06 + x02 + c15 + i12 + g4 + d03 + g1 + g3 + g2 +
           f15 + d04 + a08 + d01 + c06 + f06 + a09 + y01 + f04 + f05
           - 1, data = data_aero5)
step(fit5, scale=0, trace = 1, direction="both")


regressao5<-lm(formula = logaero ~ logmort + y06 + x02 + c15 + g4 + d03 + 
                 g3 + f15 + d04 + a08 + d01 + f06 + a09 + y01 + f05 + g1 + g2 - 1, 
                data = data_aero5)
autoplot(regressao5, which = 1:6, ncol = 3, label.size = 3)
plot(regressao5$residuals)

summary(regressao5)

# Análise ----

attributes(regressao1)

fitted.values(regressao1)
plot(regressao3$residuals)
residuals(regressao3)
summary(regressao1)
regressao1$coefficients
round(summary(regressao1)$coef, 6)


autoplot(regressao1, which = 1:6, ncol = 3, label.size = 3)

stargazer(regressao1,regressao2,regressao3,regressao4,regressao5, type="text", ci=TRUE,  ci.level=0.95,
       column.labels=c("reg1","reg2","reg3","reg4","reg5"))

summary(regressao4)
anova(regressao4)
plot(regressao1)
visreg(regressao1)
coefficients(regressao1)
plot(regressao1)

# Breusch-Pagan Test ----

bptest(regressao1, studentize=TRUE)
bptest(regressao2, studentize=TRUE)
bptest(regressao3, studentize=TRUE)
bptest(regressao4, studentize=TRUE)
bptest(regressao5, studentize=TRUE)

gvlma(regressao1)
gvlma(regressao2)
gvlma(regressao3)
gvlma(regressao4)
gvlma(regressao5)

library(sandwich)
library(lmtest)
# Uso de erros robustos

coeftest(regressao1, vcov = vcovHC(regressao1, type = "HC1"))
coeftest(regressao2, vcov = vcovHC(regressao2, type = "HC1"))
coeftest(regressao3, vcov = vcovHC(regressao3, type = "HC1"))
coeftest(regressao4, vcov = vcovHC(regressao4, type = "HC1"))
coeftest(regressao5, vcov = vcovHC(regressao5, type = "HC1"))



plot_coefs(regressao1, regressao2, model.names = c("regressao1", "regressao2"), ci_level = 0.95)
plot_summs(regressao2, inner_ci_level = 0.95, plot.distributions = TRUE)
plot_summs(regressao1, regressao2, model.names = c("regressao1", "regressao2"), plot.distributions = TRUE, inner_ci_level = .95)
plot_summs(regressao1, regressao2)

# Cross validation ----
view(data_aero5)
set.seed(800)
data_shuffle<- data_aero5[sample(1:nrow(data_aero5)), ]

view(data_shuffle)

cv.lm(data_shuffle, regressao1, dots= TRUE, seed = 500,  m=2, plotit = TRUE)
cv.lm(data_shuffle, regressao2, dots= TRUE, seed = 500,  m=2, plotit = TRUE)
cv.lm(data_shuffle, regressao3, dots= TRUE, seed = 500,  m=2, plotit = TRUE)
cv.lm(data_shuffle, regressao4, dots= TRUE, seed = 500,  m=2, plotit = TRUE)


summary(regressao1)
summary(regressao2)
summary(regressao3)
summary(regressao4)

plot(regressao1$residuals)
plot(regressao2$residuals)
plot(regressao3$residuals)
plot(regressao4$residuals)
plot(regressao5$residuals)


residuos1 <- residuals(regressao1)
mse1 <- mean(residuos1^2)
print(paste("MSE dos Resíduos:", mse1))

residuos2 <- residuals(regressao2)
mse2 <- mean(residuos2^2)
print(paste("MSE dos Resíduos:", mse2))

residuos3 <- residuals(regressao3)
mse3 <- mean(residuos3^2)
print(paste("MSE dos Resíduos:", mse3))

residuos4 <- residuals(regressao4)
mse4 <- mean(residuos4^2)
print(paste("MSE dos Resíduos:", mse4))

round(summary(regressao1)$coef, 6)
round(summary(regressao2)$coef, 6)
round(summary(regressao3)$coef, 6)
round(summary(regressao4)$coef, 6)

# Instalar e carregar o pacote necessário (Desision TREES) ----
if (!require("rpart")) install.packages("rpart")
library(rpart)

# Ajustar o modelo de árvore de decisão
modelo_arvore <- rpart(logaero ~ ., data = data_aero5, method = "anova")

# Visualizar os resultados da árvore
print(modelo_arvore)
summary(modelo_arvore)

# Plotar a árvore de decisão
plot(modelo_arvore, uniform = TRUE, main = "Árvore de Decisão para Estimar logaero")
text(modelo_arvore, use.n = TRUE, all = TRUE, cex = 0.8)

# Fazer previsões usando o modelo
previsoes <- predict(modelo_arvore, newdata = data_aero5)

# Mostrar as previsões
head(previsoes)

# Instalar e carregar o pacote necessário (Ramdom Forest) ----
if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

# Ajustar o modelo de Random Forest
set.seed(800)  # Definir uma semente para reprodutibilidade
modelo_rf <- randomForest(logaero ~ . -1, data = data_aero5, ntree = 500, importance = TRUE)


# Visualizar os resultados do modelo
print(modelo_rf)
summary(modelo_rf)

# Mostrar a importância das variáveis
importance(modelo_rf)
varImpPlot(modelo_rf)

# Fazer previsões usando o modelo
previsoes_rf <- predict(modelo_rf, newdata = data_aero5)

# Mostrar as previsões
head(previsoes_rf)

# Assumindo que 'modelo_rf' é o seu modelo Random Forest ajustado

# Obter a importância das variáveis
importancia <- importance(modelo_rf)

# Normalizar %IncMSE e IncNodePurity para o intervalo [0, 1]
norm_IncMSE <- (importancia[, "%IncMSE"] - min(importancia[, "%IncMSE"])) / 
  (max(importancia[, "%IncMSE"]) - min(importancia[, "%IncMSE"]))
norm_IncNodePurity <- (importancia[, "IncNodePurity"] - min(importancia[, "IncNodePurity"])) / 
  (max(importancia[, "IncNodePurity"]) - min(importancia[, "IncNodePurity"]))

# Combinar as métricas (média simples)
composite_score <- (norm_IncMSE + norm_IncNodePurity) / 2

# Adicionar o score ao dataframe de importância
importancia <- cbind(importancia, Composite_Score = composite_score)

# Visualizar as variáveis com base no Composite Score
importancia[order(importancia[, "Composite_Score"], decreasing = TRUE), ]


# Instalar e carregar o pacote necessário (definir variaveis importantes por random forest) ----
if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

# Ajustar o modelo de Random Forest
set.seed(800)
modelo_rf <- randomForest(logaero ~ ., data = data_aero5, ntree = 500, importance = TRUE)

# Exibir a importância das variáveis
importancia <- importance(modelo_rf)

# Ver a importância das variáveis em termos de %IncMSE e IncNodePurity
print(importancia)

# Ordenar as variáveis por %IncMSE
importancia_ord_mse <- importancia[order(importancia[, "%IncMSE"], decreasing = TRUE), ]
print("Variáveis ordenadas por %IncMSE:")
print(importancia_ord_mse)

# Ordenar as variáveis por IncNodePurity
importancia_ord_purity <- importancia[order(importancia[, "IncNodePurity"], decreasing = TRUE), ]
print("Variáveis ordenadas por IncNodePurity:")
print(importancia_ord_purity)

# Plotar a importância das variáveis por %IncMSE
varImpPlot(modelo_rf, type = 1, main = "%IncMSE - Importância das Variáveis")

# Plotar a importância das variáveis por IncNodePurity
varImpPlot(modelo_rf, type = 2, main = "IncNodePurity - Importância das Variáveis")



# Instalar e carregar os pacotes necessários (KNN) ----
if (!require("caret")) install.packages("caret")
if (!require("class")) install.packages("class")
library(caret)
library(class)

# Definir uma semente para reprodutibilidade
set.seed(8000)

# Dividir o dataset em treino e teste
index <- createDataPartition(data_aero5$logaero, p = 0.7, list = FALSE)
train_data <- data_aero5[index, ]
test_data <- data_aero5[-index, ]

# Escalar as variáveis preditoras (opcional, mas recomendado para KNN)
train_scaled <- scale(train_data[, -which(names(train_data) == "logaero")])
test_scaled <- scale(test_data[, -which(names(test_data) == "logaero")])

# Ajustar o modelo KNN
knn_model <- knn(train = train_scaled, 
                 test = test_scaled, 
                 cl = train_data$logaero, 
                 k = 5)

# Avaliar o modelo
results <- data.frame(Predicted = knn_model, Actual = test_data$logaero)
print(results)

# Calcular o erro médio quadrático (RMSE)
rmse <- sqrt(mean((as.numeric(knn_model) - test_data$logaero)^2))
print(paste("RMSE:", rmse))

# Criar um scatter plot das previsões versus valores reais
plot(test_data$logaero, as.numeric(knn_model),
     xlab = "Valores Reais de logaero",
     ylab = "Previsões de logaero",
     main = "Previsões vs. Valores Reais - KNN",
     pch = 19, col = "blue")
abline(0, 1, col = "red")  # Adicionar uma linha y=x para referência

# Calcular os resíduos
residuos <- as.numeric(knn_model) - test_data$logaero

# Criar um plot de resíduos
plot(test_data$logaero, residuos,
     xlab = "Valores Reais de logaero",
     ylab = "Resíduos",
     main = "Residual Plot - KNN",
     pch = 19, col = "purple")
abline(h = 0, col = "red")  # Adicionar uma linha horizontal em 0

# Calcular a matriz de correlação (correlacao) ---- 
cor_matrix <- cor(data_aero5[, sapply(data_aero5, is.numeric)])

# Visualizar a matriz de correlação
print(cor_matrix)

# Identificar pares de variáveis altamente correlacionadas
# (por exemplo, correlação maior que 0.9)
high_cor <- which(abs(cor_matrix) > 0.9, arr.ind = TRUE)
print(high_cor)

# Instalar e carregar o pacote necessário (Feature Selection (Seleção de Características)) ----
if (!require("caret")) install.packages("caret")
library(caret)

# Definir um controle para a seleção de variáveis
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Executar a seleção de variáveis
result <- rfe(data_aero5[, -which(names(data_aero5) == "logaero")],
              data_aero5$logaero,
              sizes = c(1:5),
              rfeControl = ctrl)

# Ver as variáveis selecionadas
print(result$optVariables)


