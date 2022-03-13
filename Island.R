library(readr)
archive <- read_csv("C:/Users/bernc/Downloads/Islander_data.csv")
View(archive)

dados <- as.data.frame(c(archive[2:7],archive[9]))
#View(dados)
names(dados)



#VARIAVEIS CATEGORICAS


#LAST NAME
#lastname<- rep(6,198)
data1 = dados
data1["last_name"][data1["last_name"] == "Carrasco"] <- 0
data1["last_name"][data1["last_name"] == "Durand"] <- 0
data1["last_name"][data1["last_name"] == "Bernard"] <- 1
data1["last_name"][data1["last_name"] == "Castro"] <- 1
data1["last_name"][data1["last_name"] == "Connolly"] <- 1
data1["last_name"][data1["last_name"] == "Fiala"] <- 1
data1["last_name"][data1["last_name"] == "Gonzalez"] <- 1
data1["last_name"][data1["last_name"] == "Hajek"] <- 1
data1["last_name"][data1["last_name"] == "Kennedy"] <- 1
data1["last_name"][data1["last_name"] == "Lopez"] <- 1
data1["last_name"][data1["last_name"] == "McCarthy"] <- 1
data1["last_name"][data1["last_name"] == "Morin"] <- 1
data1["last_name"][data1["last_name"] == "Novak"] <- 1
data1["last_name"][data1["last_name"] == "Price"] <- 1
data1["last_name"][data1["last_name"] == "Rodriguez"] <- 1
data1["last_name"][data1["last_name"] == "Steiner"] <- 1
data1["last_name"][data1["last_name"] == "Summers"] <- 1
data1["last_name"][data1["last_name"] == "Takahashi"] <- 1
data1$last_name


data1["Happy_Sad_group"][data1["Happy_Sad_group"] == "H"] <- 0
data1["Happy_Sad_group"][data1["Happy_Sad_group"] == "S"] <- 1
data1$Happy_Sad_group


data1["Dosage"][data1["Dosage"] == "1"] <- 0
data1["Dosage"][data1["Dosage"] == "2"] <- 1
data1["Dosage"][data1["Dosage"] == "3"] <- 2
data1$Dosage

data1["Drug"][data1["Drug"] == "S"] <- 0
data1["Drug"][data1["Drug"] == "T"] <- 1
data1["Drug"][data1["Drug"] == "A"] <- 2
data1$Drug

#lastname[(dados$last_name == 'Carrasco')] <- 0
#lastname[(dados$last_name == 'Durand')] <- 0
#lastname[(last_name!=0)] <- 1
#lastname

sapply(data1, mode)
#data1$last_name = as.numeric(as.character(data1$last_name))
#data1$Happy_Sad_group = as.numeric(as.character(data1$Happy_Sad_group))
#data1$Drug = as.numeric(as.character(data1$Drug))
#sapply(data1, mode)
#data1$last_name


table(data1$last_name)
barplot(table(data1$last_name),
        names.arg = c('0','1'),
        xlab = 'last name',
        ylab = 'Frequência')



library(tidyverse)

dados = data1
ggplot(dados, aes(x=age, y=Diff, color=as.factor(dados$last_name))) + geom_point(size=2)
ggplot(dados, aes(x=Dosage, y=Diff, color=as.factor(dados$last_name))) + geom_point(size=2)
ggplot(dados, aes(x=Happy_Sad_group, y=Diff, color=as.factor(dados$last_name))) + geom_point(size=2)
ggplot(dados, aes(x=Drug, y=Diff, color=as.factor(dados$last_name))) + geom_point(size=2)
ggplot(dados, aes(x=Mem_Score_Before, y=Diff, color=as.factor(dados$last_name))) + geom_point(size=2)
#não parece haver interação entre o last name e as restantes variáveis preditoras


#HAPPY_SAD
table(dados$Happy_Sad_group)
barplot(table(dados$Happy_Sad_group),
        names.arg = c('S','H'),
        xlab = 'happy and sad',
        ylab = 'Frequência')

ggplot(dados, aes(x=age, y=Diff, color=as.factor(dados$Happy_Sad_group))) + geom_point(size=2)
ggplot(dados, aes(x=Dosage, y=Diff, color=as.factor(dados$Happy_Sad_group))) + geom_point(size=2)
ggplot(dados, aes(x=Drug, y=Diff, color=as.factor(dados$Happy_Sad_group))) + geom_point(size=2)
ggplot(dados, aes(x=Mem_Score_Before, y=Diff, color=as.factor(dados$Happy_Sad_group))) + geom_point(size=2)
ggplot(dados, aes(x=last_name, y=Diff, color=as.factor(dados$Happy_Sad_group))) + geom_point(size=2)
#a variável  Happy_sad não parece interagir com as restantes variaveis preditoras



#Dosage
table(dados$Dosage)
barplot(table(dados$Dosage),
        xlab = 'dosage',
        ylab = 'Frequência')

ggplot(dados, aes(x=age, y=Diff, color=as.factor(Dosage))) + geom_point(size=2)
ggplot(dados, aes(x=Drug, y=Diff, color=as.factor(Dosage))) + geom_point(size=2)
ggplot(dados, aes(x=Mem_Score_Before, y=Diff, color=as.factor(Dosage))) + geom_point(size=2)
ggplot(dados, aes(x=Happy_Sad_group, y=Diff, color=as.factor(Dosage))) + geom_point(size=2)
ggplot(dados, aes(x=last_name, y=Diff, color=as.factor(Dosage))) + geom_point(size=2)

#Correlação entre diff e dosagem?


#Drug
table(dados$Drug)
barplot(table(dados$Drug),
        xlab = 'drug',
        ylab = 'Frequência')
ggplot(dados, aes(x=age, y=Diff, color=as.factor(Drug))) + geom_point(size=2)
ggplot(dados, aes(x=Dosage, y=Diff, color=as.factor(Drug))) + geom_point(size=2)
ggplot(dados, aes(x=Happy_Sad_group, y=Diff, color=as.factor(Drug))) + geom_point(size=2)
ggplot(dados, aes(x=Mem_Score_Before, y=Diff, color=as.factor(Drug))) + geom_point(size=2)
ggplot(dados, aes(x=last_name, y=Diff, color=as.factor(Drug))) + geom_point(size=2)
#A tem maior diff


par(mfrow=c(1,1))
plot(as.factor(Drug),Diff)

par(mar = rep(2, 4))
par(mfrow=c(2,2))
barplot(table(data1$last_name),
        #names.arg = c('0','1'),
        xlab = 'last name',
        names.arg = c('Carrasco/Durand','Outros'),
        ylab = 'Frequência')
barplot(table(dados$Happy_Sad_group),
        xlab = 'happy and sad',
        names.arg = c('S','H'),
        ylab = 'Frequência')
barplot(table(dados$Drug),
        xlab = 'Drug',
        names.arg = c('S','T','A'),
        ylab = 'Frequência')
table(dados$Dosage)
barplot(table(dados$Dosage),
        xlab = 'dosage',
        ylab = 'Frequência')



l0 = length(last_name[(last_name == 0)])
l0
#print(length(l0))
n-l0
l0/n


l0 = length(Happy_Sad_group[(Happy_Sad_group == 0)])
l0
l0/n

#CONTINUAS
attach(data1)
par(mfrow=c(1,3))
hist(age,freq=F,xlab = expression('age'),ylab = 'Freq. relativa')
curve(dnorm(x,mean=mean(age),sd=sd(age)),add=TRUE, col='red')
qqnorm(age,xlab = "Quantis",
       ylab = expression('age'))
qqline(age,col='red')
boxplot(age, ylab = expression('age'))


par(mfrow=c(1,3))
hist(Mem_Score_Before, freq=F, xlab = expression('Mem. score before'),ylab = 'Freq. relativa')
curve(dnorm(x,mean=mean(Mem_Score_Before),sd=sd(Mem_Score_Before)),add=TRUE, col='red')
qqnorm(Mem_Score_Before,xlab = "Quantis",
       ylab = expression('Mem. score before'))
qqline(Mem_Score_Before,col='red')
boxplot(Mem_Score_Before, ylab = expression('Mem. score before'))


par(mfrow=c(1,3))
hist(Diff,freq=F,xlab = expression('Diff'),ylab = 'Freq. relativa')
curve(dnorm(x,mean=mean(Diff),sd=sd(Diff)),add=TRUE, col='red')
qqnorm(Diff,xlab = "Quantis",
       ylab = expression('Diff'))
qqline(Diff,col='red')
boxplot(Diff, ylab = expression('Diff'))


mean(Diff)
sd(Diff)
mean(age)
sd(age)
mean(Mem_Score_Before)
sd(Mem_Score_Before)


cor(dados$age,dados$Mem_Score_Before) #0,066
cor(dados$Diff,dados$age) #-0.0093
cor(dados$Diff,dados$Mem_Score_Before) #-0.104

modc <- lm(Diff ~ age + last_name + as.factor(Happy_Sad_group) + as.factor(Dosage) +Mem_Score_Before + as.factor(Drug), data=data1)
summary(modc)

modca <- lm(Diff ~ age + last_name + as.numeric(Happy_Sad_group) + as.numeric(Dosage) +Mem_Score_Before + as.numeric(Drug), data=data1)
summary(modca)

modc2 <- lm(Diff ~ age + last_name + Happy_Sad_group + Dosage + Mem_Score_Before + Drug, data=data1)
summary(modc2)
data1$last_name


library(QuantPsyc) 
lm.beta(modc2)

#não existe correlação entre variáveis contínuas

lm(scale(Diff) ~ scale(as.factor(Happy_Sad_group[(Happy_Sad_group=1)]), data=data1))


library(faraway)
halfnorm(modc2$residuals)




##Diagnóstico

# Diagrama de dispersão resíduos^2 vs. variável preditora
dados=data1
#age:
plot(dados$age, modc2$residuals^2,
     xlab="Idade (anos)",
     ylab="Resíduos^2 modelo completo")
abline(h=0, col="red")

#Mem_Score_Before:
plot(dados$Mem_Score_Before, modc2$residuals^2,
     xlab="Mem_Score_Before",
     ylab="Resíduos modelo completo")
abline(h=0, col="red")

#Resíduos vs resposta

plot(dados$Diff, modc2$residuals,
     xlab="Diff",
     ylab="Resíduos modelo completo")
abline(h=0, col="red")

#Correlação

df2 <- mutate_all(dados, function(x) as.numeric(as.character(x)))


cor(df2, method = "pearson")
df
dead <- df2$status[(df2$status > 0)]
length(dead)


###Stepwise selection


data22 = data1

library(tidyverse)
library(caret)
library(leaps)

#set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Diff ~ age + last_name + Happy_Sad_group + Dosage +Mem_Score_Before + Drug, data=data22,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:8), #max var. pred.
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 2)


###BACKWARD

train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(Diff ~ age + last_name + Happy_Sad_group + Dosage +Mem_Score_Before + Drug, data = data22,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:8), #max var. pred.
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 2)


###FORWARD

train.control <- trainControl(method = "cv", number = 100)
# Train the model
step.model <- train(Diff ~ age + last_name + Happy_Sad_group + Dosage +Mem_Score_Before + Drug, data = data22,
                    method = "leapForward", 
                    tuneGrid = data.frame(nvmax = 1:8), #max var. pred.
                    trControl = train.control
)
step.model$results
step.model$bestTune
summary(step.model$finalModel)
coef(step.model$finalModel, 3)





















lastname = data1$last_name
data1$last_name
#Ajuste linear

mod_completo <- lm(Diff ~ age + last_name + as.factor(Happy_Sad_group) + as.factor(Dosage) +Mem_Score_Before + as.factor(Drug))
summary(mod_completo)

plot(fitted.values(mod_completo),rstandard(mod_completo))
abline(h=0,col="red")
#existem valores maiores que 3.3

#tirar happy do mod completo
mod_s_happy <- lm(Diff ~ age + lastname + as.factor(Dosage) +Mem_Score_Before + as.factor(Drug),data=data1)
summary(mod_s_happy)
anova(mod_completo,mod_s_happy) #é mellhor tirar happy

plot(fitted.values(mod_s_happy),rstandard(mod_s_happy))
abline(h=0,col="red")

names(mod_s_happy)
#mod_s_happy$fitted.values

n = length(data1$age)
n
extractAIC(mod_completo,k=log(n))
extractAIC(mod_s_happy,k=log(n))

#tirar age 
mod_s_hap_age <- lm(Diff ~lastname + as.factor(Dosage) +Mem_Score_Before + as.factor(Drug),data=data1)
summary(mod_s_hap_age)
anova(mod_s_happy,mod_s_hap_age) #tirar age

plot(fitted.values(mod_s_hap_age),rstandard(mod_s_hap_age))
abline(h=0,col="red")

extractAIC(mod_s_happy,k=log(n))
extractAIC(mod_s_hap_age,k=log(n))



#tirar name
mod_s_han <- lm(Diff ~ as.factor(Dosage) +Mem_Score_Before + as.factor(Drug),data=data1)
summary(mod_s_han)
anova(mod_s_hap_age,mod_s_han)

plot(fitted.values(mod_s_han),rstandard(mod_s_han))
abline(h=0,col="red")

extractAIC(mod_s_hap_age,k=log(n))
extractAIC(mod_s_han,k=log(n))


par(mar = rep(2, 4))
par(mfrow=c(2,2))
plot(mod_s_han)
par(mfrow=c(1,1))


#tirar mem_score
mod_s_hanm <- lm(Diff ~ as.factor(Dosage) + as.factor(Drug),data=data1)
summary(mod_s_hanm)
anova(mod_s_han,mod_s_hanm)

plot(fitted.values(mod_s_hanm),rstandard(mod_s_hanm))
abline(h=0,col="red")

extractAIC(mod_s_han,k=log(n))
extractAIC(mod_s_hanm,k=log(n))


par(mar = rep(2, 4))
par(mfrow=c(2,2))
plot(mod_s_hanm)
par(mfrow=c(1,1))






#tirar last name do mod completo
mod_s_name <- lm(Diff ~ age + as.factor(Happy_Sad_group) + as.factor(Dosage) +Mem_Score_Before + as.factor(Drug))
summary(mod_s_name)
anova(mod_s_name,mod_completo) #tirar age




#tirar happy do mod completo
mod_s_happy <- lm(Diff ~ age + lastname + as.factor(Dosage) +Mem_Score_Before + as.factor(Drug))
summary(mod_s_happy)
anova(mod_completo,mod_s_happy) #é mellhor tirar happy

#tirar age 
mod_s_hap_age <- lm(Diff ~lastname + as.factor(Dosage) +Mem_Score_Before + as.factor(Drug))
summary(mod_s_hap_age)
anova(mod_s_happy,mod_s_hap_age) #tirar age

#tirar name
mod_s_hap_age_name <-  lm(Diff ~ as.factor(Dosage) +Mem_Score_Before + as.factor(Drug))
summary(mod_s_hap_age_name)
anova(mod_s_hap_age,mod_s_hap_age_name) #tirar age

#tirar score??
#r squared muito baixo!



