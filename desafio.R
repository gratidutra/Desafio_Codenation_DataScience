setwd("C:/Users/grati/Documents/Desafio")
train = read.csv("train.csv", h=T)
test = read.csv('test.csv', h=T)
data= read.csv("data.csv")

nrow(train)
nrow(test)

#######################train
cn = train$NU_NOTA_CN
ch = train$NU_NOTA_CH
lc = train$NU_NOTA_LC
mt = train$NU_NOTA_MT
rd = train$NU_NOTA_REDACAO

treino_2 = data.frame(cn,ch,lc,mt,rd)
treino_2 = na.omit(treino)
nrow(treino)

write.csv(treino, "treino.csv")
#########################test
cn = test$NU_NOTA_CN
ch = test$NU_NOTA_CH
lc = test$NU_NOTA_LC
rd = test$NU_NOTA_REDACAO
ins = test$NU_INSCRICAO

teste = data.frame(cn,ch,lc,rd, ins)
teste = na.omit(teste)
nrow(teste)

write.csv(teste, "teste.csv")
#########################Regression

treino <- data.frame(data$cn, data$ch, data$lc, data$rd, data$mt)

data.cn = data$cn2
data.ch = data$ch2
data.rd = data$rd2
data.lc = data$lc2

newdata <- data.frame(data.cn, data.ch, data.rd,data.lc)

prev = lm(data.mt~data.cn+data.ch+data.lc+data.rd, data= treino)
summary(prev)

prev2 = step(prev) 
summary(prev2)

mt_pred = predict(prev, newdata = newdata)

df_2 = data.frame(teste$ins, mt_pred)
write.csv(df_2, "answer.csv")


df = data.frame(teste$ins,newdata$data.cn,newdata$data.ch, newdata$data.lc,mt_pred, newdata$data.rd)
df
names(df)[0:6] <- c("ins","cn","ch","lc","mt","rd")
names(df_2)[0:1] <- c("ins","mt")
write.csv(df_2, "answer.csv", h=F)


