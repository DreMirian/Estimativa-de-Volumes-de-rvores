#instalação dos pacotes
install.packages("e1071")
install.packages("randomForest")
install.packages("kernlab")
install.packages("caret")
library("caret")

#Carregar o arquivo Volumes.csv
tabela = read.table("Volumes.csv", sep = ";", dec = ",", header = TRUE) 
tabela
#Eliminar a coluna NR, que só apresenta um número sequencial
tabela[["NR"]]<-NULL 
tabela
#criando um dataset com os dados de tabela
dataset<-tabela 
dataset 
#Particionar a bases em treino (80%) e teste (20%)
indices <- createDataPartition(dataset$VOL, p=0.80, list=FALSE) 
indices
treino <- dataset[indices,]
treino
teste <- dataset[-indices,]
teste
#Treinar um modelo Random Forest com a base de treino
set.seed(7)
rf <- train(VOL~., data=treino, method="rf") 
#WARNING note: only 2 unique complexity parameters in default grid. Truncating the grid to 2 .
#Efetuar as predições na base de teste
predicoes.rf <- predict(rf, teste)
predicoes.rf

#Gerar um novo modelo usando SVM, predições e matriz de confusão
set.seed(7)
svm <- train(VOL~., data=treino, method="svmRadial")
svm
predicoes.svm <- predict(svm, teste)
predicoes.svm
confusionMatrix(predicoes.svm, teste$VOL) #esse da erro

#Comparando os modelos com resamples()
set.seed(7)
resultados <- resamples(list(rf=rf, svm=svm))
summary(resultados)
