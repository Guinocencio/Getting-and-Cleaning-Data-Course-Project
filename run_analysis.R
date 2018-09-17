
require(tidyverse)

doc <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipDoc <- "UCI HAR Dataset.zip"


if(!file.exists(zipDoc)){
download.file(doc, zipDoc, mode = "wb")}

caminhoDados <- "UCI HAR Dataset"
if(!file.exists(caminhoDados)){
unzip(zipDoc)}

atividades <- read.table(file.path(caminhoDados, "activity_labels.txt"))

caracteristicas <- read.table(file.path(caminhoDados, "features.txt"),as.is = T)


# leitura dos itens de subjects treino e teste

treinoSubjects <- read.table(file.path(caminhoDados, "train", "subject_train.txt"))
testeSubjects <- read.table(file.path(caminhoDados, "test", "subject_test.txt"))

# leitura dos itens de Valores treino e teste

treinoValores <- read.table(file.path(caminhoDados, "train", "X_train.txt"))
testeValores <- read.table(file.path(caminhoDados, "test", "X_test.txt"))

# leitura dos itens de Atividades treino e teste

treinoAtividades <- read.table(file.path(caminhoDados, "train", "y_train.txt"))
testeAtividades <- read.table(file.path(caminhoDados, "test", "y_test.txt"))

colnames(atividades) <- c("activityLabel", "activityId")

#Concatene as tabelas de dados por linhas

atividadePessoas <- rbind(cbind(testeSubjects, testeValores, testeAtividades),
cbind(treinoSubjects, treinoValores, treinoAtividades))


#Faz com que os dados individuais sejam removidos e salvados na tabela de memoria

rm(testeSubjects, testeValores, testeAtividades, treinoSubjects, treinoValores, treinoAtividades)

# Da nomes as colunas
colnames(atividadePessoas) <- c("subject", caracteristicas[,2], "activity")

#Pega as colunas que foram referenciadas com os nomes na linha anterior

colunas <- grepl("subject|activity|mean|std", colnames(atividadePessoas))


atividadePessoas <- atividadePessoas[, colunas]

#Substitui os valores da variavel "activity" pelos nomes dos fatores da variavel "atividadePessoas"
atividadePessoas$activity <- factor(atividadePessoas$activity, 
  levels=atividades[,1], labels=atividades[,2])

columAtividadePessoas <- colnames(atividadePessoas)

#Padroniza e remove os caracteres especiais
columAtividadesPessoas <- gsub("[\\(\\)-]", "", columAtividadePessoas)

#Subistitui as abrevia??es pelos nomes corretos
columAtividadePessoas <- gsub("^f", "DominioFrequencia", columAtividadePessoas)
columAtividadePessoas <- gsub("^t", "DominioTempo", columAtividadePessoas)
columAtividadePessoas <- gsub("Acc", "Acelerometro", columAtividadePessoas)
columAtividadePessoas <- gsub("Gyro", "Giroscopio", columAtividadePessoas)
columAtividadePessoas <- gsub("Mag", "Magnitude", columAtividadePessoas)
columAtividadePessoas <- gsub("Freq", "Frequencia", columAtividadePessoas)
columAtividadePessoas <- gsub("mean", "Media", columAtividadePessoas)
columAtividadePessoas <- gsub("std", "DesvioPadrao", columAtividadePessoas)

columAtividadePessoas <- gsub("BodyBody", "Corpo", columAtividadePessoas)

colnames(atividadePessoas ) <- columAtividadePessoas 

atividadePessoasMedia <- atividadePessoas %>% group_by(subject, activity) %>% summarise_all(funs(mean))

#cria um arquivo de saida contendo os resultados
write.table(atividadePessoasMedia , "tidy.txt", row.names = FALSE, 
            quote = FALSE)

