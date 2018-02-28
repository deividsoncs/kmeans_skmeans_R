#seto meu diretório de trabalho
setwd(dir = "/home/calixto/Área de Trabalho/R/k-means/")
# importo as libs
library(skmeans) # used for k-medians algorithm ou Kmeans Esférico (Utiliza a Distância do Cosseno)
install.packages("skmeans")
library(cluster) # required for calling the silhouette function
library(dplyr) # an excellent tool for for summarizing, ordering and filtering data features

# Importa para o DataSet Remove colunas categorias, converte NA em 0
kmcDF <- read.csv("dados_sem_trat.csv") #lê como DataFrame
View(kmcDF)
#head(kmcDF)
wineDF <- t(kmcDF[,-c(1,2,3,4,5,6,7)]) # nova variável DataFrame crica, remove colunas de 1-7, e faz a transposição do DataFrame
head(wineDF)
wineDF[is.na(wineDF)] <- 0 # substitui os valores NA por Zeros 
head(wineDF)
wineMatrix <-as.matrix(wineDF) #converte de DataFrame para Matriz

# Segment the customers into 5 clusters
partition <- skmeans(wineMatrix, 5) 
head(partition)
# Look at the segmentation outcome summary
partition # returns a summary statement for the process
partition$cluster # returns a vector showing cluster assignment for each customer

# Create a vector of customer names for each cluster
cluster_1 <- names(partition$cluster[partition$cluster == 1])
cluster_2 <- names(partition$cluster[partition$cluster == 2])
cluster_3 <- names(partition$cluster[partition$cluster == 3])
cluster_4 <- names(partition$cluster[partition$cluster == 4])
cluster_5 <- names(partition$cluster[partition$cluster == 5])

# Examine one of the clusters, as an example
cluster_1
cluster_2
cluster_3
cluster_4
cluster_5

#Gráfico silhouette
silhouette_k5 <- silhouette(partition)
summary(silhouette_k5)  
plot(silhouette_k5)

partition_k4 <- skmeans(wineMatrix, 4)
silhouette_k4 <- silhouette(partition_k4)
plot(silhouette_k4) 
summary(silhouette_k4)
