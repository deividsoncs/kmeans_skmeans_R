#seto meu diretório de trabalho
setwd(dir = "/home/calixto/Área de Trabalho/R/k-means/")
#import a lib que vai ler o meu arquivo .csv
library(readr)
#install.packages("tibble", dependencies = TRUE)
#install.packages(c("Rcpp", "readr"))

#realizo a leitura do meu .csv para o meu DataSet
#dados <- read_csv("dados_cap2.csv")
dados <- read_csv("dados_cap2_num.csv")
#visualizo dos dados
View(dados)

#mostra média, mediana , 1º e 3º Quartis, Min e Max
summary(dados)

#remove a coluna passada
dados$Offer <- NULL
#mostra dados no console
head(dados)

#realiza a transposição dos dados
dadosT <- t(dados)
head(dadosT)
#mostra dados aba
#View(dados)

#matrizDadosT <- as.matrix(dadosT)
#head(matrizDadosT)
k <-kmeans(dadosT, centers=4, iter.max = 500) #cria x clusters 
head(k$centers)
#View(k$centers) #Mostra os centros dos clusters
table(k$cluster) #conta os pontos de dados alocados em cada cluster

#visualização dos clusters
library(factoextra)
install.packages("factoextra")
fviz_cluster(k$centers, data = dadosT,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), 
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal()
)

#iteração para descobrir curva de acordo com o número de clusters
rng<-2:20 #K w 2 até 20
tries <-100 #Run the K Means algorithm 100 times
avg.totw.ss <-integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(dadosT, centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")

#Visualização da clusterização
library(cluster)
library(fpc)
#install.packages("fpc")
head(dadosT)
# Análise do Kmeans e agrupamentos
clus <- kmeans(dadosT, centers=4, iter.max = 500)
# plotagem 
plotcluster(dadosT, clus$cluster)

#plotagem com silhouette
partition <- kmeans(dadosT, centers = 5, iter.max = 500)
cluster_1_e <- names(partition$cluster[partition$cluster == 1])
cluster_2_e <- names(partition$cluster[partition$cluster == 2])
cluster_3_e <- names(partition$cluster[partition$cluster == 3])
cluster_4_e <- names(partition$cluster[partition$cluster == 4])
cluster_5_e <- names(partition$cluster[partition$cluster == 5])

cluster_1_e
cluster_2_e
cluster_3_e
cluster_4_e
cluster_5_e


