# Creación del contenedor

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("SummarizedExperiment")
library(SummarizedExperiment)

features <- read.csv("DataValues_S013.csv",
                     row.names = 1)

metadata <- read.csv("DataInfo_S013.csv",
                     row.names = 1)

metadataPath <- readLines("description.md")

metadataDescription <- paste(metadataPath)

if (ncol(features) != nrow(metadata)) {
  stop("El número de muestras en los datos y los metadatos no coinciden.")
}


se <- SummarizedExperiment(assays = list(assays = as.matrix(features)), 
                           colData = metadata, metadata = metadataDescription)

# Exploración del Dataset

class(se)

dim(se)

colData(se)

dimnames(se)[[1]]
head(dimnames(se)[[2]], 20)

assays(se)
assays(se)[[1]][1:5,1:5]



data <- assays(se)[[1]]

barplot(table(data[, 4]), 
        main = "Barplot del Género", 
        xlab = "Edad", 
        col = "lightblue", 
        border = "black")

barplot(table(data[, 2]), 
        main = "Barplot del tipo de Cirugía", 
        xlab = "Edad", 
        col = "lightgreen", 
        border = "black")

data <- as.matrix(assays(se)[[1]])
data <- as.data.frame(data)
data <- data[, colSums(is.na(data)) == 0]
data <- data[, -c(2,4)]
data[] <- lapply(data, function(x) as.numeric(as.character(x)))

pcaResult <- prcomp(data, scale. = TRUE)
loads<- round(pcaResult$sdev^2/sum(pcaResult$sdev^2)*100,1)

xlab<-c(paste("PC1",loads[1],"%"))
ylab<-c(paste("PC2",loads[2],"%"))

pcaData <- as.data.frame(pcaResult$x)
pcaData$gender <- features$GENDER
pcaData$surgery <- features$SURGERY

plot(pcaResult$x[,1:2],xlab=xlab ,ylab=ylab, 
     col = ifelse(pcaData$gender == "M", "blue", "hotpink"), 
     main ="Principal components (PCA) por Género")
legend("topright", legend = c("Hombres", "Mujeres"), col = c("blue", "hotpink"), pch = 19)


plot(pcaResult$x[,1:2],xlab=xlab ,ylab=ylab,
     col = ifelse(pcaData$surgery == "by pass", "red", "green"), 
     main ="Principal components (PCA) por Cirugía")
legend("topright", legend = c("By pass", "Tubular"), col = c("red", "green"), pch = 19)

clust.euclid.average <- hclust(dist(pcaResult$x[, 1:2]) ,method="average")
plot(clust.euclid.average, hang=-1)

metadata(se)

