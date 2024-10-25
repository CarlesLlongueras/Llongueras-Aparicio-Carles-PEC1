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

metadata(se)
