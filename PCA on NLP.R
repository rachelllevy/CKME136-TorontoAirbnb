NLP.pca <- prcomp(tdm2, center = TRUE,scale. = TRUE)

#Identify the zero-variance column

which(apply(tdm2, 2, var)==0)

#Remove zero variance columns from the dataset

tdm3 <- tdm2[ , which(apply(tdm2, 2, var) != 0)]

PCs <- princomp(tdm3, cor = TRUE, scores = TRUE)
summary(PCs)

plot(PCs, type = "l")

install.packages("factoextra")
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(PCs)
eig.val

# Results for Variables
res.var <- get_pca_var(PCs)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(PCs)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 