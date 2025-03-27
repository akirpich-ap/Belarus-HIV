rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)


# Loading package required to read library(readxl)
library(readxl)

# Library to work with plot.igraph
library(igraph)



# ---------------
# Reading data in

# Path for the data file
adjecency_matrix_path <- "../Data/adjecency_matrix.xlsx"
# Path for the data file
adjecency_matrix_directed_path <- "../Data/adjecency_matrix_directed.xlsx"



# Reading data
adjecency_matrix <- data.frame(read_excel(path = adjecency_matrix_path), check.names = FALSE)
dim(adjecency_matrix)
names(adjecency_matrix)

adjecency_matrix_directed <- data.frame(read_excel(path = adjecency_matrix_directed_path), check.names = FALSE)
dim(adjecency_matrix_directed)
names(adjecency_matrix_directed)



# Fixing names
names(adjecency_matrix)[1]  <- "Name" 
names(adjecency_matrix)[-1] <- adjecency_matrix$Name 
rownames(adjecency_matrix) <- adjecency_matrix$Name 

names(adjecency_matrix_directed)[1]  <- "Name" 
names(adjecency_matrix_directed)[-1] <- adjecency_matrix_directed$Name 
rownames(adjecency_matrix_directed) <- adjecency_matrix_directed$Name 



# Removing the first column
adjecency_matrix <- adjecency_matrix[, (!names(adjecency_matrix) %in% "Name") ]
adjecency_matrix_directed <- adjecency_matrix_directed[, (!names(adjecency_matrix_directed) %in% "Name") ]


# Converting to the matrix type
adjecency_matrix <- as.matrix(adjecency_matrix)
adjecency_matrix_directed <- as.matrix(adjecency_matrix_directed)


# Checking that it is indeed symmetric
isSymmetric(adjecency_matrix)




# Saving adjecency_matrix
save(adjecency_matrix, file = "../R_Data/adjecency_matrix.RData" ) 

# Saving adjecency_matrix_directed
save(adjecency_matrix_directed, file = "../R_Data/adjecency_matrix_directed.RData" ) 






