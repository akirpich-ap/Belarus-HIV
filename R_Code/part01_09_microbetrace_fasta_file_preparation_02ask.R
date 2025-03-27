rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)
options(max.print=1000000)

# Import needed libraries.

# library(devtools)
# library(coda)

# install_github("laduplessis/bdskytools")
# install_github("laduplessis/beastio")


# library(bdskytools)
# library(beastio)


# Loading package required to read library(readxl)
library(readxl)





# library to read fasta files
# install.packages("seqinr ")
library(seqinr)


# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("msa")

# Library for alignment
library(msa)

# Library to export
library(bios2mds)

# Path for the data file
belriem_data_path                <- "../Data/RES Database (w sequences)_loc_fixed.xlsx"
belriem_data <- data.frame(read_excel(path = belriem_data_path), check.names = FALSE)

# List of unique strains in the orignla dataset
list_of_strains <- unique(belriem_data$strain)



# Reading data from BELRIEM
aligned_fasta <- bios2mds::import.fasta(file = "../Data/aligned.fasta" )

# Subsetting
which_to_keep <- which( names(aligned_fasta) %in% list_of_strains )
length(which_to_keep)


mismatch_sorted1 <- sort( setdiff( names(aligned_fasta), list_of_strains ) )
length(mismatch_sorted1)

mismatch_sorted2 <- sort( setdiff( list_of_strains, names(aligned_fasta) ) )
length(mismatch_sorted2)


# Subsetting elements of the list with the names which are belarusin sequences only.
typeof(aligned_fasta)


# Subset of sequences which are Belarus specific
aligned_fasta_subset <- aligned_fasta[which_to_keep]

attr(aligned_fasta_subset, "class") <- "align"

names(aligned_fasta_subset)

bios2mds::export.fasta(x = aligned_fasta_subset, outfile = "../R_Output/part01_09_aligned_fasta_subset.fasta", ncol = 60, open = "w")





