rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)


# Loading package required to read library(readxl)
# install.packages("readxl")
library(readxl)

# library read fasta files
# install.packages("seqinr ")
library(seqinr)

# install.packages("xlsx")
# library(xlsx)


# Library to create ASCII analogs
# library(stringi)

# Library to create ASCII analogs
# library(fuzzyjoin)


# install.packages("httr")
# library(httr)


# install.packages("XML")
# library(XML)


# install.packages("jsonlite")
# library(jsonlite)


# install.packages("stringr")
# library(stringr)


# install.packages("stringi")
# library(stringi)




# ---------------
# Reading data in



# reading fasta file to get the names only
sequences_fasta_path <- paste0("../Data/hiv_msa_earlier_seq.fasta")
# Reading sequences 
sequences_fasta <- read.fasta(file = sequences_fasta_path)
names_from_fasta <- sort(names(sequences_fasta))



# Coordinates from openstreemap.org + BELRIEM
belriem_data_plus_coordinates_path_xlsx <- paste0("../R_Output/part01_06_reading_cities_fixing_names_belriem_data_plus_coordinates.xlsx")
# Reading data from BELRIEM + BELRIEM
belriem_data_plus_coordinates <- data.frame(read_excel(path = belriem_data_plus_coordinates_path_xlsx), check.names = FALSE)


# Network from microbetrace 
transnet_microbetrace_path_csv <- paste0("../results/transNet.csv")
# Network from microbetrace 
transnet_microbetrace <- data.frame(read.csv(file = transnet_microbetrace_path_csv), check.names = FALSE)


# Actual check of the relationships
setdiff(transnet_microbetrace$source, belriem_data_plus_coordinates$strain_plus_date)
setdiff(transnet_microbetrace$source, belriem_data_plus_coordinates$strain_fix_plus_date)

setdiff(transnet_microbetrace$source, belriem_data_plus_coordinates$strain_plus_date_ascii)
setdiff(transnet_microbetrace$source, belriem_data_plus_coordinates$strain_fix_plus_date_ascii)


setdiff(belriem_data_plus_coordinates$strain_plus_date, transnet_microbetrace$source )
setdiff(belriem_data_plus_coordinates$strain_fix_plus_date, transnet_microbetrace$source )

setdiff(belriem_data_plus_coordinates$strain_plus_date_ascii, transnet_microbetrace$source )
setdiff(belriem_data_plus_coordinates$strain_fix_plus_date_ascii, transnet_microbetrace$source )

























