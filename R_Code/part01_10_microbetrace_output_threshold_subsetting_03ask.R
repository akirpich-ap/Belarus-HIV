rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)


# Import needed libraries.

# library(devtools)
# library(coda)

# install_github("laduplessis/bdskytools")
# install_github("laduplessis/beastio")


# library(bdskytools)
# library(beastio)


# Loading package required to read library(readxl)
library(readxl)




# Setting working directory.
# setwd("C:\\Users\\alina\\Desktop\\HIV_Belarus")
setwd("C:/Users/akirpich/Google Drive/2022 Kirpich-Skums-GSU-Belriem-HIV/results")
# setwd("G:/My Drive/2022 Kirpich-Skums-GSU-Belriem-HIV/results")


# Reading data
microbetrace_output_path <- paste0("../Data/microbetrace_output_links_within_graph_790.csv")
microbetrace_output <- data.frame(read.csv(file = microbetrace_output_path ))
dim(microbetrace_output)
head(microbetrace_output)


microbetrace_output <- microbetrace_output[!is.na(microbetrace_output$distance), ]
dim(microbetrace_output)



# Obtaining subset where distances are equal or above 0.025 
microbetrace_output_subset <- microbetrace_output[microbetrace_output$distance <= 0.025, ]
dim(microbetrace_output)
dim(microbetrace_output_subset)

summary(microbetrace_output$distance)
summary(microbetrace_output_subset$distance)


# Exporting table
microbetrace_output_subset_path_csv <- paste0("../R_Output/part01_10_reading_cities_fixing_names_microbetrace_output_subset_790.csv")
write.table(x = microbetrace_output_subset, file = microbetrace_output_subset_path_csv, sep = ",", quote = TRUE, row.names = FALSE, fileEncoding="UTF-8")




