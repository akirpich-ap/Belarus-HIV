rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)



# install_github("YuLab-SMU/treeio")
# install_github("YuLab-SMU/ggtree")

# library("pkgload")
# library("treeio")
# library("ggtree")


# if(!require("phytools")) {install.packages("phytools")}
# if(!require("strap")) {install.packages("strap")}
library(phytools)
# library(strap)

library(lubridate)

library(ape)

# Loading package required to read library(readxl)
library(readxl)


# Setting working directory.
setwd("C:/Users/akirpich/Google Drive/2022 Kirpich-Skums-GSU-Belriem-HIV/R_Code")


# Fix 2014.11.26.
# Reading locations data
# Path for the data file
belriem_data_path                <- "../R_Output/part01_06_reading_cities_fixing_names_belriem_data_plus_coordinates.xlsx"
belriem_data <- data.frame(read_excel(path = belriem_data_path), check.names = FALSE)

# List of unique strains in the orignla dataset
list_of_strains <- unique(belriem_data$strain)





# Reading data
time_tree_hiv_path <- paste0("../results/Bayesian skyline/results/tree.nexus")

tree_nexus_hiv <- read.nexus(time_tree_hiv_path)
class(tree_nexus_hiv)

# Extracting the length of the tree
initial_value <- min(nodeHeights(tree_nexus_hiv)) 
final_value   <- max(nodeHeights(tree_nexus_hiv)) 
lenght_of_values <- final_value - initial_value


# Extracting usual dates from names
tip_labels_extract <- tree_nexus_hiv$tip.label
length(tip_labels_extract)
table(nchar(tip_labels_extract))
# Extracted
tip_labels_extract_truncate <- substr( x = tip_labels_extract, start = nchar(tip_labels_extract) - 9, stop = nchar(tip_labels_extract) )
# Converted to dates
tip_labels_extract_truncate_date <- as.Date( x = tip_labels_extract_truncate, origin = "1970-01-01" )


# Comparing name agreements i.e. that all lables in the tree should be in belriem_data$strain_fix_plus_date_ascii
setdiff( tip_labels_extract, belriem_data$strain_fix_plus_date_ascii )
         
         
# Creating color codes for regions of interest
sort(table(belriem_data$location))
table(belriem_data$division_ascii.y)


which_minsk         <- which(belriem_data$location == "Minsk")
which_salihorsk     <- which(belriem_data$location == "Salihorsk")
which_zlobin        <- which(belriem_data$location == "Žlobin")
which_svietlahorsk  <- which(belriem_data$location == "Svietlahorsk")
which_zodzina       <- which(belriem_data$location == "Žodzina")
which_homiel        <- which(belriem_data$location == "Homieĺ")
which_asipovicy     <- which(belriem_data$location == "Asipovičy")
which_mahiliou      <- which(belriem_data$location == "Mahilioŭ")
which_sluck         <- which(belriem_data$location == "Sluck")
which_slonim        <- which(belriem_data$location == "Slonim")
which_recyca        <- which(belriem_data$location == "Rečyca")
which_pinsk         <- which(belriem_data$location == "Pinsk")
which_homielskaja   <- which(belriem_data$division_ascii.y == "Homielskaja voblasc")
which_mahilouskaja  <- which(belriem_data$division_ascii.y == "Mahilouskaja voblasc")
which_hrodzienskaja <- which(belriem_data$division_ascii.y == "Hrodzienskaja voblasc")
which_minskaja      <- which(belriem_data$division_ascii.y == "Minskaja voblasc")
which_bresckaja     <- which(belriem_data$division_ascii.y == "Bresckaja voblasc")
which_viciebskaja   <- which(belriem_data$division_ascii.y == "Viciebskaja voblasc")

# Fix 2024.12.12.
which_salihorsk_svietlahorsk  <-  union(which(belriem_data$location == "Salihorsk"), which(belriem_data$location == "Svietlahorsk"))



short_name_list <- c("which_minsk" , "which_salihorsk", "which_zlobin", "which_svietlahorsk", "which_zodzina", 
                     "which_homiel", "which_asipovicy", "which_mahiliou", "which_sluck", "which_slonim", 
                     "which_recyca", "which_pinsk", 
                     "which_homielskaja", "which_mahilouskaja", "which_hrodzienskaja", "which_minskaja", "which_bresckaja", "which_viciebskaja", "which_salihorsk_svietlahorsk")

long_name_list  <- c("Minsk", "Salihorsk", "Zlobin", "Svietlahorsk", "Zodzina", 
                     "Homiel", "Asipovicy", "Mahiliou", "Sluck", "Slonim", 
                     "Recyca", "Pinsk", 
                     "Homyel Region", "Mahiliow Region", "Hrodna Region", "Minsk Region", "Brest Region", "Vitsyebsk Region", "Salihorsk and Svietlahorsk")



design_table <- data.frame( short_name =  short_name_list,
                            long_name  =  long_name_list )








# Getting maximum date value
max_date_value <- max(tip_labels_extract_truncate_date)

# Computing min date value after converting to decimals 
min_date_value_decimal <- decimal_date(max_date_value) - lenght_of_values
min_date_value <- as.Date(date_decimal(min_date_value_decimal))






# Raw version (vertical) v1
pdf(paste("../Plots/part01_05_plotting_tree_vertical_v1.pdf", sep = ""), height = 35, width = 13)
par(mar = c(5.5, 2, 1.25, 0))

plot.phylo(x = tree_nexus_hiv,
           edge.width = 1,
           # label.offset = 0.4,
           cex = 2,
           show.tip.label = FALSE,
           # direction =  "downwards",
           cex.main = 2,
           main = "Timed Phylogeny of the Belarusian HIV sequences")

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

no_of_displayed_dates <- 20
x_tlab <- seq( from  = initial_value, to  = final_value,  by = (final_value - initial_value)/no_of_displayed_dates ) 
x_lablist <- seq( from  = min_date_value, to  = max_date_value,  by = (max_date_value - min_date_value)/no_of_displayed_dates ) 
axis(1, at = x_tlab, labels = FALSE)
text(x = x_tlab, y=par()$usr[3]-0.01*(par()$usr[4]-par()$usr[3]), labels = x_lablist, srt=45, adj=1, xpd=TRUE, cex.axis = 2)

dev.off()



# Raw version (vertical) v2
pdf(paste("../Plots/part01_05_plotting_tree_vertical_v2.pdf", sep = ""), height = 35, width = 13)
par(mar = c(8, 2, 1.25, 0))

plot.phylo(x = tree_nexus_hiv,
           edge.width = 1,
           # label.offset = 0.4,
           cex = 2,
           show.tip.label = FALSE,
           # direction =  "downwards",
           cex.main = 2,
           main = "Timed Phylogeny of the Belarusian HIV sequences")

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.

no_of_displayed_dates <- 20
x_tlab <- seq( from  = initial_value, to  = final_value,  by = (final_value - initial_value)/no_of_displayed_dates ) 
x_lablist <- seq( from  = min_date_value, to  = max_date_value,  by = (max_date_value - min_date_value)/no_of_displayed_dates ) 
axis(1, at = x_tlab, labels = x_lablist, las = 2, cex.axis = 1.5)

dev.off()




# Raw version (vertical)
pdf(paste("../Plots/part01_05_plotting_tree_horizontal_all.pdf", sep = ""), height = 13, width = 35)
par(mar = c(0, 12, 4, 0))

plot.phylo(x = tree_nexus_hiv,
           edge.width = 1,
           # label.offset = 0.4,
           cex = 2,
           show.tip.label = FALSE,
           direction =  "downwards",
           cex.main = 4,
           main = "Timed Phylogeny of the Belarusian HIV sequences")

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


no_of_displayed_dates <- 20
y_tlab <- seq( from  = final_value, to = initial_value, by = -(final_value - initial_value)/no_of_displayed_dates ) 
y_lablist <- seq( from  = max_date_value, to = min_date_value, by = -(max_date_value - min_date_value)/no_of_displayed_dates ) 
axis(2, at = y_tlab, labels = x_lablist, las = 2, cex.axis = 2.25)



dev.off()




# Raw version (vertical)
pdf(paste("../Plots/part01_05_plotting_tree_horizontal_since1998.pdf", sep = ""), height = 13, width = 35)
par(mar = c(0, 12, 4, 0))


key_node_label <- 644

plot.phylo(x = extract.clade(tree_nexus_hiv,  key_node_label),
           edge.width = 1,
           # label.offset = 0.4,
           cex = 3,
           show.tip.label = FALSE,
           direction =  "downwards",
           cex.main = 4,
           main = "Timed Phylogeny of the Belarusian HIV sequences")

# X-axis
# labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
# Creating labels by month and converting.


no_of_displayed_dates <- 20
y_tlab <- seq( from  = final_value, to = initial_value, by = -(final_value - initial_value)/no_of_displayed_dates ) 
y_lablist <- seq( from  = max_date_value, to = min_date_value, by = -(max_date_value - min_date_value)/no_of_displayed_dates ) 
axis(2, at = y_tlab, labels = x_lablist, las = 2, cex.axis = 2.25)


dev.off()




# Fix 2024.12.12.
# Coloring according to design_table

# List of graph types 
graph_type_list <- c("all", "mrca", "since1998")


for( graph_current in graph_type_list )
{
   # Debugging
   # graph_current <- graph_type_list[1]

  for( i in  c(1:dim(design_table)[1]) )
  {
    # Debugging step
    # i <- 1
    # i <- 12
    # i <- 19      
    
    # Getting the list  
    list_name_current_text <- design_table$short_name[i]
    eval(parse(text = paste0("list_index_current <- ", list_name_current_text  ) ))
    
    
    # Getting the tip labels of interest
    tip_labels_interest <- tree_nexus_hiv$tip.label[ tree_nexus_hiv$tip.label %in% belriem_data$strain_fix_plus_date[list_index_current] ]
    
    # Creating a subtree  
    
    # This label was obtained by hand after using nodelabels() after plot which is now commented.
    key_node_label <- 644
    
    
    
    # This depends on what root is desired -> all.
    if (graph_current == graph_type_list[1])
    {
      tree_nexus_hiv_extract <- tree_nexus_hiv
    }  

        
    # This depends on what root is desired -> mrca.
    if (graph_current == graph_type_list[2])
    {
      tree_nexus_hiv_extract <- extract.clade(tree_nexus_hiv, getMRCA(tree_nexus_hiv, tip_labels_interest ) )
    }  

        
    # This depends on what root is desired -> since1998.
    if (graph_current == graph_type_list[3])
    {
      tree_nexus_hiv_extract <- extract.clade(tree_nexus_hiv,  key_node_label)
    }  

    
    # Defining colors
    general_color    <- "#525252"
    highlight_color  <- "#fec44f"
    highlight_color2 <- "#2171b5"
    highlight_color3 <- "#8856a7"
    
    # All tips color codes
    tip_colors  <- rep(general_color, length(tree_nexus_hiv_extract$tip.label) )
    edge_colors <- rep(general_color, length(tree_nexus_hiv_extract$edge[,2]) )
    
    # Adjusting the current color codes
    
    # Assigning red to the selected tip colors
    tip_colors[ tree_nexus_hiv_extract$tip.label %in% belriem_data$strain_fix_plus_date_ascii[list_index_current] ] <- highlight_color
    
    # Assigning red to the selected edge colors
    edge_colors[ tree_nexus_hiv_extract$edge[,2] %in% which(tip_colors == highlight_color) ] <- highlight_color
    
    # Fix 2025.01.15.
    # Adjusting colors for the second location if needed.
    if ( list_name_current_text == "which_salihorsk_svietlahorsk" )
    {
     
      
      # Those edges are added manually from the tree with numbers and added below.
      edges_list <- c( c(641, 1266),
                       c(642, 625),
                       c(643, 624),
                       c(642, 625),
                       c(1266, 1267),
                       c(1267, 1268),
                       c(1268, 1269),
                       c(1268, 1269),
                       c(1269, 626),
                       c(1269, 627),
                       c(1268, 628),
                       c(1267, 1270),
                       c(1270, 1271),
                       c(1271, 1272),
                       c(1272, 1273),
                       c(1272, 633),
                       c(1273, 1274),
                       c(1274, 629),
                       c(1274, 1275),
                       c(1275, 630),
                       c(1275, 631),
                       c(1273, 632),
                       c(1271, 1276),
                       c(1276, 634),
                       c(1276, 635),
                       c(1270, 636),
                       c(1266, 1277),
                       c(1277, 1278),
                       c(1278, 637),
                       c(1278, 638),
                       c(1277, 1279),
                       c(1279, 639),
                       c(1279, 640) )
            
      edges_matrix <- t(matrix(data = edges_list, nrow = 2, ncol = length(edges_list)/2))
      
      
      # Function to check if row `rowA` is a subrow of row `rowB`
      is_subrow <- function(rowA, rowB) {
        all(rowA %in% rowB) && length(rowA) <= length(rowB)
      }
      
      # Check if each row of A is a subrow of any row in B
      check_subrows <- function(A, B) {
        result <- apply(A, 1, function(rowA) {
          any(apply(B, 1, function(rowB) is_subrow(rowA, rowB)))
        })
        return(result)
      }
      
      # Apply the function to check if rows of A are subrows of B
      subrow_check_result <- check_subrows(tree_nexus_hiv_extract$edge, edges_matrix)
      
      # Assigning red to the selected edge colors
      edge_colors[ subrow_check_result ] <- highlight_color3
      

      
      # Assigning red to the selected tip colors
      tip_colors[ tree_nexus_hiv_extract$tip.label %in% belriem_data$strain_fix_plus_date_ascii[list_index_current] ] <- highlight_color
      
      # Assigning red to the selected edge colors
      edge_colors[ tree_nexus_hiv_extract$edge[,2] %in% which(tip_colors == highlight_color) ] <- highlight_color
      
      
      
      list_index_current2 <- which_svietlahorsk
      
      # Assigning red to the selected tip colors
      tip_colors[ tree_nexus_hiv_extract$tip.label %in% belriem_data$strain_fix_plus_date_ascii[list_index_current2] ] <- highlight_color2
      
      # Assigning red to the selected edge colors
      edge_colors[ tree_nexus_hiv_extract$edge[,2] %in% which(tip_colors == highlight_color2) ] <- highlight_color2
      
      
    # End of -> if ( list_name_current_text == "which_salihorsk_svietlahorsk" )  
    }  
    
    
    # Raw version (vertical)
    pdf(paste("../Plots/part01_05_plotting_tree_horizontal_", design_table$short_name[i] , "_", graph_current, ".pdf", sep = ""), height = 13, width = 38)
    par(mar = c(0, 12, 4, 0))
    
    
    plot.phylo(x = tree_nexus_hiv_extract,
               edge.width = 1.5,
               # show.node.label = TRUE,
               # label.offset = 0.4,
               cex = 3,
               show.tip.label = FALSE,
               direction =  "downwards",
               tip.color = tip_colors,
               edge.color = edge_colors,
               cex.main = 4 )
               # main = paste0( "Timed Phylogeny of the Belarusian HIV sequences (", design_table$long_name[i], ")") )
    
    # nodelabels()
    # tiplabels()  
    # dev.off()

    if ( list_name_current_text != "which_salihorsk_svietlahorsk" )
    {
      
      legend( x = "topleft", 
              inset= c(0.010, 0.010), 
              legend = c(design_table$long_name[i], "All Other"), 
              # col = c(highlight_color,  general_color), 
              fill = c(highlight_color,  general_color), 
              pt.cex = 4,  
              cex = 2.5 ) 
      
      
      # X-axis
      # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
      # Creating labels by month and converting.
      
    }    
    

    if ( list_name_current_text == "which_salihorsk_svietlahorsk" )
    {
      
      legend( x = "topleft", 
              inset= c(0.010, 0.010), 
              legend = c("Salihorsk", "Svietlahorsk", "Isolated Introductions", "All Other"), 
              # col = c(highlight_color,  general_color), 
              fill = c(highlight_color, highlight_color2, highlight_color3,  general_color), 
              pt.cex = 4,  
              cex = 2.5 ) 
      
      
      # X-axis
      # labels FAQ -> http://www.r-bloggers.com/rotated-axis-labels-in-r-plots/
      # Creating labels by month and converting.
      
    }    
    
    
        
    
    no_of_displayed_dates <- 20
    y_tlab <- seq( from  = final_value, to = initial_value, by = -(final_value - initial_value)/no_of_displayed_dates ) 
    y_lablist <- seq( from  = max_date_value, to = min_date_value, by = -(max_date_value - min_date_value)/no_of_displayed_dates ) 
    axis(2, at = y_tlab, labels = x_lablist, las = 2, cex.axis = 2.25)
    
    
    
    dev.off()
    
    
    
    
  # End of -> for( i in  c(1:dim(design_table)[1]) )  
  }  
  
  
  
  
  
# End of -> for( graph_current in graph_type_list )   
}  
  




















































