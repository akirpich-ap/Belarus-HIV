rm(list = ls(all = TRUE))
# Extra check that we deleted everything.
# 20 Digits Precision Representation.
# options(scipen=20)


# Loading package required to read library(readxl)
# install.packages("readxl")
library(readxl)

# install.packages("xlsx")
library(xlsx)


# Library to create ASCII analogs
# library(stringi)

# Library to create ASCII analogs
# library(fuzzyjoin)


# install.packages("httr")
library(httr)


# install.packages("XML")
library(XML)


# install.packages("jsonlite")
# library(jsonlite)


# install.packages("stringr")
library(stringr)


# install.packages("stringi")
library(stringi)


# library to read fasta files
# install.packages("seqinr ")
library(seqinr)


# ---------------
# Reading data in

# reading fasta file to get the names only
sequences_fasta_path <- paste0("../Data/hiv_msa_earlier_seq.fasta")
# Reading sequences 
sequences_fasta <- read.fasta(file = sequences_fasta_path)
names_from_fasta <- sort(names(sequences_fasta))
names_from_fasta
length(unique(names_from_fasta))


# Path for the data file
belriem_data_path                <- "../Data/RES Database (w sequences)_loc_fixed.xlsx"
belarus_longitude_lattitude_path <- "../Data/Belarus_long_lat_gpt_final.xlsx"
belarus_names_cyrillic_path      <- "../Data/Belarus_names_final_cyrillic.xlsx"




# Reading data from BELRIEM
belriem_data <- data.frame(read_excel(path = belriem_data_path), check.names = FALSE)
# Fixing dates
belriem_data$date <- as.Date(belriem_data$date)
dim(belriem_data)
names(belriem_data)



# Reading Belarusian cities
belarus_longitude_lattitude <- data.frame(read_excel(path = belarus_longitude_lattitude_path), check.names = FALSE)
dim(belarus_longitude_lattitude)

# Reading Belarusian neighborhoods
belarus_names_cyrillic <- data.frame(read_excel(path = belarus_names_cyrillic_path), check.names = FALSE)
dim(belarus_names_cyrillic)


# Merging Cyrillic divisions
belarus_longitude_lattitude <- base::merge( x = belarus_longitude_lattitude, 
                                            y = belarus_names_cyrillic, 
                                            by = "division_ascii", 
                                            all.x = TRUE, 
                                            all.y = FALSE )



# Fixing sequences in the location file so that the names will be aligned with the fasta file
# Those are the "correct names"
names_from_fasta
# Those are the ones with the geo coordinates
belriem_data$strain
#  1. Adding date to the end of the sequence name
belriem_data$strain_plus_date       <- paste0(belriem_data$strain, "_",  belriem_data$date )


discrepancy_with_fasta <- sort(setdiff(names_from_fasta, belriem_data$strain_plus_date))
sort(belriem_data$strain_plus_date)


# Duplicating the fixes to be corrected
belriem_data$strain_fix <- belriem_data$strain




sink(paste0("../R_Output/part01_06_reading_cities_fixing_names_part01.txt"))
ls()


# Fixing ids in belriem_data so that they agree with names_from_fasta
for(current_discrepancy in discrepancy_with_fasta)
{
  # Debugging
  # current_discrepancy <- discrepancy_with_fasta[1]
  
  
  # Strain check
  pattern_to_check_strain <- substr( x = current_discrepancy , start = nchar(current_discrepancy) - 20, stop = nchar(current_discrepancy) - 11 )
  which_in_belriem_data_strain <- which( grepl( pattern = pattern_to_check_strain, x = belriem_data$strain ))
  
  # date to check
  pattern_to_check_date <- as.Date(x = substr( x = current_discrepancy , start = nchar(current_discrepancy) - 9, stop = nchar(current_discrepancy) ), origin = "1970-01-01")
  which_in_belriem_data_date <- which( grepl( pattern = pattern_to_check_date, x = belriem_data$date ))
  
  
  which_in_belriem_data <- intersect(which_in_belriem_data_strain, which_in_belriem_data_date)
  
  # Fixing the strain
  belriem_data$strain_fix[which_in_belriem_data] <- pattern_to_check_strain
  
  cat("current_discrepancy in fasta -> ", current_discrepancy, " value in  belriem_data$strain ", belriem_data$strain[which_in_belriem_data], " >>> changed to ",
      belriem_data$strain_fix[which_in_belriem_data], "with belriem_data$date ", as.character(belriem_data$date[which_in_belriem_data]), "\n")
  
# End of -> for(current_discrepancy in discrepancy_with_fasta)  
}  
  
# Extra check
length(discrepancy_with_fasta)
sum(!belriem_data$strain_fix == belriem_data$strain)


#  Adding date to the end of the sequence name after the fix
belriem_data$strain_fix_plus_date       <- paste0(belriem_data$strain_fix, "_",  belriem_data$date )

# Check again
discrepancy_with_fasta <- sort(setdiff(names_from_fasta, belriem_data$strain_fix_plus_date))

sink()









# Function to get coordinates from Nominatim API
get_coordinates <- function(place_name) {
  
  # Debugging
  # i <- 1
  # i <- 2      
  # i <- 9  
  # i <- 14
  # i <- 15
  # i <- 16
  # place_name <- c(belarus_longitude_lattitude$location_ascii[i],
  #                 belarus_longitude_lattitude$belarusian_name_without_voblast[i],
  #                 belarus_longitude_lattitude$russian_name_without_voblast[i],
  #                 belarus_longitude_lattitude$belarusian_latin_ascii[i],
  #                 belarus_longitude_lattitude$russian_latin_ascii[i],
  #                 belarus_longitude_lattitude$country_latin[i],
  #                 belarus_longitude_lattitude$country_cyrrilic[i])
  

  
  # url <- paste0("https://nominatim.openstreetmap.org/search?q=", place_name[1], "&addressdetails=1&limit=100&format=xml" )
  url <- paste0("https://nominatim.openstreetmap.org/search?q=", URLencode(place_name[1]), "&limit=50&format=xml" )
  
  
  # url <- paste0("https://nominatim.openstreetmap.org/search?q=Mir&addressdetails=1&limit=100&format=xml")
  
  # Perform the GET request
  response <- GET(url)
  
  # response <- GET(url)
  
  # data <- xmlParse(url)
  
  data_xml <- xmlParse(response)
  
  data_final <- xmlToList(data_xml)
  

  
  
  # Searching one by one
  for( current_loc in c(1:length(data_final)) )
  {
    # Debugging
    # current_loc <- 1
    # current_loc <- 2
    # current_loc <- 3    
    # current_loc <- 13
    
    cat("Record Number which is porcessedo is -> ", current_loc, " out of",  length(data_final), "\n" )
    
    
    # Reading current location 
    data_final_current_loc <- data_final[[current_loc]]
    
    # Converting to frame if possible
    data_final_current_loc <- data.frame(t(data_final_current_loc))
    
    
    
    # Ordering reminder
    # belarus_longitude_lattitude$location_ascii[i],
    # belarus_longitude_lattitude$belarusian_name_without_voblast[i],
    # belarus_longitude_lattitude$russian_name_without_voblast[i],
    # belarus_longitude_lattitude$belarusian_latin_ascii[i],
    # belarus_longitude_lattitude$russian_latin_ascii[i],
    # belarus_longitude_lattitude$country_latin[i],
    # belarus_longitude_lattitude$country_cyrrilic[i])
    
    
    cat("Location -> ", place_name[1], " <- Parental Details Below!\n" )
    
    # if we are NOT in the final string    
    if ( !is.null(data_final_current_loc$display_name) )
    {

      # Checking that the record is indeed from Belarus.
      # Either of the filters should satisfy
      indexes_belarus <- which( (grepl(pattern = place_name[length(place_name)],  x = data_final_current_loc$display_name) +
                                 grepl(pattern = place_name[length(place_name)-1],  x = data_final_current_loc$display_name)) > 0 )

      cat("Location -> ", place_name[1], " <- for Belarus for current_loc ->", current_loc,  " in MAIN xml status -> ", length(indexes_belarus), "\n" )
      cat("Record:\n" )
      print(data_final_current_loc$display_name[indexes_belarus] )
      
      
      cat("Location -> ", place_name[1], " <- Filtered by Region!\n" )
      
            
      # Checking that display name is of interest has mathcing oblast
      index_belarus_region <- which( (grepl(pattern = place_name[2], x = data_final_current_loc$display_name[indexes_belarus]) +
                                      grepl(pattern = place_name[3], x = data_final_current_loc$display_name[indexes_belarus]) +
                                      grepl(pattern = place_name[4], x = data_final_current_loc$display_name[indexes_belarus]) +
                                      grepl(pattern = place_name[5], x = data_final_current_loc$display_name[indexes_belarus])) > 0 )
      
      
      if (length(index_belarus_region) > 0) {
        
        lat <- data_final_current_loc$lat[ indexes_belarus[index_belarus_region] ]
        lon <- data_final_current_loc$lon[ indexes_belarus[index_belarus_region] ]
        cat("Extract  -> lat ->", lat, "long ->", lon, "\n\n\n" )
        
        return(c(lat, lon))
      } else {
        
        # cat("Location -> ", place_name, " <- has not been found!!!!\n\n\n" )
        # return(c(NA, NA))
      }
      
    # End of -> if ( !is.null(data_final_current_loc$display_name) )  
    }

    
    
    
    
    
    # if we are IN in the final string    
    if ( is.null(data_final_current_loc$display_name) )
    {
      
      cat("Location -> ", place_name[1], " <- for Belarus for current_loc ->", current_loc,  "MOVING TO ALTERNATIVE SEARCH!!!\n" )

      # Searching for the URL with more info
      
      # UFL search template
      url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
      
      
      if ( (".attrs" %in% names(data_final)) )
      {
        further_info_strings <-  str_extract(data_final$.attrs, url_pattern)
        index_with_match <- grep(pattern = "https://nominatim.openstreetmap.org/", x = further_info_strings)
        
        # Getting NEW URL
        further_info_strings_exact_url <- further_info_strings[index_with_match]
        
      } else {
        
        cat("Location -> ", place_name, " <- has not been found ANYWHERE!\n\n\n" )
        
        return(c(NA, NA))
        
      }
      
      
      
      further_info_strings <-  str_extract(data_final$.attrs, url_pattern)
      index_with_match <- grep(pattern = "https://nominatim.openstreetmap.org/", x = further_info_strings)
      
      # Getting NEW URL
      further_info_strings_exact_url <- further_info_strings[index_with_match]
      
      
      # Perform the GET request
      response_new <- GET(further_info_strings_exact_url)
      
      data_xml_new <- xmlParse(response_new)
      
      data_final_new <- xmlToList(data_xml_new)
      
      
      
      # Same procedure again
      # Searching one by one
      for( current_loc_new in c(1:length(data_final_new)) )
      {
        # Debugging
        # current_loc_new <- 1
        # current_loc_new <- 2
        # current_loc_new <- 3    
        # current_loc_new <- 11
        
        # Reading current location 
        if ( (current_loc_new != length(data_final_new)) && (".attrs" %in% names(data_final_new))   )
        {
          data_final_current_loc_new <- (data_final_new[[current_loc_new]])$.attrs
        } else {
          
          data_final_current_loc_new <- (data_final_new[[current_loc_new]])
        }
        
        

        # Converting to frame if possible
        data_final_current_loc_new <- data.frame(t(data_final_current_loc_new))
        

        # Ordering reminder
        # belarus_longitude_lattitude$location_ascii[i],
        # belarus_longitude_lattitude$belarusian_name_without_voblast[i],
        # belarus_longitude_lattitude$russian_name_without_voblast[i],
        # belarus_longitude_lattitude$belarusian_latin_ascii[i],
        # belarus_longitude_lattitude$russian_latin_ascii[i],
        # belarus_longitude_lattitude$country_latin[i],
        # belarus_longitude_lattitude$country_cyrrilic[i])
        
        
        # if we are NOT in the final string    
        if ( !is.null(data_final_current_loc_new$display_name) )
        {
          
          # Checking that the record is indeed from Belarus.
          # Either of the filters should satisfy
          indexes_belarus <- which( (grepl(pattern = place_name[length(place_name)],   x = data_final_current_loc_new$display_name) +
                                     grepl(pattern = place_name[length(place_name)-1], x = data_final_current_loc_new$display_name)) > 0 )
          
          cat("Location -> ", place_name[1], " <- for Belarus for current_loc ->", current_loc,  " in SECOND xml status -> ", length(indexes_belarus), "\n" )
          cat("Record:\n" )
          print(data_final_current_loc_new$display_name[indexes_belarus] )
          
          
          cat("Location -> ", place_name[1], " <- Filteting by Region!\n" )
          
          
          # Checking that display name is of interest has mathcing oblast
          index_belarus_region <- which( (grepl(pattern = place_name[2], x = data_final_current_loc_new$display_name[indexes_belarus]) +
                                          grepl(pattern = place_name[3], x = data_final_current_loc_new$display_name[indexes_belarus]) +
                                          grepl(pattern = place_name[4], x = data_final_current_loc_new$display_name[indexes_belarus]) +
                                          grepl(pattern = place_name[5], x = data_final_current_loc_new$display_name[indexes_belarus])) > 0 )
          
          
          if (length(index_belarus_region) > 0) {
            
            cat("Location -> ", place_name[1], " <- found!\n" )
            lat <- data_final_current_loc_new$lat[ indexes_belarus[index_belarus_region] ]
            lon <- data_final_current_loc_new$lon[ indexes_belarus[index_belarus_region] ]
            cat("Extract  -> lat ->", lat, "long ->", lon, "\n\n\n" )
            
            return(c(lat, lon))
          } else {
            
            cat("Location -> ", place_name, " <- has not been found in the SECOND xml!\n\n\n" )
            # return(c(NA, NA))
          }
          
        # End of -> if ( !is.null(data_final_current_loc$display_name) )  
        }
        
        
        
        
        # if we are in the final string    
        if ( is.null(data_final_current_loc_new$display_name) )
        {
          
          # Same procedure again
          
          # Searching for the URL whith more info
          
          # UFL search template
          url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
          
          
          if ( (".attrs" %in% names(data_final_new)) )
          {
            further_info_strings <-  str_extract(data_final_new$.attrs, url_pattern)
            index_with_match <- grep(pattern = "https://nominatim.openstreetmap.org/", x = further_info_strings)
            
            # Getting NEW URL
            further_info_strings_exact_url <- further_info_strings[index_with_match]
            
          } else {
            
            cat("Location -> ", place_name, " <- has not been found ANYWHERE!\n\n\n" )
            
            return(c(NA, NA))
            
          }
          
          
          
          
          # Perform the GET request
          response_new2 <- GET(further_info_strings_exact_url)
          
          data_xml_new2 <- xmlParse(response_new2)
          
          data_final_new2 <- xmlToList(data_xml_new2)
          
          
          
          
          # Searching one by one
          for( current_loc_new2 in c(1:length(data_final_new2)) )
          {
            # Debugging
            # current_loc_new2 <- 1
            # current_loc_new2 <- 2
            # current_loc_new2 <- 3    
            # current_loc_new2 <- 8
            
            # Reading current location 
            if ( current_loc_new2 != length(data_final_new2)  )
            {
              data_final_current_loc_new2 <- (data_final_new2[[current_loc_new2]])$.attrs
            } else {
              
              data_final_current_loc_new2 <- (data_final_new2[[current_loc_new2]])
            }
            
            
            
            
            
            # Converting to frame if possible
            data_final_current_loc_new2 <- data.frame(t(data_final_current_loc_new2))
            
            
            
            
            
            
            
            # Ordering reminder
            # belarus_longitude_lattitude$location_ascii[i],
            # belarus_longitude_lattitude$belarusian_name_without_voblast[i],
            # belarus_longitude_lattitude$russian_name_without_voblast[i],
            # belarus_longitude_lattitude$belarusian_latin_ascii[i],
            # belarus_longitude_lattitude$russian_latin_ascii[i],
            # belarus_longitude_lattitude$country_latin[i],
            # belarus_longitude_lattitude$country_cyrrilic[i])
            
            
            # if we are NOT in the final string    
            if ( !is.null(data_final_current_loc_new2$display_name) )
            {
              
              # Checking that the record is indeed from Belarus.
              # Either of the filters should satisfy
              indexes_belarus <- which( (grepl(pattern = place_name[length(place_name)],   x = data_final_current_loc_new2$display_name) +
                                           grepl(pattern = place_name[length(place_name)-1], x = data_final_current_loc_new2$display_name)) > 0 )
              
              cat("Location -> ", place_name[1], " <- for Belarus for current_loc ->", current_loc,  " in THIRD xml status -> ", length(indexes_belarus), "\n" )
              cat("Record:\n" )
              print(data_final_current_loc_new2$display_name[indexes_belarus] )
              
              
              cat("Location -> ", place_name[1], " <- Filteting by Region!\n" )
              
              
              # Checking that display name is of interest has mathcing oblast
              index_belarus_region <- which( (grepl(pattern = place_name[2], x = data_final_current_loc_new2$display_name[indexes_belarus]) +
                                              grepl(pattern = place_name[3], x = data_final_current_loc_new2$display_name[indexes_belarus]) +
                                              grepl(pattern = place_name[4], x = data_final_current_loc_new2$display_name[indexes_belarus]) +
                                              grepl(pattern = place_name[5], x = data_final_current_loc_new2$display_name[indexes_belarus])) > 0 )
              
              
              if (length(index_belarus_region) > 0) {
                
                cat("Location -> ", place_name[1], " <- found!\n" )
                lat <- data_final_current_loc_new2$lat[ indexes_belarus[index_belarus_region] ]
                lon <- data_final_current_loc_new2$lon[ indexes_belarus[index_belarus_region] ]
                cat("Extract  -> lat ->", lat, "long ->", lon, "\n\n\n" )
                
                return(c(lat, lon))
              } else {
                
                cat("Location -> ", place_name, " <- has not been found!\n\n\n" )
                # return(c(NA, NA))
              }
              
            # End of -> if ( !is.null(data_final_current_loc_new2$display_name) )
            }
            
            

            
            # if we are in the final string    
            if ( !is.null(data_final_current_loc_new2$display_name) )
            {
              cat("Location -> ", place_name, " <- has not been found ANYWHERE!\n\n\n" )
              
              return(c(NA, NA))
              # End of -> if ( !is.null(data_final_current_loc$display_name) )  
            }
            
            
            
            
            
          # End of -> for( current_loc_new2 in c(1:length(data_final_new2)) )
          } 
          
          
          

        # End of -> if ( !is.null(data_final_current_loc_new$display_name) )  
        }
        
        
        
        
        
      # End of -> for( current_loc_new in c(1:length(data_final_new)) )    
      } 
      
      
      
      
    
      
        
    # End of -> if ( is.null(data_final_current_loc$display_name) )  
    }
    
    
    
    
    
    
    
  # End of -> for( current_loc in c(1:length(data_final)) )  
  }  
    

  

  
}







sink(paste0("../R_Output/part01_06_reading_cities_fixing_names_part02.txt"))
ls()


# Looping to double check
for( i in c(1:dim(belarus_longitude_lattitude)[1]) )
{
  # Debugging
  # i <- 1
  # i <- 9
  
  
  # Extracting current location
  current_location <- c(belarus_longitude_lattitude$location_ascii[i],
                        belarus_longitude_lattitude$belarusian_name_without_voblast[i],
                        belarus_longitude_lattitude$russian_name_without_voblast[i],
                        belarus_longitude_lattitude$belarusian_latin_ascii[i],
                        belarus_longitude_lattitude$russian_latin_ascii[i],
                        belarus_longitude_lattitude$country_latin[i],
                        belarus_longitude_lattitude$country_cyrrilic[i])
  
  
  cat("\n\n\n\nCurrent Location which is porcessed is -> | ", current_location, " | with number", i, "out of",  dim(belarus_longitude_lattitude)[1], "\n" )
  
  # Extracting current coordinates
  current_coordinates_extract <- get_coordinates(current_location)
  
  # Assigning current coordinates  
  belarus_longitude_lattitude$latitude_fixed[i]   <- current_coordinates_extract[1]
  belarus_longitude_lattitude$longitude_fixed[i] <-  current_coordinates_extract[2]
  
  
# End of -> for( i in dim(belarus_longitude_lattitude)[1] )  
}  



sink()





which_lat_missing <- which(is.na(belarus_longitude_lattitude$latitude_fixed))
which_lon_missing <- which(is.na(belarus_longitude_lattitude$longitude_fixed))

which_missing <- union(which_lat_missing, which_lon_missing)

# Fixing by hand the remaining 8 records
belarus_longitude_lattitude$location_ascii[which_missing]
belarus_longitude_lattitude[which_missing,]

# Fixing by hand
belarus_longitude_lattitude$latitude_fixed[which_missing]  <- belarus_longitude_lattitude$latitude_hand[which_missing]
belarus_longitude_lattitude$longitude_fixed[which_missing] <- belarus_longitude_lattitude$longitude_hand[which_missing]



# BELRIEM

# Removing segment
belriem_data <- belriem_data[, (!names(belriem_data) %in% c("segment")) ]


# Fixing names
# Minsk
which_location_na_and_division_minsk <- which(  (is.na(belriem_data$location)) + (belriem_data$division == "Minsk")  == 2 )
belriem_data$location[which_location_na_and_division_minsk] <- "Minsk"
table(belriem_data$location)
# Warsaw
which_location_na_and_division_warsaw <- which(  (is.na(belriem_data$location)) + (belriem_data$division == "Warsaw")  == 2 )
belriem_data$location[which_location_na_and_division_warsaw] <- "Warsaw"
table(belriem_data$location)


# Conversions
belriem_data$strain_ascii <- stri_trans_general( str = belriem_data$strain, id  = "Latin-ASCII" )
belriem_data$strain_fix_ascii <- stri_trans_general( str = belriem_data$strain_fix, id  = "Latin-ASCII" )
belriem_data$strain_plus_date_ascii <- stri_trans_general( str = belriem_data$strain_plus_date, id  = "Latin-ASCII" )
belriem_data$strain_fix_plus_date_ascii <- stri_trans_general( str = belriem_data$strain_fix_plus_date, id  = "Latin-ASCII" )


# Conversions
belriem_data$division_ascii <- stri_trans_general( str = belriem_data$division, id  = "Latin-ASCII" )
belriem_data$location_ascii <- stri_trans_general( str = belriem_data$location, id  = "Latin-ASCII" )

# Extra check
table(belriem_data$division, belriem_data$division_ascii)
table(belriem_data$location, belriem_data$location_ascii)

sum(diag(table(belriem_data$division, belriem_data$division_ascii)))  ==  dim(belriem_data)[1]
sum(diag(table(belriem_data$location, belriem_data$location_ascii)))  ==  dim(belriem_data)[1]

sum( !as.matrix(table(belriem_data$division, belriem_data$division_ascii)) == 
       t(as.matrix(table(belriem_data$division, belriem_data$division_ascii))) )

sum( !as.matrix(table(belriem_data$location, belriem_data$location_ascii)) == 
       t(as.matrix(table(belriem_data$location, belriem_data$location_ascii))) )





which_division_different <- which(belriem_data$division != belriem_data$division_ascii)
which_location_different <- which(belriem_data$location != belriem_data$location_ascii)

# Extra check for the subset
table(belriem_data$division[which_division_different], belriem_data$division_ascii[which_division_different])
table(belriem_data$location[which_location_different], belriem_data$location_ascii[which_location_different])

sum( !as.matrix(table(belriem_data$division[which_division_different], belriem_data$division_ascii[which_division_different])) == 
     t(as.matrix(table(belriem_data$division[which_division_different], belriem_data$division_ascii[which_division_different]))) )

sum( !as.matrix(table(belriem_data$location[which_location_different], belriem_data$location_ascii[which_location_different])) == 
     t(as.matrix(table(belriem_data$location[which_location_different], belriem_data$location_ascii[which_location_different]))) )






# Merging coordinates
list_to_remove_in_merge <- c("latitude", "longitude", "latitude_hand", "longitude_hand", "belarusian_latin_ascii", "russian_latin_ascii", "belarusian_name_cyrillic")

# Fix 2024.09.16.
# Keeping UNIQUE ROWS only
# Part 1: Only needed columns
belarus_longitude_lattitude <- belarus_longitude_lattitude[, -which(names(belarus_longitude_lattitude) %in% list_to_remove_in_merge)] 
# Part 2: Only UNIQUE rows
dim(belarus_longitude_lattitude)
belarus_longitude_lattitude <- unique(belarus_longitude_lattitude)
dim(belarus_longitude_lattitude)


# Adding coordinates
belriem_data_plus_coordinates <- 
      base::merge( x = belriem_data, 
                   y = belarus_longitude_lattitude, 
                   by = "location_ascii", 
                   all.x = TRUE, 
                   all.y = FALSE )

dim(belriem_data_plus_coordinates)
sum(is.na(belriem_data_plus_coordinates$latitude_fixed))
sum(is.na(belriem_data_plus_coordinates$longitude_fixed))




# Fix 2024.09.16.
# Checking that ROWS are unique
dim(belriem_data_plus_coordinates)
belriem_data_plus_coordinates <- unique(belriem_data_plus_coordinates)
dim(belriem_data_plus_coordinates)




# Exporting table
belriem_data_plus_coordinates_path_csv <- paste0("../R_Output/part01_06_reading_cities_fixing_names_belriem_data_plus_coordinates.csv")
write.table(x = belriem_data_plus_coordinates, file = belriem_data_plus_coordinates_path_csv, sep = ",", quote = TRUE, row.names = FALSE, fileEncoding="UTF-8")


# Exporting table
belriem_data_plus_coordinates_path_xlsx <- paste0("../R_Output/part01_06_reading_cities_fixing_names_belriem_data_plus_coordinates.xlsx")
write.xlsx(x = belriem_data_plus_coordinates, file = belriem_data_plus_coordinates_path_xlsx, sheetName="Sheet1")


