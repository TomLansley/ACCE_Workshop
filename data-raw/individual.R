## code to prepare `individual` dataset goes here
## Setup ----
library(dplyr)
source(here::here("R", "geolocate.R")) 

## Combine indvidual tables ----
## Create paths to inputs
raw_data_path <- here::here("data-raw", "wood-survey-data-master")

#make a list of all the files in the specified directory
individual_paths <- fs::dir_ls(
  here::here(raw_data_path, "individual"))

## Read in all individual tables and combine them
individual <- purrr::map(.x = individual_paths,
                         ~ readr::read_csv(
                           .x,
                           col_types = readr::cols(.default = "c"), #if I know the column types, it can speed things up
                           show_col_types = FALSE)
) %>% 
  purrr::list_rbind() %>% 
  readr::type_convert() #converts column classes, because we specified character

#write the output to file
individual %>% 
  readr::write_csv(
    file = fs::path(raw_data_path, "vst_individuals.csv") 
  )

## Combine NEON data tables ----
#read in tables
maptag <- readr::read_csv(
  fs::path(raw_data_path, "vst_mappingandtagging.csv")
) %>% 
  select(-eventID)

perplot <- readr::read_csv(
  fs::path(raw_data_path, "vst_perplotperyear.csv")
)%>% 
  select(-eventID)

#left join the tables to individual data
individual %<>% # %<>% assigns the result to the original 
  left_join(maptag, by = "individualID",
            suffix = c("", "_map")) %>% #instead of naming uid columns into uid.x and uid.y, keep indivual$uid the same and maptag$uid uid_map
  left_join(perplot, by = "plotID",
            suffix = c("", "_ppl")) %>% 
  assertr::assert(
    assertr::not_na, stemDistance, stemAzimuth, pointID, #stops if there are NAs in these columns and shows where
    decimalLatitude, decimalLongitude
  )
#assertr doesn't say anything if the checks are passed
#if there were already NAs in these columns, this check wouldn't work

## Geolocate individuals ----
#create new columns in our dataframe
#use our function to produce the values
individual <- individual %>% mutate(
    stemLat = get_stem_location(
      decimalLongitude, decimalLatitude, stemAzimuth, stemDistance
    )$lat, #we want $lat from the output of the function
    stemLon = get_stem_location(
      decimalLongitude, decimalLatitude, stemAzimuth, stemDistance
    )$lon
  ) %>% 
  janitor::clean_names() #edits variable names to separate words with underscores

#create a data directory
fs::dir_create("data") #won't change anything if run again
#write data to data directory
individual %>% 
  readr::write_csv(
    here::here("data", "individual.csv")
  )


###Data spice ----
#meta data stuff
library(dataspice)
library(dplyr)


#creates a meta data folder with four csv files
create_spice()
edit_creators() #opens a shiny app popup where I can input data to the creators csv
prep_access() #finds the individual file and adds its metadata to the access file
edit_access() #open shiny app to see/edit the data inputted

#read our data in again
individual <- readr::read_csv(
  here::here("data","individual.csv")
)
#look at range of dates, spatial extent etc
range(individual$date)
range(individual$decimal_latitude)
range(individual$decimal_longitude)
range(individual$domain_id)
unique(individual$domain_id)
"Neon Domain areas D01:D09"
#copy these data into the biblio file
edit_biblio()
#use wktString or the four coords, don't need both

#automatically add variable names to the attributes csv file
prep_attributes()

#read in the attributes file
attributes <- readr::read_csv(
  here::here("data", "metadata", "attributes.csv")
) %>% 
  select(-description, -unitText) #don't want these empty variables as we will use the other file

#there is already a file that describes some of the variables - use this to add info
#before combining, some names need to match up because they're in different formats
variables <- read.csv("./data-raw/wood-survey-data-master/NEON_vst_variables.csv") %>%
  rename("variableName" = fieldName) %>% 
  rename("unitText" = units) %>% 
  mutate(variableName = janitor::make_clean_names(variableName)) %>% 
  select(variableName, description, unitText)

left_join(attributes, variables,
          by = "variableName") %>% 
  readr::write_csv(
    file = here::here("data", "metadata", "attributes.csv")
  )

#then view/edit the file
edit_attributes() 
#we created stem_lat and stem_long so they (and a couple of others)
# are missing descriptions

# create json-ld file
#this allows google to add it to their searches
#it's machine readable
write_spice()

#now create a more readable website for the metadata
build_site(out_path = "data/index.html")






#usethis::use_data(individual, overwrite = TRUE)
