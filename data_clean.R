## RCCE Assessment - Data Cleaning Script - Data cleaning
## V1
## 25/08/2020

rm(list=ls())

today <- Sys.Date()

## Download necessary packages
# devtools::install_github("mabafaba/clog", build_opts = c(), force = TRUE)
# install.packages("tidyverse")
# install.packages("openxlsx")
# install.packages("stringr")
# install.packages("lubridate")

## Load libraries
require(tidyverse)
require(openxlsx)
require(clog)


## Load data to clean
data <- read.xlsx("./input/RCCE_survey_-_latest_version_-_False_-_2020-09-03-06-57-41.xlsx")
names(data)[names(data) == "_index"] <- "index"
names(data)[names(data) == "_uuid"] <- "uuid"

## Load cleaning logs and enumerators checks
lst <- lapply(1:2, function(i) read.xlsx("./output/rcce_cleaning_log_2020-09-03.xlsx", 
                                          sheet = i))
enumerators_checks <- lst[[1]]
cleaning_log <- lst[[2]]


### Clean data

## Delete surveys from dataset and cleaning log
enumerators_checks <- enumerators_checks %>% dplyr::filter(delete == "TRUE")

data_red <- data[!(data$uuid %in% enumerators_checks$uuid),]
cleaning_log_red <- cleaning_log[!(cleaning_log$uuid %in% enumerators_checks$uuid),]

## Convert all numeric cols into char before cleaning
char_columns <- sapply(data_red, is.numeric)             
data_red[ , char_columns] <- as.data.frame(apply(data_red[ , char_columns], 2, as.character))

sapply(data_red, class)


## Apply cleaning log
mylog <- cleaninglog(ids = cleaning_log_red$uuid,
                     variables = cleaning_log_red$var_to_change,
                     new_values = cleaning_log_red$value_to_change,
                     change = cleaning_log_red$fix,
                     data_id_column_name = "uuid")


data_clean <- clog_clean(data_red, mylog)

## Convert factors to numeric
# char_columns <- sapply(data_clean, is.factor)             
# data_clean[ , char_columns] <- as.data.frame(apply(data_clean[ , char_columns], 2, as.numeric))

# sapply(data_clean, class)

## Save and open final document
write.xlsx(data_clean, paste0("./output/rcce_clean_dataset_",today,".xlsx"))
browseURL(paste0("./output/rcce_clean_dataset_",today,".xlsx"))


