## RCCE Assessment - Data Cleaning Script - Data cleaning
## V1
## 25/08/2020

rm(list=ls())

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
data <- read.csv("./input/RCCE_survey_-_latest_version_-_False_-_2020-08-31-07-59-54.csv")
names(data)[names(data) == "X_index"] <- "index"
names(data)[names(data) == "X_uuid"] <- "uuid"

## Load cleaning logs and enumerators checks
cleaning_log <- read.xlsx("./output/rcce_cleaning_log_2020-08-31.xlsx", sheet = "Cleaning log")

lst <- lapply(1:2, function(i) read.xlsx("./output/rcce_cleaning_log_2020-08-31.xlsx", 
                                          sheet = i))
enumerators_checks <- lst[[1]]
cleaning_log <- lst[[2]]

### Clean data

## Delete surveys from dataset and cleaning log
enumerators_checks <- enumerators_checks %>% filter(delete == TRUE)

data_red <- data[!(data$uuid %in% enumerators_checks$uuid),]
cleaning_log_red <- cleaning_log[!(cleaning_log$uuid %in% enumerators_checks$uuid),]



## Apply cleaning log
mylog <- cleaninglog(ids = cleaning_log_red$uuid,
                     variables = cleaning_log_red$var_to_change,
                     new_values = cleaning_log_red$value_to_change,
                     change = cleaning_log_red$fix,
                     data_id_column_name = "uuid")


data_clean <- clog_clean(data_red, mylog)
