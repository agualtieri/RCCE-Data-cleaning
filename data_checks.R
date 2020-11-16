## RCCE Assessment - Data Cleaning Script - Data checks
## alberto.gualtieri@reach-initiative.org 
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
require(stringr)
require(lubridate)
require(plyr)


## Load sources
source("./R/moveme.R")
source("./R/check_time.R")



## Upload data to be cleaned  - load the latest file that needs to be cleaned
data <- read.xlsx("./input/UGA2002a_-_latest_version_-_False_-_2020-09-21-03-43-15.xlsx")
names(data)[names(data) == "_index"] <- "index"
names(data)[names(data) == "_uuid"] <- "uuid"

## Replace "/" with "." in col headers -- can be commented if data downloaded appropriatly from kobo server
colnames(data) <- gsub("/", ".", colnames(data))



### Enumerators' behaviour checks
## Check survey time. Limits for Refugees are: 30-70 and for Hosts 20-50
ref_timecheck <- data %>% filter(status == "yes") %>% check_time(30, 70)
host_timecheck <- data %>% filter(status == "no") %>% check_time(20, 50)

all_timecheck <- rbind(ref_timecheck, host_timecheck)
all_timecheck <- merge(x = all_timecheck, y = data[ , c("uuid", "today")], by = "uuid", all.x=TRUE)


if(nrow(all_timecheck) >= 1){
  
  all_timecheck$fix <- "Checked with enumerator"
  all_timecheck$checked_by <- "AG"
  all_timecheck$delete <- ""

  
  check_time_log <- data.frame(uuid = all_timecheck$uuid,
                               date = all_timecheck$today,
                               enumerator = all_timecheck$enumerator,
                               area = all_timecheck$area, 
                               settlement = all_timecheck$settlemet,
                               status = all_timecheck$status,
                               variable = all_timecheck$variable, 
                               issue = all_timecheck$issue_type, 
                               value = all_timecheck$value, 
                               fix = all_timecheck$fix, 
                               delete = all_timecheck$delete,
                               checked_by = all_timecheck$checked_by)
  
} else {
  
  check_time_log <- data.frame(uuid = as.character(),
                               date = as.character(),
                               enumerator = as.character(),
                               area = as.character(),
                               settlemet = as.character(),
                               status = as.character(),
                               variable = as.character(),
                               issue = as.character(),
                               value = as.character(),
                               fix = as.character(),
                               delete = as.character(),
                               checked_by = as.character())
  
  
  print("The lenghts of the survey are within acceptable values. No cleaning needed.") 
}

## Check for shortest path
data$CountNa <- rowSums(apply(is.na(data), 2, as.numeric))

shortest_path <- data %>% select("uuid", "enumerator", "district_name", "status", "CountNa")
shortest_path <- shortest_path %>% filter(CountNa > 280)

shortest_path <- merge(x = shortest_path, y = data[ , c("uuid", "today")], by = "uuid", all.x=TRUE)

if(nrow(shortest_path)>=1) {
  
  shortest_path$issue_type <- "The majority of entries are NAs"
  shortest_path$checked_by <- "AG"
  shortest_path$fix <- "Checked with enumerator"
  shortest_path$variable <- "Count of all variables"
  shortest_path$delete <- ""
  
  
  
  shortest_path_log <- data.frame(uuid = shortest_path$uuid, 
                                  date = shortest_path$today,
                                  enumerator = shortest_path$enumerator,
                                  area = shortest_path$district_name,
                                  status = shortest_path$status,
                                  variable = shortest_path$variable,
                                  issue = shortest_path$issue_type, 
                                  value = shortest_path$CountNa, 
                                  fix = shortest_path$fix, 
                                  delete = shortest_path$delete,
                                  checked_by = shortest_path$checked_by)
  
  
} else {
  
  shortest_path_log <- data.frame(uuid = as.character(),
                                  date = as.character(),
                                  enumerator = as.character(),
                                  area = as.character(),
                                  status = as.character(),
                                  variable = as.character(),
                                  issue = as.character(),
                                  value = as.character(),
                                  fix = as.character(),
                                  delete = as.character(),
                                  checked_by = as.character()) 
  
  print("No enumerators seems to have taken the shortest path")
}

## Check number of surveys per enumerator
n_surveys <- data %>% select("district_name", "refugee_settlement", "start", "end", "enumerator") %>% 
                                 separate(start, c("start_date", "start_time"), "T") %>% separate(end, c("end_date", "end_time"), "T")

n_surveys <- n_surveys %>% select(district_name, refugee_settlement, start_date, enumerator) %>% 
                                             dplyr::group_by(start_date, enumerator) %>% dplyr::mutate(n_surveys = n()) %>% 
                                                                      mutate(issue=ifelse((n_surveys < 6), "less than 6 surveys", "no issue")) %>% filter(n_surveys <6)
                                                                


                                                           

if(nrow(n_surveys)>=1) {
  

  n_surveys$checked_by <- "AG"
  n_surveys$fix <- "Checked with enumerator"
  n_surveys$variable <- "Number of surveys per day"
  n_surveys$status <- "NA"
  n_surveys$uuid <- "NA"
  
  n_surveys_log <- data.frame(uuid = n_surveys$uuid, 
                                  date = n_surveys$start_date,
                                  enumerator = n_surveys$enumerator,
                                  district = n_surveys$district_name,
                                  settlement = n_surveys$refugee_settlement,
                                  status = n_surveys$status,
                                  variable = n_surveys$variable,
                                  issue = n_surveys$issue, 
                                  value = n_surveys$n_surveys, 
                                  fix = n_surveys$fix, 
                                  checked_by = n_surveys$checked_by)
  
  
} else {
  
  n_surveys_log <- data.frame(uuid = as.character(),
                                  date = as.character(),
                                  enumerator = as.character(),
                                  district = as.character(),
                                  settlement = as.character(),
                                  status = as.character(),
                                  variable = as.character(),
                                  issue = as.character(),
                                  value = as.character(),
                                  fix = as.character(),
                                  checked_by = as.character()) 
  
  print("All enumerators have met their daily quota")
}             

## Bind All
enumerator_checks <- rbind.fill(check_time_log, shortest_path_log) 

# write.xlsx(enumerator_checks, paste0("./output/rcce_enumerators_check_",today,".xlsx"))


### Data quality checks

## Check 1: How often the respondent received COVID-related info in the past 2 months and when was the last communication
covid_comm <- data %>% select(uuid, enumerator, district_name, comm_freq, last_comm_covid) %>% 
              mutate(comm_issue = ifelse(comm_freq == "daily" | comm_freq == "at_least_once_week" & last_comm_covid == "past_24h" | last_comm_covid == "past_7_days", 0, 1)) %>%
              mutate(comm_issue = ifelse(last_comm_covid == "do_not_remember", 0, comm_issue)) %>% filter(comm_issue == 1)

if(nrow(covid_comm)>=1) {
  
  
  covid_comm$checked_by <- "AG"
  covid_comm$issue <- "Issue between frequency and latest COVID-related communication"
  covid_comm$variable <- "Issue between comm_freq and last_comm_covid"
  covid_comm$var_to_change <- ""
  covid_comm$value_to_change <- ""
  covid_comm$fix <- "TRUE"
  
  
  covid_comm_log <- data.frame(uuid = covid_comm$uuid, 
                                enumerator = covid_comm$enumerator,
                                area = covid_comm$district_name,
                                variable = covid_comm$variable,
                                issue = covid_comm$issue,
                                var_to_change = covid_comm$var_to_change,
                                value_to_change = covid_comm$value_to_change,
                                fix = covid_comm$fix,
                                checked_by = covid_comm$checked_by
                               )
  
  
} else {
  
  covid_comm_log <- data.frame(uuid = as.character(),
                                enumerator = as.character(),
                                area = as.character(),
                                variable = as.character(),
                                issue = as.character(),
                                var_to_change = as.character(),
                                value_to_change = as.character(),
                                fix = as.character(),
                                checked_by = as.character()
                               ) 
  
  print("No communication-related issues")
}             


## Check 2: If respondent reported a predominant source of income he cannot report no access to livelihood opportunities
liveli_issue <- data %>% select(uuid, enumerator, district_name, economic_activity, livilihood_access) %>%
                        mutate(liveli_issue = ifelse(economic_activity != "none" & livilihood_access == "no" | livilihood_access == "no_answer", 1, 0)) %>% filter(liveli_issue == 1)

if(nrow(liveli_issue)>=1) {
  
  
  liveli_issue$checked_by <- "AG"
  liveli_issue$fix <- "TRUE"
  liveli_issue$issue <- "Issue between reported economic activity and access to livelihoods"
  liveli_issue$variable <- "Issue between economic_activity and livilihood_access"
  liveli_issue$var_to_change <- ""
  liveli_issue$value_to_change <- ""
  
  
  liveli_issue_log <- data.frame(uuid = liveli_issue$uuid, 
                                 enumerator = liveli_issue$enumerator,
                                 area = liveli_issue$district_name,
                                 variable = liveli_issue$variable,
                                 issue = liveli_issue$issue,
                                 var_to_change = liveli_issue$var_to_change,
                                 value_to_change = liveli_issue$value_to_change,
                                 fix = liveli_issue$fix, 
                                 checked_by = liveli_issue$checked_by)
  
  
} else {
  
  liveli_issue_log <- data.frame(uuid = as.character(),
                                 enumerator = as.character(),
                                 area = as.character(),
                                 variable = as.character(),
                                 issue = covid_comm$issue,
                                 var_to_change = as.character(),
                                 value_to_change = as.character(),
                                 fix = as.character(),
                                 checked_by = as.character()) 
  
  print("No livelihoods-related issues")
}


## Check 3: Issues with reporting barriers to reading and hearing but not reporting disabilities
barries_issue <- data %>% select(uuid, enumerator, district_name, chronic_illness_disease, difficulty_seeing, difficulty_hearing) %>%
                              mutate(barriers_issues = ifelse(chronic_illness_disease == "no" & (difficulty_hearing == "yes" | difficulty_seeing == "yes"), 1, 0)) %>% filter(barriers_issues == 1)


if(nrow(barries_issue)>=1) {
  
  
  barries_issue$checked_by <- "AG"
  barries_issue$fix <- "TRUE"
  barries_issue$issue <- "Issue between reported reported illness/disability and ability to see and hear"
  barries_issue$variable <- "Issue between chronic_illness_disease and difficulty_seeing or difficulty_hearing"
  barries_issue$var_to_change <- ""
  barries_issue$value_to_change <- ""
  
  
  barries_issue_log <- data.frame(uuid = barries_issue$uuid, 
                                    enumerator = barries_issue$enumerator,
                                    area = barries_issue$district_name,
                                    variable = barries_issue$variable,
                                    issue = barries_issue$issue,
                                    var_to_change = barries_issue$var_to_change,
                                    value_to_change = barries_issue$value_to_change,
                                    fix = barries_issue$fix, 
                                    checked_by = barries_issue$checked_by
                                  )
  
  
} else {
  
  barries_threat_log <- data.frame(uuid = as.character(),
                                    enumerator = as.character(),
                                    area = as.character(),
                                    variable = as.character(),
                                    issue = covid_comm$issue,
                                    var_to_change = as.character(),
                                    value_to_change = as.character(),
                                    fix = as.character(),
                                    checked_by = as.character()
                                   ) 
  
  print("No barriers-related issues")
}

## Check 4: Favorite communication channel is radio/tv but they do not have a radio or tv
comm_chan <- data %>% select(uuid, enumerator, district_name, inform_pref.radio, inform_pref.television, inform_barrier.limited_tv_access, inform_barrier.limited_radio_access) %>%  
                          mutate(comm_chan_issue = ifelse((inform_pref.radio == "1" & inform_barrier.limited_radio_access == "1") | (inform_pref.television == "1" & inform_barrier.limited_tv_access == "1"), 1, 0)) %>%
                          filter(comm_chan_issue == 1)

if(nrow(comm_chan)>=1) {
  
  
  comm_chan$checked_by <- "AG"
  comm_chan$fix <- "TRUE"
  comm_chan$issue <- "Issue between reported reported favorite channel of communication and access to such channel"
  comm_chan$variable <- "Issue between inform_pref.radio/television and inform_barrier.limited_tv/radio_access"
  comm_chan$var_to_change <- ""
  comm_chan$value_to_change <- ""
  
  
  comm_chan_log <- data.frame(uuid = comm_chan$uuid, 
                              enumerator = comm_chan$enumerator,
                              area = comm_chan$district_name,
                              variable = comm_chan$variable,
                              issue = comm_chan$issue,
                              var_to_change = comm_chan$var_to_change,
                              value_to_change = comm_chan$value_to_change,
                              fix = comm_chan$fix, 
                              checked_by = comm_chan$checked_by
                              )
  
} else {
  
  comm_chan_log <- data.frame(uuid = as.character(),
                                enumerator = as.character(),
                                area = as.character(),
                                variable = as.character(),
                                issue = covid_comm$issue,
                                var_to_change = as.character(),
                                value_to_change = as.character(),
                                fix = as.character(),
                                checked_by = as.character()) 
  
  print("No favourite information channel issues found")
}

### Interview Feedback
int_feedback <- data %>% select(interview_feedback, respondent_sex, respondent_age, nationality, nationality_other, status, district_name, refugee_settlement, 
                                feedback_details, corrective_measure, complainant_name, complainant_type, complainant_id, respondent_telephone, name_pers_recording, title_pers_recording,
                                feedback_note) %>% filter(interview_feedback == "yes")

write.xlsx(int_feedback, paste0("./output/rcce_interview_feedback_",today,".xlsx"))

## Bind all
cleaning_log <- rbind(covid_comm_log,
                      liveli_issue_log,
                      barries_issue_log,
                      comm_chan_log)
## Final
list <- list("Enumerators checks" = enumerator_checks,
             "Cleaning log" = cleaning_log,
             "Productivity" = n_surveys_log
             )

write.xlsx(list, paste0("./output/rcce_cleaning_log_",today,".xlsx"))
browseURL(paste0("./output/rcce_cleaning_log_",today,".xlsx"))

