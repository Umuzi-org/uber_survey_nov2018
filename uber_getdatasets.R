##### SCRIPT FOR IMPORTING DATA FROM UBER SURVEYS ######
##### Collected at GLH Joburg and Uber Expo TicketPro Dome Joburg ######

#updated: 10 December 2018
# Michelle Hoogenhout


library(tidyverse) #data wrangling
library(magrittr) #pipe
library(readxl) #for excel files

source("importdata.R")

##### define factor levels ###############################################

#define order of factor levels
OrderLengthStudy<- c("Less than 6 months", "6 months - 1 year", "1 - 2 years",
                     "3 - 5 years", "More than 5 years")

OrderTimeCommitment<- c("1 - 2 hours", "3 -5 hours", "6 - 10 hours",
                        "10 - 15 hours", "16 - 20 hours", "More than 20 hours")

OrderFreqAttendance<- c("Every weekday", "Twice a week", "Once a week", "Twice a month", "Once a month")

OrderPreferredCost <- c("I'm not willing to pay for studying", "Less than R200", 
                        "R200 - R499", "R500 - R999", "R1000 - R1999", "R2000 - R2999")


############## combine all variants of currntly studying ####################
studying_list_glh = c("Currently busy with studying", 
                      "currently studying", 
                      "Currently studying", 
                      "Studies in progress",
                      "I am currently busy studying at college")

studying_list_expo = c("Still studying", 
                       "Still in progress")

############ functions ####################################################

#get laptop and personal computer separately from list
pclaptop <- function(data){
 data %<>% mutate(
  laptop=ifelse(grepl("Personal computer", ResourceAccess), 1, 0),
  HomeInternet=ifelse(grepl("Internet", ResourceAccess), 1, 0),
  FriendsPC=ifelse(grepl("friend or family member's computer", ResourceAccess), 1, 0))
 
 return(data)
}

#change Studied Previously frm yes to Currently Studying
studycompletion <- function(data, input){
 if (input == "GLH"){
  levels(data$StudiedPreviously) <- c("No", "Yes", "Currently Studying")
  data[which(data$ReasonNotCompleting == "Currently Studying"),
       ]$StudiedPreviously <- "Currently Studying"
 }
 
 else if (input == "expo"){
  levels(data$StudiedPreviously) <- c("No", "No", "Yes", "Currently Studying")
  data[which(data$ReasonNotCompleting == "Currently Studying"),
       ]$StudiedPreviously <- "Currently Studying"
 }
 
 else
  return("unrecognised input")
 
 return(data)
}

################ get  data  #################################################

#import survey data
glhdata <- importdata("Umuzi Driver Education Revised.xlsx", "GLH")
expodata <- importdata("UMUZI2Uber Driver-Partner Survey 2.0 (Responses).xlsx", "expo")

#revalue internet access
glhdata$InternetAccess <- plyr::revalue(glhdata$InternetAccess, 
                                        c("I don't have any internet access when not operating on Uber"="None",
                                          "Internet at home"="WiFi at home",
                                          "Internet Cafe" = "Internet Cafe",
                                          "With your cellphone data" = "Mobile data"))

expodata$InternetAccess <- plyr::revalue(expodata$InternetAccess, 
                                         c("I don't have any internet access when not operating on Uber"="None",
                                           "Internet at home (WiFi or ADSL)"="WiFi at home",
                                           "Internet Cafe" = "Internet Cafe",
                                           "With your cellphone data" = "Mobile data",
                                           "Free internet from a community centre" = "Community Centre"))

#get PC & laptop columns (1 == yes)
glhdata <- pclaptop(glhdata)
expodata <- pclaptop(expodata)

#change completed = No to completed = Currently studying for those still in program
glhdata <- studycompletion(glhdata, "GLH")
expodata <- studycompletion(expodata, "expo")

#combine data
 #rearrange order of expodata
 #drop PassedMatric
 temp <- expodata %>% 
  dplyr::select(id, DailyTimeCommitment, everything()) %>% 
  dplyr::select(-PassedMatric, -Timestamp) 

 #change weekly commitment to daily commitment
 glhdata %<>% mutate(
  DailyTimeCommitment = ifelse(WeeklyTimeCommitment == "6 - 10 hours", "1 hour",
                              ifelse(WeeklyTimeCommitment == "10 - 15 hours", "2 hours",
                                     ifelse(WeeklyTimeCommitment == "16 - 20 hours", "3 hours",
                                            ifelse(WeeklyTimeCommitment == "More than 20 hours", 
                                                   "4 or more hours","Less than an hour"))))
 ) %>% 
  dplyr::select(-WeeklyTimeCommitment) %>% 
  dplyr::select(id, DailyTimeCommitment, everything())

 #merge
 alldata <- rbind(glhdata, temp)
 
#################### subset those willing to study ##################################

#subset only those willing to study
willstudy_glh <- glhdata[which(glhdata$WillingnessStudy == "Yes"),]
willstudy_expo <- expodata[which(expodata$WillingnessStudy == "Yes"),]
willstudy_all <- alldata[which(alldata$WillingnessStudy == "Yes"),]


##################  export #########################################################

write.csv(willstudy_all, "clean/willstudy_all.csv", row.names = F)
write.csv(willstudy_glh, "clean/willstudy_glh.csv", row.names = F) 
write.csv(willstudy_expo, "clean/willstudy_expo.csv", row.names = F)

write.csv(alldata, "clean/alldata.csv", row.names = F)
write.csv(glhdata, "clean/glhdata.csv", row.names = F) 
write.csv(expodata, "clean/expodata.csv", row.names = F)

