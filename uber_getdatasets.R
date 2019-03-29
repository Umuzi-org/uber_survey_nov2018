##### SCRIPT FOR IMPORTING DATA FROM UBER SURVEYS ######
##### Collected at GLH Joburg and Uber Expo TicketPro Dome Joburg ######

#updated: 10 December 2018
# Michelle Hoogenhout


library(tidyverse) #data wrangling
library(magrittr) #pipe
library(readxl) #for excel files

source("importdata.R")
source("revalue_internet.R")

## data location
GLHDATA = "raw_data/Umuzi Driver Education Revised.xlsx"
EXPODATA = "raw_data/UMUZI2Uber Driver-Partner Survey 2.0 (Responses).xlsx"
ONLINE_SURVEY = "raw_data/Uber DriverPartner Survey Powered by Umuzi.csv"

##### define factor levels ###############################################

#define order of factor levels
OrderLengthStudy<- c("Less than 6 months", "6 months - 1 year", "1 - 2 years",
                     "3 - 5 years", "More than 5 years")

OrderTimeCommitment<- c("Less than an hour", "1 hour", "2 hours",
                        "3 hours", "4 or more hours")

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
datalist <- list(
 "GLH" = GLHDATA, 
 "expo" = EXPODATA,
 "online" = ONLINE_SURVEY
)


# for (input in len(datalist)){
#  
# } 

#import survey data
glhdata <- importdata(GLHDATA, "GLH")
expodata <- importdata(EXPODATA, "expo")
onlinedata <- importdata(ONLINE_SURVEY, "online")


glhdata <- revalue_internet(glhdata, "GLH")
expodata <- revalue_internet(expodata, "expo")
onlinedata <- revalue_internet(onlinedata, "online")


#get PC & laptop columns (1 == yes)
glhdata <- pclaptop(glhdata)
expodata <- pclaptop(expodata)
onlinedata <- pclaptop(onlinedata)

#change completed = No to completed = Currently studying for those still in program
glhdata <- studycompletion(glhdata, "GLH")
expodata <- studycompletion(expodata, "expo")
#TODO: change onlinedata

#combine data
 #rearrange order of expodata
 #drop PassedMatric
 temp1 <- expodata %>% 
  dplyr::select(id, DailyTimeCommitment, everything()) %>% 
  dplyr::select(-PassedMatric, -Timestamp)
 
 temp2 <- onlinedata %>% 
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

 #merge and define factor levels
 alldata <- rbind(glhdata, temp1, temp2) %>% 
  mutate(LengthWillingStudy = factor(LengthWillingStudy, levels = OrderLengthStudy),
         AttendanceFrequency = factor(AttendanceFrequency, levels = OrderFreqAttendance),
         PreferredCost = factor(PreferredCost, levels = OrderPreferredCost),
         DailyTimeCommitment = factor(DailyTimeCommitment, levels = OrderTimeCommitment),
         Skills = toupper(trimws(Skills)))
 
 #mark which sample participants came from
 alldata$sample <- ifelse(alldata$id %in% c(1:999), "GLH", 
                          ifelse(alldata$id %in% c(1000:1999), "expo", "online"))
 
#################### subset those willing to study ##################################

#subset only those willing to study
willstudy_glh <- glhdata[which(glhdata$WillingnessStudy == "Yes"),]
willstudy_expo <- expodata[which(expodata$WillingnessStudy == "Yes"),]
willstudy_online <- onlinedata[which(onlinedata$WillingnessStudy == "Yes"),]
willstudy_all <- alldata[which(alldata$WillingnessStudy == "Yes"),]

#clean
rm(temp1, temp2)
