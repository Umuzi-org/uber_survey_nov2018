importdata <- function(filename, input){
  #' filename = name of excel file
  #' input = collection location, either "GLH" or "expo"
  #' 
   if (grepl("xlsx", filename, fixed = "True")){
    data <- readxl::read_xlsx(filename)
  } else if (grepl("csv", filename, fixed = "True")){
   data <- read_csv(filename)
   } else {
   print("file format not recognised")
  }
 
  
  ############## combine all variants of currntly studying ####################
  studying_list_glh = c("Currently busy with studying", 
                        "currently studying", 
                        "Currently studying", 
                        "Studies in progress",
                        "I am currently busy studying at college")
  
  studying_list_expo = c("Still studying", 
                         "Still in progress")
  
  motivation_expo = c("I did not feel motivated to complete the course",
                      "I worked for a company that deals with electronics not heavy current as my dream is become an artisan",
                      "Outstanding training ti get qualification")
  
  ############################################################################
  
  #rename and create factors, defining order and collapsing levels of currently studying
  data %<>% dplyr::rename(
    WillingnessStudy = "Are you willing to study part-time while operating on Uber?",
    LengthWillingStudy = "For how long would you be willing to study while operating on Uber?",
    Skills = "Which skills would you most like to learn? Select up to three skills.",
    DesiredOutcome = "What is most important to you at the end of studying?",
    AttendanceFrequency = "How often would you be able to attend classes, in-person, at a local campus in your city?",
    PreferredClassFormat = "What format would you prefer for the classes?",
    InternetAccess = "How do you access the internet when you not operating on Uber?",
    PreferredCost = "How much would you be willing to pay monthly for your part-time studying?",
    SwitchPosition = "If you were given the opportunity to start a new career, would you be willing to start in a junior position?",
    StudySubject = "If \" yes\", what did you study?",
    CompletedStudies = "Did you complete your studies?",
    ReasonNotCompleting = "What prevented you from completing your studies?") %>% 
    mutate_all(funs(factor))
  
  if (input == "GLH"){
    data %<>% dplyr::rename(
      id = "Response ID",
      WeeklyTimeCommitment = "How much time can you commit on a weekly basis to studying?",
      StudiedPreviously = "Have you studied before?",
      FocusGroup = "Would you be willing to attend a focus group to talk about your goals for further education and skills?",
      ResourceAccess = "I have access to") %>% 
      mutate(ReasonNotCompleting = fct_collapse(ReasonNotCompleting, 
                                                `Currently Studying` = studying_list_glh))
    
  } else if (input == "expo" | input == "online"){
    data %<>% dplyr::rename(
      DailyTimeCommitment = "How much time can you commit on a DAILY basis to studying?",
      PassedMatric = "Did you pass Grade 12 (or an equivalent such as A-levels)?",
      StudiedPreviously = "Did you study after high school?",
      FocusGroup = "Would you be willing to attend a focus group to talk about your goals for further education and  skills?")

    
    #make sure forms weren't submitted twice
    data <- data[!duplicated(data),]
    
    #anonymise
    drop <- c("Cellphone Number ( as per your Uber profile)",
              "Email address ( as per your Uber profile)")
    data <- data[ , !(names(data) %in% drop)]
    
     if (input == "expo"){
      data %<>% dplyr::rename(ResourceAccess = "I have access to") %>% 
       mutate(ReasonNotCompleting = fct_collapse(ReasonNotCompleting, 
                                           `Currently Studying` = studying_list_expo,
                                           `No motivation` = motivation_expo))
      #add unique number
      data$id <- c(1:nrow(data))
      data$id <- as.factor(1000 + data$id) #make the id different from glh id
      
     } else if (input == "online"){
      data %<>% dplyr::rename(ResourceAccess = "I have access to... (Please select all that apply)")
      
      #add unique number
      data$id <- c(1:nrow(data))
      data$id <- as.factor(2000 + data$id) #make the id different from glh and expo id
     }
    
  } else {
    return("sample not recognised")
  }
  
  return(data)
  
}