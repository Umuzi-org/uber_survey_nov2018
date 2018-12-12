importdata <- function(filename, input){
  #' filename = name of excel file
  #' input = collection location, either "GLH" or "expo"
  data <- readxl::read_xlsx(filename)
  
  
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
  data %<>% rename(
    WillingnessStudy = "Are you willing to study part-time while operating on Uber?",
    LengthWillingStudy = "For how long would you be willing to study while operating on Uber?",
    Skills = "Which skills would you most like to learn? Select up to three skills.",
    DesiredOutcome = "What is most important to you at the end of studying?",
    AttendanceFrequency = "How often would you be able to attend classes, in-person, at a local campus in your city?",
    PreferredClassFormat = "What format would you prefer for the classes?",
    ResourceAccess = "I have access to",
    InternetAccess = "How do you access the internet when you not operating on Uber?",
    PreferredCost = "How much would you be willing to pay monthly for your part-time studying?",
    SwitchPosition = "If you were given the opportunity to start a new career, would you be willing to start in a junior position?",
    StudySubject = "If \" yes\", what did you study?",
    CompletedStudies = "Did you complete your studies?",
    ReasonNotCompleting = "What prevented you from completing your studies?") %>% 
    mutate_all(funs(factor)) %>%
    mutate(LengthWillingStudy = factor(LengthWillingStudy, levels = OrderLengthStudy),
           AttendanceFrequency = factor(AttendanceFrequency, levels = OrderFreqAttendance),
           PreferredCost = factor(PreferredCost, levels = OrderPreferredCost),
           Skills = toupper(trimws(Skills))
    )
  
  if (input == "GLH"){
    data %<>% rename(
      id = "Response ID",
      WeeklyTimeCommitment = "How much time can you commit on a weekly basis to studying?",
      StudiedPreviously = "Have you studied before?",
      FocusGroup = "Would you be willing to attend a focus group to talk about your goals for further education and skills?") %>% 
      mutate(ReasonNotCompleting = fct_collapse(ReasonNotCompleting, 
                                                `Currently Studying` = studying_list_glh))
    
  } else if (input == "expo"){
    data %<>% rename(
      DailyTimeCommitment = "How much time can you commit on a DAILY basis to studying?",
      PassedMatric = "Did you pass Grade 12 (or an equivalent such as A-levels)?",
      StudiedPreviously = "Did you study after high school?",
      FocusGroup = "Would you be willing to attend a focus group to talk about your goals for further education and  skills?") %>% 
      mutate(ReasonNotCompleting = fct_collapse(ReasonNotCompleting, 
                                                `Currently Studying` = studying_list_expo,
                                                `No motivation` = motivation_expo))
    
    #make sure forms weren't submitted twice
    data <- data[!duplicated(data),]
    
    #anonymise
    drop <- c("Cellphone Number ( as per your Uber profile)",
              "Email address ( as per your Uber profile)")
    data <- data[ , !(names(data) %in% drop)]
    
    #add unique number
    data$id <- c(1:nrow(data))
    data$id <- as.factor(1000 + data$id) #make the id different from glh id
    
    
  } else {
    return("sample not recognised")
  }
  
  return(data)
  
}