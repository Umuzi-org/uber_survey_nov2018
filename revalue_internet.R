library(plyr)

revalue_internet <- function(data, input){
 if (input == "GLH"){
  data[['InternetAccess']] <- plyr::revalue(data[['InternetAccess']], 
                                          c("I don't have any internet access when not operating on Uber"="None",
                                            "Internet at home"="WiFi at home",
                                            "Internet Cafe" = "Internet Cafe",
                                            "With your cellphone data" = "Mobile data"),
                                          warn_missing = FALSE)
  return(data)
  
 } else if (input == "expo" | input == "online") {
  data[['InternetAccess']] <- plyr::revalue( data[['InternetAccess']], 
                                           c("I don't have any internet access when not operating on Uber"="None",
                                             "Internet at home (WiFi or ADSL)"="WiFi at home",
                                             "Internet Cafe" = "Internet Cafe",
                                             "With your cellphone data" = "Mobile data",
                                             "Free internet from a community centre" = "Community Centre"),
                                           warn_missing = FALSE)
  return(data)
 } else { 
  print("dataset not recognised")}
}
