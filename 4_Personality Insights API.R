
# 1. initialize & setup ------------------------------------------------


setwd('/Users/alexander.liss/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/aspiration_model')

library(WatsonR)
library(curl)
library(devtools)
library(RCurl)
library(tidyjson)
library(dplyr)
library(readr)
library(jsonlite)

## setting the RCurloptions

# PROBLEM > If you get this > Error in function (type, msg, asError = TRUE)  :  SSL certificate problem: self signed certificate in certificate chain 
# SOLUTION then you need to do this > YOU MUST KEEP THIS IN FOR below to work To begin: this line sets CERT Global to make a CA Cert go away - http://stackoverflow.com/questions/15347233/ssl-certificate-failed-for-twitter-in-r
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"),httpauth=AUTH_BASIC)) # NOTE - the "httpauth=AUTH_BASIC" piece gets rid of the "Error: UNAUTHORIZED" message 

## enter API keys to use Personality Insights API- credentials
## the URL to find this for your account is here (at least for Personality Insights API)
## https://console.bluemix.net/services/personality-insights
pi_url="https://gateway.watsonplatform.net/personality-insights/api/v3/profile?version=2017-10-13&consumption_preferences=true&csv_headers=true"
username = "109b8520-444a-40f5-8abd-b8bc0e850204" # yours goes here from service credentials
password = "ptjvmrw6SR3e" # yours goes here from service credentials
username_password = paste(username,":",password,sep="")


# 2. Background functions ----------------------------------------------------

## port this into an Rproject once you ge the hang of it

###### FUNCTION - ANalyze text with Personality Insights service
watson.personality_insights.analyze <- function(TEXT)
{
  return(POST(url=pi_url,
              authenticate(username,password),
              add_headers("Content-Type"="text/plain","charset"="utf-8" ),
              add_headers("Accept: text/csv"),
              body = TEXT
  ))
}


# 3. Calling the Personality Insights API ----------------------------------------------------

json_data <- fromJSON("tweets_for_each_class.json")
data_list <- list()
length(data_list) <- nrow(json_data)
personality_big <- data.frame(matrix(0, ncol = 0, nrow = 0))
needs_big <- data.frame(matrix(0, ncol = 0, nrow = 0))
values_big <- data.frame(matrix(0, ncol = 0, nrow = 0))
consumption_preferences_big <- data.frame(matrix(0, ncol = 0, nrow = 0))


for (i in 1:nrow(json_data)){
  data_list[i] <- json_data[i,2]
  i <- i + 1
}

for (i in 1:length(data_list)){
  
    TEXT_TO_ANALYZE=as.character(data_list[i])
      
    response <- watson.personality_insights.analyze(TEXT_TO_ANALYZE)  # send text to function that queries the Watson API
    
    response # this should be a status 200 if it went well - if not, check your authentication  or look at content(response,"text")
    PI_analysis <- content(response) # pull the analysis trait and % (X52)
    #PI_analysis <- tidyResponse(PI_analysis) # hacky way to tidy up in table format - there are certainly better ways than this :) 
    summary(PI_analysis)
    
    personality <- PI_analysis$personality
    needs <- PI_analysis$needs
    values <- PI_analysis$values
    consumption_preferences <- PI_analysis$consumption_preferences
    
    
    # 4. Creating a datatable for OCEAN - Big 5 -------------------------------
    
    personality <- toJSON(personality)
    
    personality <- as.tbl_json(as.character(personality)) %>%            # Parse the JSON and setup a 'tbl_json' object
      gather_array %>%  # Gather (stack) the array by index
      spread_values(    # Spread (widen) values to widen the data.frame
        Big_5_Trait = jstring("name"),  # Extract the "name" object as a character column  
        Big_5_Percentile = jstring("percentile")) %>%     # Extract the "age" object as a numeric column  
      enter_object("children") %>%             # Look at the "children"
      gather_array %>% 
      spread_values(    # Spread (widen) values to widen the data.frame
        facet = jstring("name"),  # Extract the "name" object as a character column  
        facet_score = jstring("percentile"))     # Extract the "percentile" object as a numeric column 
    
    #cleaning up and reformatting columns
    personality <- as.data.frame(personality)
    personality[] <- lapply(personality, gsub, pattern="list\\(", replacement='')
    personality[] <- lapply(personality, gsub, pattern=")", replacement='')
    personality[, c(4,6)] <- sapply(personality[, c(4,6)], as.numeric)
    personality <- personality[-c(1,2)]
    personality$group <- json_data[i,1]
    personality <- personality[c(5,1:4)]
    #summary(personality)
    personality_big <- rbind(personality_big, personality)
    #write_csv(personality, "personality.csv")
   
    
    # 5. Creating a datatable for Needs ---------------------------------------
    needs <- toJSON(needs)
    
    needs <- as.tbl_json(as.character(needs)) %>%            # Parse the JSON and setup a 'tbl_json' object
      gather_array %>%  # Gather (stack) the array by index
      spread_values(    # Spread (widen) values to widen the data.frame
        need_type = jstring("name"),  # Extract the "name" object as a character column  
        need_percentile = jstring("percentile"))     # %>% Extract the "age" object as a numeric column  
    # enter_object("children") %>%             # Look at the "children"
    # gather_array %>% 
    # spread_values(    # Spread (widen) values to widen the data.frame
    #   facet = jstring("name"),  # Extract the "name" object as a character column  
    #   facet_score = jstring("percentile"))     # Extract the "percentile" object as a numeric column 
    
    #cleaning up and reformatting columns
    needs <- as.data.frame(needs)
    needs[] <- lapply(needs, gsub, pattern="list\\(", replacement='')
    needs[] <- lapply(needs, gsub, pattern=")", replacement='')
    needs[, 4] <- sapply(needs[, 4], as.numeric)
    needs <- needs[-c(1,2)]
    needs$group <- json_data[i,1]
    needs <- needs[c(3,1,2)]
    needs_big <- rbind(needs_big, needs)
    #summary(needs)
    #write_csv(needs, "needs.csv")
    
    
    
    # 6. Creating a datatable for Values --------------------------------------
    values <- toJSON(values)
    
    values <- as.tbl_json(as.character(values)) %>%            # Parse the JSON and setup a 'tbl_json' object
      gather_array %>%  # Gather (stack) the array by index
      spread_values(    # Spread (widen) values to widen the data.frame
        value_type = jstring("name"),  # Extract the "name" object as a character column  
        value_percentile = jstring("percentile"))     # %>% Extract the "age" object as a numeric column  
    # enter_object("children") %>%             # Look at the "children"
    # gather_array %>% 
    # spread_values(    # Spread (widen) values to widen the data.frame
    #   facet = jstring("name"),  # Extract the "name" object as a character column  
    #   facet_score = jstring("percentile"))     # Extract the "percentile" object as a numeric column 
    
    #cleaning up and reformatting columns
    values <- as.data.frame(values)
    values[] <- lapply(values, gsub, pattern="list\\(", replacement='')
    values[] <- lapply(values, gsub, pattern=")", replacement='')
    values[, 4] <- sapply(values[, 4], as.numeric)
    values <- values[-c(1,2)]
    values$group <- json_data[i,1]
    values <- values[c(3,1,2)]
    #summary(values)
    values_big <- rbind(values_big, values) 
    #write_csv(values, "values.csv")
    
    
    # 7. Creating a datatable for Consumption Preferences --------------------------
    consumption_preferences <- toJSON(consumption_preferences)
    
    consumption_preferences <- as.tbl_json(as.character(consumption_preferences)) %>%            # Parse the JSON and setup a 'tbl_json' object
      gather_array %>%  # Gather (stack) the array by index
      spread_values(    # Spread (widen) values to widen the data.frame
        preference = jstring("name")) %>% # Extract the "name" object as a character column  
      #value_percentile = jstring("percentile"))     # %>% Extract the "age" object as a numeric column  
      enter_object("consumption_preferences") %>%             # Look at the "children"
      gather_array %>% 
      spread_values(    # Spread (widen) values to widen the data.frame
        propensity = jstring("name"),  # Extract the "name" object as a character column  
        propensity_score = jstring("score"))     # Extract the "percentile" object as a numeric column 
    
    #cleaning up and reformatting columns
    consumption_preferences <- as.data.frame(consumption_preferences)
    consumption_preferences[] <- lapply(consumption_preferences, gsub, pattern="list\\(", replacement='')
    consumption_preferences[] <- lapply(consumption_preferences, gsub, pattern=")", replacement='')
    consumption_preferences[, 5] <- sapply(consumption_preferences[, 5], as.numeric)
    consumption_preferences <- consumption_preferences[-c(1,2)]
    consumption_preferences$group <- json_data[i,1]
    consumption_preferences <- consumption_preferences[c(4,1:3)]
    consumption_preferences_big <- rbind(consumption_preferences_big, consumption_preferences)
    #summary(consumption_preferences)
    #write_csv(consumption_preferences, "consumption_preferences.csv")
    
}

# 8.do some data processing and  export the UBER file!


#adding a split variable to each column 
write_csv(personality_big, "personality_ratings_total.csv")
write_csv(needs_big, "needs_total.csv")
write_csv(values_big, "values_total.csv")
write_csv(consumption_preferences_big, "consumption_preferences_total.csv")



########   neeeds
needs_updated <- read.csv("needs_total.csv")
test <- needs_updated %>%
  group_by(need_type) %>%
  summarise(average = mean(need_percentile))

needs_updated <- merge(needs_updated, test, by = "need_type")

needs_updated <- needs_updated %>%
  mutate(split = need_percentile - average, split_percentile = need_percentile / average) %>%
  select(2, 1, 3:6) %>%
  arrange(group)

write.csv(needs_updated, "needs_total_update.csv", row.names = FALSE)

########   consumption prefs
prefs_updated <- read.csv("consumption_preferences_total.csv")
test <- prefs_updated %>%
  group_by(propensity) %>%
  summarise(average = mean(propensity_score))

prefs_updated <- merge(prefs_updated, test, by = "propensity")

prefs_updated <- prefs_updated %>%
  mutate(split = propensity_score - average, split_percentile = propensity_score / average) %>%
  arrange(group, preference) %>%
  select(3, 1, 2, 4:7) 

write.csv(prefs_updated, "consumption_preferences_total_update.csv", row.names = FALSE)

########    values - resume here
values_updated <- read.csv("values_total.csv")
test <- values_updated %>%
  group_by(value_type) %>%
  summarise(average = mean(value_percentile))

values_updated <- merge(values_updated, test, by = "value_type")

values_updated <- values_updated %>%
  mutate(split = value_percentile - average, split_percentile = value_percentile / average) %>%
  arrange(group, value_type) %>%
  select(2, 1, 3:6) 

write.csv(values_updated, "values_total_update.csv", row.names = FALSE)

##### personality ratings: resume here
personality_updated <- read.csv("personality_ratings_total.csv")
test <- personality_updated %>%
  group_by(Big_5_Trait, facet) %>%
  summarise(Big_5_average = mean(Big_5_Percentile), facet_average = mean(facet_score))

personality_updated <- left_join(personality_updated, test, by = c("Big_5_Trait","facet"))

personality_updated <- personality_updated %>%
  mutate(Big_5_split = Big_5_Percentile - Big_5_average,
         facet_split = facet_score - facet_average,
         Big_5_split_percentile = Big_5_Percentile / Big_5_average,
         facet_split_percentile = facet_score / facet_average) %>%
  arrange(group, Big_5_Trait, facet) %>%
  select(1:3,8,4:5,6,9:10,7,11) 

write.csv(personality_updated, "personality_ratings_total_update.csv", row.names = FALSE)

