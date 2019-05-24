#-------
# Introduction: this data processing script will:
# Part 0 - Append Modularity Class (from Gephi) to Adjacency List 
# Part 1 - Summarize the top segments and export a table with that info
# Part 2 - Create 3 export tables with the top authors, top @ mentions, and top #hashtag mentions for each segment

#____
#Before you dive in, there a few parameters to set up 

# Set working directly where you will save the files, and load necessary package
setwd('/Users/alexander.liss/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/aspiration_model')

#install.packages('plyr')
library(plyr)

# Set the file names you will need 
part1 <- read.csv('AdjListMentionsAndHashtags5min.csv') #the adjacency list
part2 <- read.csv('gephi_export_with_modularity_class.csv') #the Gephi export

# What % of the total conversation do you want to capture in your top segments export?
#         -  Set the number below to the % of total conversation you want to capture. 
#         -  This is used in Part 2.0 to determine how many segments you will get in your summary export
sum_of_weights = 0.91  

# How many of the top authors / mentions do you want for each segment?
# Set this variable to a number, like 25, for how many you want to export for each segment 
toprows = 25

#--------------------
#Do not Edit the script below unless you know what you're doing :)
#--------------------


# PART O: Merge Adjacency List with Gephi Export

# recreate the 'VLOOKUP' procedure in Excel, shortening the data files to make lookup tables, then matching by row and down, then appending
names(part2)[1] <- "Source"
part1_lookup <- part1[1]
part2_lookup <- part2[c(1,4)]
temp_table <- join(part1_lookup, part2_lookup, by = "Source")
mydata <- part1
mydata[4] <- temp_table[2]

# clean-up unusued files and export output
rm(part1, part2, part1_lookup, part2_lookup, temp_table) 
write.csv(mydata, file = "adjacency_list_with_modularity_class.csv", row.names = FALSE)
# ------------------------------

# PART 1: create a table with summary statistics for the top segments

# creates a table with the count of posts for each modularity class (segment), and % of posts by class
segment_table <- count(mydata[4])
segment_table <- subset(segment_table, modularity_class!="NA", select = modularity_class:freq) #removing any lines where modularity class is NA
names(segment_table)[2] <- "Number of Posts by Class" #rename the column
segment_table$'Percent of Posts by Class' <- with(mydata, prop.table(table(modularity_class))) 

#this appends the sum of the 'weight' column for each class, and % of weight by class
temp_table <- aggregate(Weight ~ modularity_class, data=mydata, sum)
segment_table$'Sum of Weight by Class' <- temp_table$Weight 
segment_table$'Percent of Weight by Class' <- segment_table[,4]/sum(segment_table[,4])

#append # of authors for each segment as a column
temp_table <- ddply(mydata,~modularity_class,summarise,'Number of Authors'=length(unique(Source)))
segment_table$'Number of Authors' <- temp_table$'Number of Authors'
segment_table$'Percent of Authors by Class' <- segment_table[,6]/sum(segment_table[,6])

#sort by Number of Authors to be consistent with Gephi output
segment_table <- segment_table[order(segment_table$`Number of Authors`, decreasing = TRUE),]

# clean up variable and export to excel
rm(temp_table)
write.csv(segment_table, file = "segment_table.csv", row.names = FALSE)
# ------------------------------

# PART 2: export spreadsheets for analysis with top authors / posts / mentions by segment

# Part 2.0: identify the top segments to start exporting authors & posts from

# initialize these variables to create the loop
temp_variance_counter = 0
i = 1
list_of_segments <- c()

while (temp_variance_counter < sum_of_weights) {
  print(c(("we are in row number"),i))
  temp_variance_counter = temp_variance_counter + segment_table[i,5]
  list_of_segments <- c(list_of_segments, segment_table[i,1])
  i = i+1
}
# ------------------------------

# Part 2.1: export the top AUTHORS for the top segments. (1 of 3)
max_number_of_authors = toprows #set this to be the number of authors you want to export for each segment

#initialize the counting variable for the loop
i = 1 
temp_table_2 <- data.frame(matrix(0, ncol = 0, nrow = as.numeric(max_number_of_authors))) #set to the same # of rows as the # of authors

 
while (i <= length(list_of_segments)){
  print(c("starting with segment #",i))
  temp_table_1 <- subset(mydata, modularity_class==list_of_segments[i], select=Source:Weight) #create a subset for a specific segment
  local_derp <- count(temp_table_1[1]) #choosing the first column, authors
  names(local_derp)[2] <- "Number of Posts by Author"  
  local_derp <- local_derp[order(local_derp$`Number of Posts by Author`, decreasing = TRUE),]
  names(local_derp)[1] <- paste("Segment",list_of_segments[i],sep=" ") #use PASTE to concatenate a vector! cool :)
  local_derp$'Percent of Posts by Author within Segment' <- local_derp[,2]/sum(local_derp[,2])
  #adding a process to add blank rows if the number of rows in 'local derp' is less than 25
  checkrows <- max_number_of_authors - nrow(local_derp)
  while (checkrows > 0){
    temp_row <-data.frame("NA", "NA", "NA")
    names(temp_row) <- colnames(local_derp)
    local_derp <- rbind(local_derp, temp_row)
    checkrows = checkrows - 1
  }
  local_derp <- head(local_derp, as.numeric(max_number_of_authors))  
  temp_table_2 <- cbind(temp_table_2,local_derp) #use cbind
  i = i + 1
}

write.csv(temp_table_2, file = "top_authors_table.csv", row.names = FALSE)
# ------------------------------

# Part 2.2: export the top @Mentions for the top segments. (2 of 3)
max_number_of_at_mentions = toprows #set this to be the number of mentions you want to export for each segment
i = 1 
temp_table_2 <- data.frame(matrix(0, ncol = 0, nrow = as.numeric(max_number_of_at_mentions))) #set to the same # of rows as the # of mentions

while (i <= length(list_of_segments)){
  print(c("starting with segment #",i))
  temp_table_1 <- subset(mydata, modularity_class==list_of_segments[i], select=Source:Weight) #create a subset for a specific segment
  local_derp <- count(temp_table_1[2]) #choosing the second column
  search_character <- '@'
  local_derp <- subset(local_derp, grepl(search_character, Target) ) #subsetting to capture just @ mentions
  names(local_derp)[2] <- "Number of @Mentions"  
  local_derp <- local_derp[order(local_derp$`Number of @Mentions`, decreasing = TRUE),]
  names(local_derp)[1] <- paste("Segment",list_of_segments[i],sep=" ") #use PASTE to concatenate a vector! cool :)
  local_derp$'Percent of @Mentions within Segment' <- local_derp[,2]/sum(local_derp[,2])
  #adding a process to add blank rows if the number of rows in 'local derp' is less than 25. this code is kludgy and i will streamline over time
  checkrows <- max_number_of_at_mentions - nrow(local_derp)
  while (checkrows > 0){
    temp_row <-data.frame("NA", "NA", "NA")
    names(temp_row) <- colnames(local_derp)
    local_derp <- rbind(local_derp, temp_row)
    checkrows = checkrows - 1
  }
  local_derp <- head(local_derp, as.numeric(max_number_of_at_mentions))  
  temp_table_2 <- cbind(temp_table_2,local_derp) #use cbind
  i = i + 1
}

write.csv(temp_table_2, file = "top_@mentions_table.csv", row.names = FALSE)
# ------------------------------

# Part 2.3: export the top #Hashtags for the top segments. (3 of 3)
max_number_of_hashtags = toprows #set this to be the number of authors you want to export for each segment
i = 1 
temp_table_2 <- data.frame(matrix(0, ncol = 0, nrow = as.numeric(max_number_of_hashtags))) #set to the same # of rows as the # of mentions

while (i <= length(list_of_segments)){
  print(c("starting with segment #",i))
  temp_table_1 <- subset(mydata, modularity_class==list_of_segments[i], select=Source:Weight) #create a subset for a specific segment
  local_derp <- count(temp_table_1[2]) #choosing the second column
  search_character <- '#'
  local_derp <- subset(local_derp, grepl(search_character, Target) ) #subsetting to capture just # hashtags
  names(local_derp)[2] <- "Number of Hashtag Mentions"  
  local_derp <- local_derp[order(local_derp$`Number of Hashtag Mentions`, decreasing = TRUE),]
  names(local_derp)[1] <- paste("Segment",list_of_segments[i],sep=" ") #use PASTE to concatenate a vector! cool :)
  local_derp$'Percent of Hashtag Mentions within Segment' <- local_derp[,2]/sum(local_derp[,2])
  #adding a process to add blank rows if the number of rows in 'local derp' is less than 25
  checkrows <- max_number_of_hashtags - nrow(local_derp)
  while (checkrows > 0){
    temp_row <-data.frame("NA", "NA", "NA")
    names(temp_row) <- colnames(local_derp)
    local_derp <- rbind(local_derp, temp_row)
    checkrows = checkrows - 1
  }
  local_derp <- head(local_derp, as.numeric(max_number_of_hashtags))  
  temp_table_2 <- cbind(temp_table_2,local_derp) #use cbind 
  i = i + 1
}

write.csv(temp_table_2, file = "top_hashtag_mentions_table.csv", row.names = FALSE)
# ------------------------------
