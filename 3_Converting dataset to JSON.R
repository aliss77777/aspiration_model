
# 1. Set Up & Load Files ---------------------------------------------------------------
setwd('/Users/alexander.liss/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/aspiration_model')

part1 <- read.csv('CombinedDataset.csv') #the big file of social posts stiched together
part2 <- read.csv('adjacency_list_with_modularity_class.csv') #the Gephi export, an adjacency list with modularity class appended



# 2. Append Modularity class to data set ----------------------------------

library(tidyverse)

data_file_with_modularity_class <- part1

#reducing part2 to a lookup table-type structure, and removing duplicates, to append modularity class 
part2 <-  part2[-c (2,3)]
part2 <- unique(part2[1:2])
names(part2)[1] <- 'Author'
data_file_with_modularity_class  <- left_join(data_file_with_modularity_class, part2, by="Author")

#renaming NA so it won't be dropped from the analysis
data_file_with_modularity_class$modularity_class[is.na(data_file_with_modularity_class$modularity_class)] <- 999 

#check # of rows with Modularity class assigned successfully
nrow(subset(data_file_with_modularity_class, data_file_with_modularity_class$modularity_class!=999))

# exporting the data file for use in outside application
write.csv(data_file_with_modularity_class, "data_file_with_modularity_class.csv", row.names = FALSE)

#get authors with no modularity class
authors_with_no_modularity_class <- subset(data_file_with_modularity_class, data_file_with_modularity_class$modularity_class!=999) 
authors_with_no_modularity_class <- unique(authors_with_no_modularity_class$Author)
length(authors_with_no_modularity_class)

#lookup table of authors' modularity class
lookup_table <- subset(data_file_with_modularity_class, data_file_with_modularity_class$modularity_class!="999")
lookup_table <- lookup_table[-c (1:4,6:20)] #note this just indexes columns, not rows



# 3.  Collapse tweets in each modularity class ----------------------------

library(tm)
require(tidytext)

#need to create an index of all the tweets by each modularity class
uniqueClasses = unique(data_file_with_modularity_class$modularity_class)
#uniqueUsers = tolower(uniqueUsers)
numClasses = length(uniqueClasses)
tweets_by_class <- list()

for (i in 1:numClasses) {
  modularity_class = uniqueClasses[i]
  indexOfTweets = which(data_file_with_modularity_class$modularity_class==modularity_class)
  classTweets = data_file_with_modularity_class$Contents[indexOfTweets]
  oneBigTweet = paste(classTweets, sep="", collapse=" ") # collapse list of tweets into "one big tweet"
  tweets_by_class <- c(tweets_by_class, oneBigTweet)
  # try making a list with oneBigTweet[i] where i is each class
}

#combine full text of each modularity class in a data frame
full_text_of_tweets_of_each_modularity_class <- do.call(rbind, Map(data.frame, modularity_class=uniqueClasses, text=tweets_by_class, row.names = FALSE))

#write.csv(full_text_of_tweets_of_each_modularity_class, "test.csv")


# 4. Export to a JSON for IBM Watson --------------------------------------

# summarizing the authors
summary_table_of_authors <- data_file_with_modularity_class %>%
  select(Post.Title, Author, modularity_class) %>%
  group_by(modularity_class) %>%
  mutate(Count_of_Authors = length(unique(Author)), Count_of_Posts = length(Post.Title)) %>%
  select(modularity_class, Count_of_Authors, Count_of_Posts) %>%
  distinct(modularity_class, Count_of_Authors, Count_of_Posts) %>%
  arrange(-Count_of_Authors)

#sorting for top 20 clusters by number of authors, and removing Modularity class 999 (NA)

tweets_by_class_for_export <- full_text_of_tweets_of_each_modularity_class %>%
  filter(modularity_class != 999)

tweets_by_class_for_export <- left_join(tweets_by_class_for_export, summary_table_of_authors, by="modularity_class")
tweets_by_class_for_export <- tweets_by_class_for_export[order(tweets_by_class_for_export[3], decreasing = TRUE),]
tweets_by_class_for_export <- tweets_by_class_for_export[1:20,]  

library(readr)
library(jsonlite)

tweets_by_class_for_export %>% 
  toJSON() %>%
  write_lines("tweets_for_each_class.json")