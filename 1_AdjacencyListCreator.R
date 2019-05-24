##**************** EDIT BELOW ****************

# Set Working Directory (This is where the output file will be saved to)
setwd('/Users/alexander.liss/Egnyte/Private/alexander.liss/0_MarSci_Practice/1_CodingTutorials/aspiration_model')

# Choose the file to load
#*** IMPORTANT ***
# Make sure the column in your data containing the post message is called "Contents" and 
# Make sure the column in your data containing the Screen Name of the Author is called "Author"
# This is Case-Sensitive!
file2Load = "CombinedDataset.csv"


# How many UNIQUE times must a node be mentioned in order to enter the network?
minMentions = 5




##**************** DO NOT EDIT BELOW ****************
#__________________________________________________________________________

# Load the post file
mydata = read.csv(file2Load,
                  header = TRUE,
                  stringsAsFactors = FALSE,
                  fileEncoding="latin1", #on pc comment this part out
                  sep = ","
)

# Check if needed packages are installed
if (("tm" %in% rownames(installed.packages()))==FALSE){
  install.packages("tm")
}
# load "tm" package
library("tm")

#__________________________________________________________________________



## Step 1: Combine all tweets for each individual user (each user has 1 big tweet)

uniqueUsers = unique(mydata$Author)
#uniqueUsers = tolower(uniqueUsers)
numUsers = length(uniqueUsers)
# Mentions And Hashtags
MentionsAndHashtags = matrix("",nrow=numUsers,ncol=1) # Creates empty character array
row.names(MentionsAndHashtags) = uniqueUsers # changes the row names of MentionsAndHashtags to uniqueUsers list
# Only Mentions
OnlyMentions = matrix("",nrow=numUsers,ncol=1) # Creates empty character array
row.names(OnlyMentions) = uniqueUsers # changes the row names of MentionsAndHashtags to uniqueUsers list
# Only Hashtags
OnlyHashtags = matrix("",nrow=numUsers,ncol=1) # Creates empty character array
row.names(OnlyHashtags) = uniqueUsers # changes the row names of MentionsAndHashtags to uniqueUsers list
# Record number of tweets each user has
NumTweets = matrix(0,nrow=numUsers,ncol=1)

for (i in 1:numUsers) {
  # find indices of all tweets each user has
  userName = uniqueUsers[i] # Screen Name
  indexOfTweets = which(mydata$Author==userName) # each index for every "username" tweet
  NumTweets[i] = length(indexOfTweets)
  # Get each tweet's message
  userTweets = mydata$Contents[indexOfTweets] # list of each message "username" said
  # Compile each tweet into one big tweet
  oneBigTweet = paste(userTweets, sep="", collapse=" ") # collapse list of tweets into "one big tweet"
  # Extract @mentions from each tweet
  atMentions = regmatches(oneBigTweet, gregexpr('@\\w+', oneBigTweet))[[1]] # get column vector list of all @ mentions
  hashtags = regmatches(oneBigTweet, gregexpr('#\\w+', oneBigTweet))[[1]] # get column vector list of all hashtags
  # Paste all hashtags and mentions to single character array separated by " "
  atMentions = paste(atMentions, collapse = " ")
  hashtags = paste(hashtags, collapse = " ")
  
  # Store Targets
  MentionsAndHashtags[i] = paste(atMentions,hashtags,collapse=" ")
  OnlyMentions[i] = paste(atMentions,collapse=" ")
  OnlyHashtags[i] = paste(hashtags,collapse=" ")
    
}






#__________________________________________________________________________
# Convert each message into mentions and hashtags separated by " "


nRows = nrow(mydata)
Messages = matrix("",nrow=nRows,ncol=1)

for (i in 1:nRows) {
  
  currentMessage = tolower(mydata$Contents[i])
  currentHashtags = regmatches(currentMessage, gregexpr('#\\w+', currentMessage))[[1]] # get column vector list of all hashtags
  currentMentions = regmatches(currentMessage, gregexpr('@\\w+', currentMessage))[[1]] # get column vector list of all @ mentions
  currentMentionsAndHashtags = paste(currentHashtags,currentMentions,collapse=" ")
  currentMentionsAndHashtags = paste(" ",currentMentionsAndHashtags," ")
  
  Messages[i] = currentMentionsAndHashtags
  
}






#__________________________________________________________________________

## Step 2: Make VCorpus and Doc-Term Matrix of @Mentions & HashTags

docs = VCorpus(VectorSource(MentionsAndHashtags))
docsMentions = VCorpus(VectorSource(OnlyMentions))
docsHashtags = VCorpus(VectorSource(OnlyHashtags))

for (i in 1:numUsers) {
  
  docs[[i]]$meta$author = uniqueUsers[i]
  docsMentions[[i]]$meta$author = uniqueUsers[i]
  docsHashtags[[i]]$meta$author = uniqueUsers[i]
  
}


#Transform to lower case (need to wrap in content_transformer)
docs <- tm_map(docs,content_transformer(tolower))
docsMentions <- tm_map(docsMentions,content_transformer(tolower))
docsHashtags <- tm_map(docsHashtags,content_transformer(tolower))


#________Reduce the Document-Term Matrix______________
#******* Reduce the Document-Term Matrix to only frequently occuring items
# include only those @mentions that occur in 5 to 156 documents
minThreshold = minMentions
DTMreduced = DocumentTermMatrix(docs,control=list(bounds = list(global = c(minThreshold,1000000))))
DTMreducedMentions = DocumentTermMatrix(docsMentions,control=list(bounds = list(global = c(minThreshold,1000000))))
DTMreducedHashtags = DocumentTermMatrix(docsHashtags,control=list(bounds = list(global = c(minThreshold,1000000))))

#__________________________________________________________________________


## Create Adjacency List

# First turn Document-Term-Matrix into matrix
dtmrMatrix = as.matrix(DTMreduced)
dtmrMatrixMentions = as.matrix(DTMreducedMentions)
dtmrMatrixHashtags = as.matrix(DTMreducedHashtags)

numRows = nrow(dtmrMatrix)
numRowsMentions = nrow(dtmrMatrixMentions)
numRowsHashtags = nrow(dtmrMatrixHashtags)
numCols = ncol(dtmrMatrix)
numColsMentions = ncol(dtmrMatrixMentions)
numColsHashtags = ncol(dtmrMatrixHashtags)

maxRowsForList = numRows*numCols
maxRowsForListMentions = numRowsMentions*numColsMentions
maxRowsForListHashTags = numRowsHashtags*numColsHashtags

numColsForList = 2

AdjacencyList = matrix(nrow=maxRowsForList,ncol=3)
AdjacencyListMentions = matrix(nrow=maxRowsForListMentions,ncol=3)
AdjacencyListHashtags = matrix(nrow=maxRowsForListHashTags,ncol=3)

columnNames = colnames(dtmrMatrix)
columnNamesMentions = colnames(dtmrMatrixMentions)
columnNamesHashtags = colnames(dtmrMatrixHashtags)

listCount = 1
listCountMentions = 1
listCountHashtags = 1
for (i in 1:numRows) {
  
  rowName = uniqueUsers[i]
  rowNameMentions = uniqueUsers[i]
  rowNameHashtags = uniqueUsers[i]
  
  for (j in 1:numCols) {
    
    elementValue = dtmrMatrix[i,j]
    elementValueMentions = tryCatch(dtmrMatrixMentions[i,j], error = function(e) 0)
    elementValueHashtags = tryCatch(dtmrMatrixHashtags[i,j], error = function(e) 0)
    
    # Mentions & Hashtags
    if (elementValue>0){
      
      colName = columnNames[j]
      
      AdjacencyList[listCount,1] = rowName
      AdjacencyList[listCount,2] = colName
      AdjacencyList[listCount,3] = elementValue#/NumTweets[i] # Term Frequency Weight
      
      listCount = listCount + 1
    }
    
    # Mentions Only
    if (elementValueMentions>0){
      
      colName = columnNamesMentions[j]
      
      AdjacencyListMentions[listCountMentions,1] = rowNameMentions
      AdjacencyListMentions[listCountMentions,2] = colName
      AdjacencyListMentions[listCountMentions,3] = elementValueMentions#/NumTweets[i] # Term Frequency Weight
      
      listCountMentions = listCountMentions + 1
    }
    
    # Hashtags Only
    if (elementValueHashtags>0){
      
      colName = columnNamesHashtags[j]
      
      AdjacencyListHashtags[listCountHashtags,1] = rowNameHashtags
      AdjacencyListHashtags[listCountHashtags,2] = colName
      AdjacencyListHashtags[listCountHashtags,3] = elementValueHashtags#/NumTweets[i] # Term Frequency Weight
      listCountHashtags = listCountHashtags + 1
    }
    
  }
  
}

# Remove incomplete cases
AdjacencyList = AdjacencyList[complete.cases(AdjacencyList),]
AdjacencyListMentions = AdjacencyListMentions[complete.cases(AdjacencyListMentions),]
AdjacencyListHashtags = AdjacencyListHashtags[complete.cases(AdjacencyListHashtags),]

# Change Column Names in Adjacency List 
colnames(AdjacencyList) <- c("Source","Target","Weight");
colnames(AdjacencyListMentions) <- c("Source","Target","Weight");
colnames(AdjacencyListHashtags) <- c("Source","Target","Weight");

# Make Edge List (Adjacency List)
EdgeList = AdjacencyList

# Make Node List (Contains information about nodes)
uniqueSources = unique(EdgeList[,1])
numSources = length(uniqueSources)

uniqueTargets = unique(EdgeList[,2])
numTargets = length(uniqueTargets)

Id = c(uniqueSources,uniqueTargets)
Label = c(uniqueSources,uniqueTargets)

NodeSizeSources = matrix(0,nrow=numSources,ncol=1)
NodeSizeTargets = matrix(0,nrow=numTargets,ncol=1)

# for (i in 1:numTargets) {
#   
#   target = uniqueTargets[i]
#   searchStr = paste(" ",target," ")
#   MessagesWithTarget = grepl(searchStr, Messages,ignore.case = TRUE) # logical
#   NodeSizeTargets[i] = mean(na.omit(mydata$InteractionRate[MessagesWithTarget]))
#   
#   if (is.nan(NodeSizeTargets[i])==TRUE){
#     NodeSizeTargets[i] = 0
#   }
#   
# }
# 
# NodeSize = c(NodeSizeSources,NodeSizeTargets)
# 
# NodesList = data.frame(Id,Label,NodeSize)


write.csv(AdjacencyList,"AdjListMentionsAndHashtags5min.csv",row.names=FALSE)


# How Many Authors Didn't Make It Into The Reduced Document-Term Matrix?
NumLeftOutPeople = sum(rowSums(dtmrMatrix)==0)
NumLeftOutPeopleMentions = sum(rowSums(dtmrMatrixMentions)==0)
NumLeftOutPeopleHashtags = sum(rowSums(dtmrMatrixHashtags)==0)

