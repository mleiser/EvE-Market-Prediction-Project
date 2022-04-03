library(plyr)
library(dplyr)
library(rlang)
library(jsonlite)
library(data.table)
library(datasets)
library(e1071)
library(forecast)
library(formattable)
library(ggplot2)
library(ggrepel)
library(progress)
library(progressr)
library(readxl)
library(reshape2)
library(tibble)
library(tidyquant)
library(tidyr)
library(tidyselect)
library(tseries)

#Test data frame
EveMarketExt <- paste0("https://market.fuzzwork.co.uk/aggregates/?region=10000002&types=")

#jita <- 10000002
#amarr <- 30002187
Market.json <- fromJSON(readLines(EveMarketExt), simplifyDataFrame = TRUE, flatten = TRUE)
dfs <- lapply(Market.json, data.frame, stringsAsFactors = FALSE)
Market.json <- bind_rows(dfs, .id = 'ID')

#Initilize the Market ID data frame
colnames(typeids) <- c('Type ID', 'Item Type', 'Market ID')
MarketIDList <- unique(typeids$`Market ID`)

#Adds the parent group IDs to the typeids data frame
ParentGroups <- c()
for (i in 1:length(typeids$`Market ID`)) {
  for(j in 1:length(invMarketGroups$parentGroupID)) {
    if (typeids$`Market ID`[i] == invMarketGroups$marketGroupID[j]) {
      ParentGroups <- append(ParentGroups, invMarketGroups$parentGroupID[j])
    }
  }
}

#Adds the second parent group IDs to the data frame
ParentGroups2 <- c()
for (i in 1:nrow(typeids)) {
  for(j in 1:nrow(invMarketGroups)) {
    if (typeids$`Parent ID`[i] == invMarketGroups$marketGroupID[j]) {
      ParentGroups2 <- append(ParentGroups2, invMarketGroups$parentGroupID[j])
    }
  }
}
typeids$pID2 <- ParentGroups2

#Adds the third parent group IDs
ParentGroups3 <- c()
for (i in 1:nrow(typeids)) {
  if (typeids$pID2[i] == 'None') {
    ParentGroups3 <- append(ParentGroups3, 'None')
    next
  }
  for(j in 1:nrow(invMarketGroups)) {
    if (typeids$pID2[i] == invMarketGroups$marketGroupID[j]) {
      ParentGroups3 <- append(ParentGroups3, invMarketGroups$parentGroupID[j])
    } 
  }
}
typeids$pID3 <- ParentGroups3

#Adds the fourth and final parent group IDs
ParentGroups4 <- c()
for (i in 1:nrow(typeids)) {
  if (typeids$pID3[i] == 'None') {
    ParentGroups4 <- append(ParentGroups4, 'None')
    next
  }
  for(j in 1:nrow(invMarketGroups)) {
    if (typeids$pID3[i] == invMarketGroups$marketGroupID[j]) {
      ParentGroups4 <- append(ParentGroups4, invMarketGroups$parentGroupID[j])
    } 
  }
}
typeids$pID4 <- ParentGroups4
x = 0

#data.frame pre processing function
preprocessMarketDataFrame <- function(dataFrame, parID) {
  library(formattable)
  shape <- dim(dataFrame)
  
  # Convert column data types
  dataFrame$`ID` <- as.factor(dataFrame$`ID`)
  dataFrame$`buy.weightedAverage` <- as.numeric(dataFrame$`buy.weightedAverage`)
  dataFrame$`sell.weightedAverage` <- as.numeric(dataFrame$`sell.weightedAverage`)
  dataFrame$`buy.max` <- as.numeric(dataFrame$`buy.max`)
  dataFrame$`sell.max` <- as.numeric(dataFrame$`sell.max`)
  dataFrame$`buy.min` <- as.numeric(dataFrame$`buy.min`)
  dataFrame$`sell.min` <- as.numeric(dataFrame$`sell.min`)
  dataFrame$`buy.stddev` <- as.numeric(dataFrame$`buy.stddev`)
  dataFrame$`sell.stddev` <- as.numeric(dataFrame$`sell.stddev`)
  dataFrame$`buy.median` <- as.numeric(dataFrame$`buy.median`)
  dataFrame$`sell.median` <- as.numeric(dataFrame$`sell.median`)
  dataFrame$`buy.volume` <- as.numeric(dataFrame$`buy.volume`)
  dataFrame$`sell.volume` <- as.numeric(dataFrame$`sell.volume`)
  dataFrame$`buy.orderCount` <- as.numeric(dataFrame$`buy.orderCount`)
  dataFrame$`sell.orderCount` <- as.numeric(dataFrame$`sell.orderCount`)
  dataFrame$`buy.percentile` <- as.numeric(dataFrame$`buy.percentile`)
  dataFrame$`sell.percentile` <- as.numeric(dataFrame$`sell.percentile`)
  
  
  #typeidsub <- subset(typeids, typeids$`Top Market ID` == parID)
  typeidsub <- subset(typeids, typeids$`pID2` == parID)
  
  #Add the item type column
  itemType <- c()
  for (i in 1:nrow(typeidsub)) {
    for (j in 1:nrow(dataFrame)){
      if (typeidsub$`Type ID`[i] == dataFrame$`ID`[j]) {
        itemType <- append(itemType, typeidsub$`Item Type`[i])
        break
      }
    }
    progress(i, max.value = nrow(typeidsub), progress.bar = TRUE)
  }
  
  #Adds the order.total and ROI column to the complete data frame
  dataFrame <- cbind(ID = dataFrame$`ID`, itemType, dataFrame[, 2:17])
  dataFrame$order.total <- dataFrame$buy.orderCount + dataFrame$sell.orderCount
  dataFrame$ROI <- (dataFrame$sell.min - dataFrame$buy.max) / dataFrame$buy.max
  
  #Removes some outlier data points
  dataFrame <- subset(dataFrame, ROI < 100000 & ROI > 0)
  
  #Displays ROI as a percent
  dataFrame$ROI <- percent(dataFrame$ROI, 2)
  return(dataFrame)
}

#Creates a vector of all parent group 4 IDs
p4All <- c()
for (i in 1:nrow(typeids)) {
  if (typeids$pID2[i] == 'None') {
     p4All <- append(p4All, typeids$`Parent ID`[i]) 
  } else if (typeids$pID3[i] == 'None') {
    p4All <- append(p4All, typeids$pID2[i])  
  } else if (typeids$pID4[i] == 'None') {
    p4All <- append(p4All, typeids$pID3[i])  
  } else {
    p4All <- append(p4All, typeids$pID4[i])
  }
}

#Adds the Ids to the type ID data frame
typeids$`Parent ID` <- ParentGroups 
typeids$`Top Market ID` <- as.numeric(p4All)

#Pulls unique values for Parent groups
ParentGroupsU <- unique(ParentGroups)
p4U <- unique(ParentGroups4)

marketList <- list('Blueprints & Reactions' = 2, 'Ships' = 4, 'Ship Equipment' = 9, 'Ammunition & Charges' = 11, 
                   'Trade Goods' = 19, 'Implants & Boosters' = 24, 'Skills' = 150, 'Drones' = 157, 
                   'Manufacturing & Research' = 475, 'Structures' = 477, 'Ship and Module Modifications' = 955, 
                   'Planetary Infrastructure' = 1320, 'Apparel' = 1396, 'Special Edition Assets' = 1659, 
                   'Pilot\'s Services' = 1922, 'Ship SKINs' = 1954, 'Structure Equipment' = 2202, 
                   'Structure Modifications' = 2203)

EveMarketExtGet <- paste(p4U, collapse = ',')
EveMark <- paste(EveMarketExt, EveMarketExtGet, sep = '')


#Creates the market data table based on specified parent ID then gets all items on the market at and under that ID
Market.Pull <- function(ParentID) {
  library(jsonlite)
  library(progress)
  
  n <- 200
  itemList <- c()
  
  itemList1 <- ''
  
  #Creates the item list to be run thourgh
  for (i in 1:nrow(typeids)) {
    if (typeids$`Top Market ID`[i] == ParentID) {
      itemList <- append(itemList, typeids$`Type ID`[i])
    } else if (typeids$pID4[i] == ParentID) {
      itemList <- append(itemList, typeids$`Type ID`[i])
    } else if (typeids$pID3[i] == ParentID) {
      itemList <- append(itemList, typeids$`Type ID`[i])
    } else if (typeids$pID2[i] == ParentID) {
      itemList <- append(itemList, typeids$`Type ID`[i])
    }
  }
  print("Task 1 Done!")
  
  #Splits the item list into groups of 'n' items
  itemList <- split(itemList, sort(itemList%%n))
  
  #Reads the market api based on the market list info and creates a data frame
  Market.json <- list()
  itemList1 <- paste(itemList[["0"]], collapse = ',')
  marketGet <- paste(EveMarketExt, itemList1, sep = '')
  Market.json <- fromJSON(readLines(marketGet))
  dfs <- lapply(Market.json, data.frame, stringsAsFactors = FALSE)
  
  for(i in 1:length(dfs)) {
    for(j in 1:16) {
      if (is.character(dfs[[i]][[j]]) == FALSE) {
        dfs[[i]][[j]] <- as.character(dfs[[i]][[j]])
      }
    }
  }
  print("Task 2 Done!")
  Market.Sheet <- bind_rows(dfs, .id = 'ID')
  
  #Searches through the market data data frames created above and consolidates them into one data frame
  for (item in 2:length(itemList)) {
    itemList1 <- paste(itemList[[item]], collapse = ',')
    marketGet <- paste(EveMarketExt, itemList1, sep = '')
    Market.json <- fromJSON(readLines(marketGet))
    dfs <- lapply(Market.json, data.frame, stringsAsFactors = FALSE)
    for(i in 1:length(dfs)) {
      for(j in 1:16) {
        if (is.character(dfs[[i]][[j]]) == FALSE) {
          dfs[[i]][[j]] <- as.character(dfs[[i]][[j]])
        }
      }
    }
    Market.Sheet2 <- bind_rows(dfs, .id = 'ID')
    Market.Sheet <- rbind(Market.Sheet, Market.Sheet2)
    progress(item, max.value = length(itemList), progress.bar = TRUE)
  } 
  print("Task 3 Done!")
  return(Market.Sheet)
}

# TODO: Data visualization
ggplot(HybridT1.Prices, aes(x=ID, y = ROI)) + 
  geom_point() + 
  geom_text_repel(label = HybridT1.Prices$itemType)

ggplot(testdf, aes(x = ID, y = value, fill = variable)) +
  geom_bar(stat = "identity") 