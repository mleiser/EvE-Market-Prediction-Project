EveMarketExt <- paste0("https://api.evemarketer.com/ec/marketstat/json?typeid=18&usesystem=30000142")
Market.json <- fromJSON(readLines(EveMarketExt), simplifyDataFrame = TRUE)

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

NamStart <- c('buy.forQuery.bid', 'buy.forQuery.types', 'buy.forQuery.regions', 'buy.forQuery.systems', 
              'buy.forQuery.hours', 'buy.forQuery.minq', 'buy.volume', 
         'buy.wavg', 'buy.avg', 'buy.variance', 'buy.stdDev', 'buy.median', 'buy.fivePercent', 'buy.max', 
         'buy.min', 'buy.highToLow', 'buy.generated', 'sell.forQuery.bid', 'sell.forQuery.types', 
         'sell.forQuery.regions', 'sell.forQuery.systems', 'sell.forQuery.hours', 'sell.forQuery.minq', 
         'sell.volume', 'sell.wavg', 'sell.avg', 'sell.variance', 'sell.stdDev', 
         'sell.median', 'sell.fivePercent', 'sell.max', 'sell.min', 'sell.highToLow', 'sell.generated')
typeids$`Parent ID` <- ParentGroups 

#Pulls unique values for Parent groups
ParentGroupsU <- unique(ParentGroups)

#Creates an empty data frame to be filled by the API
Market.json <- data.frame(matrix(ncol = 34, nrow = 0))
colnames(Market.json) <- NamStart

#TODO 
# Figure out how to utilize data.table to subset by parent/market groups

#Creates the market data table
for (i in 1:length(typeids$`Type ID`)) {
  for(j in 1:4){
    if (typeids$`Parent ID`[i] == ParentGroupsU[j]){
      EveMarketExt <- sprintf("https://api.evemarketer.com/ec/marketstat/json?typeid=%d&usesystem=30000142", typeids$`Type ID`[i])
      Market.json[nrow(Market.json) + 1,] = list(fromJSON(readLines(EveMarketExt), simplifyVector = TRUE, flatten = TRUE))
      #Market.json <- Market.json %>% add_row(fromJSON(readLines(EveMarketExt), simplifyVector = TRUE))
    }
  }
}
colnames(Market.json) <- Nam
view(Market.json)