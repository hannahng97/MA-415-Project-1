
A2010 <- read.csv("BP Apprehensions 2010.csv", header = TRUE, stringsAsFactors = FALSE)
A2017 <- read.csv("PB Apprehensions 2017.csv", header = TRUE, stringsAsFactors = FALSE)


rownames(A2010) <- A2010[,1]
A2010[,1]
rownames(A2017) <- A2017[,1]

getTotals <- function(data) {
  data <-  subset(data, select= -c(Sector))
  rownames(data)
  
  data <- rbind(data, colSums(data))
  
  rownames(data)
  
  -length(rownames(data))  
  
  rownames(data) <- c(rownames(data)[-length(rownames(data))], "Total")
  
  data <- cbind(data,rowSums(data))
  
  colnames(data) <- c(colnames(data)[-length(colnames(data))], "Total")
  return(data)
  
}

#Append totals for data
A2010 <- getTotals(A2010)
A2017 <- getTotals(A2017)

compare <- function(){
  
  for (x in 1:9) {
    
    title <- rownames(A2010)[x]
    
    both <- rbind(A2010[x,1:12], A2017[x,1:12])
    
    row.names(both) <- c("2010", "2017")
    
    barplot(as.matrix(both), beside = TRUE, col = c("red", "blue"), bty="n",xlab = 'Month', ylab = 'Number of Apprehensions' )
    
    legend("topright", c("2010","2017"), pch=15,  col=c("red","blue"),  bty="n", title = title)
    
  }
}
compare()

barplot(A2010[1:9,13], 
        names.arg = rownames(A2010), 
        las=2,
        axisnames=TRUE,
        main="2010 Border Patrol Apprehensions by Sector",
        border="blue",
        col="yellow")

barplot(A2017[1:9,13], 
        names.arg = rownames(A2017), 
        las=2,
        axisnames=TRUE,
        main="2017 Border Patrol Apprehensions by Sector",
        border="blue",
        col="yellow")

#T Test
customTest <- function(earlyData, newData) {
  #Assumes getTotals function has been run on data
  #Data for old data input
  maximumOld <- max(earlyData[1:9,13])
  maxRowIndexOld <- which.max(earlyData[1:9,13])
  earlyDataMean <- rowMeans(earlyData[maxRowIndexOld,1:12])
  #earlyDataSd <- sd(earlyData[maxRowIndexOld,1:12])
  #earlyDataQuantile <- quantile(earlyData[maxRowIndexOld,1:12])
  
  #Data for new data input
  maximumNew <- max(newData[1:9,13])
  maxRowIndexNew <- which.max(newData[1:9,13])
  newDataMean <- rowMeans(newData[maxRowIndexNew,1:12])
  #newDataSd <- sd(newData[maxRowIndexNew,1:12])
  #newDataQuantile <- quantile(newData[maxRowIndexNew,1:12])
  
  t.test(earlyData[maxRowIndexOld,1:12], newData[maxRowIndexNew,1:12])
  
  
}

#Get top three months
topMonths <- function(data){
  copy <- data
  copy <- sort(copy[10,1:12], decreasing = TRUE)[1:3]
  copy
}

compareThreeMonthPeriod <- function(earlyData, newData) {
  earlyDataThree <- topMonths(earlyData)
  newDataThree <- topMonths(newData)
  earlyColName <- colnames(earlyDataThree[1])
  earlyIndex <- grep(earlyColName, colnames(earlyDataThree))
  
  newColName <- colnames(newDataThree[1])
  newIndex <- grep(newColName, colnames(newDataThree))
  
   earlyMean <- mean(earlyData[1:9,earlyIndex:earlyIndex + 2])
   newMean <- mean(newData[1:9,newIndex:newIndex + 2])
   
   
   
   t.test(earlyData[1:9,earlyIndex:earlyIndex + 2],newData[1:9,newIndex:newIndex + 2])
   

}
ts <- readClipboard(raw = FALSE)

ts1 <- read.table("clipboard", sep = "\t")
ts1

ts2 <- rev(as.vector(t(ts1)))

ts3 <- ts(ts2, start = c(2000,10), frequency=12)

ts.plot(ts3, gpars=list(xlab="year", ylab="Apprehensions", lty=c(1:3)), col = 'blue')

