generateTagRow <- function(){
  # "Private" function
  addTagRow <- function(dfTag, szTag, szDescription ){
    return (rbind(dfTag,data.frame(tag=szTag, description=szDescription)))
  }
  # Creation of require tag
  dfTag = data.frame(tag=character(0),description=character(0))
  dfTag <- addTagRow(dfTag,"s","Symbol")
  dfTag <- addTagRow(dfTag,"n","Name")
  # dfTag <- addTagRow(dfTag,"l","Last Trade (with time)")
  dfTag <- addTagRow(dfTag,"l1","Last Trade (without time)")
  # dfTag <- addTagRow(dfTag,"d1","Last Trade Date")
  # dfTag <- addTagRow(dfTag,"t1","Last Trade Time")
  # dfTag <- addTagRow(dfTag,"k3","Last Trade Size")
  # dfTag <- addTagRow(dfTag,"c","Change and Percent Change")
  # dfTag <- addTagRow(dfTag,"c1","Change")
  # dfTag <- addTagRow(dfTag,"p2","Change in Percent")
  # dfTag <- addTagRow(dfTag,"t7","Ticker Trend")
  # dfTag <- addTagRow(dfTag,"v","Volume")
  # dfTag <- addTagRow(dfTag,"a2","Average Daily Volume")
  # dfTag <- addTagRow(dfTag,"i","More Info")
  # dfTag <- addTagRow(dfTag,"t6","Trade Links")
  # dfTag <- addTagRow(dfTag,"b","Bid")
  # dfTag <- addTagRow(dfTag,"b6","Bid Size")
  # dfTag <- addTagRow(dfTag,"a","Ask")
  # dfTag <- addTagRow(dfTag,"a5","Ask Size")
  # dfTag <- addTagRow(dfTag,"p","Previous Close")
  # dfTag <- addTagRow(dfTag,"o","Open")
  # dfTag <- addTagRow(dfTag,"m","Day’s Range")
  # dfTag <- addTagRow(dfTag,"w","52 Week Range")
  # dfTag <- addTagRow(dfTag,"j5","Change from 52 Week Low")
  # dfTag <- addTagRow(dfTag,"j6","Percent Change from 52 Week Low")
  # dfTag <- addTagRow(dfTag,"k4","Change from 52 Week High")
  # dfTag <- addTagRow(dfTag,"k5","Percent Change from 52 Week High")
  dfTag <- addTagRow(dfTag,"e","Earnings/Share")
  dfTag <- addTagRow(dfTag,"r","P/E Ratio")
  # dfTag <- addTagRow(dfTag,"s7","Short Ratio")
  # dfTag <- addTagRow(dfTag,"r1","Dividend Pay Date")
  # dfTag <- addTagRow(dfTag,"q","Ex-Dividend Date")
  dfTag <- addTagRow(dfTag,"d","Dividend/Share")
  dfTag <- addTagRow(dfTag,"y","Dividend Yield")
  # dfTag <- addTagRow(dfTag,"f6","Float Shares")
  dfTag <- addTagRow(dfTag,"j1","Market Capitalization")
  # dfTag <- addTagRow(dfTag,"t8","1 Year Target Price")
  dfTag <- addTagRow(dfTag,"e7","EPS Est. Current Year")
  dfTag <- addTagRow(dfTag,"e8","EPS Est. Next Year")
  dfTag <- addTagRow(dfTag,"e9","EPS Est. Next Quarter")
  dfTag <- addTagRow(dfTag,"r6","Price/EPS Est. Current Year")
  dfTag <- addTagRow(dfTag,"r7","Price/EPS Est. Next Year")
  dfTag <- addTagRow(dfTag,"r5","PEG Ratio")
  dfTag <- addTagRow(dfTag,"b4","Book Value")
  dfTag <- addTagRow(dfTag,"p6","Price/Book")
  dfTag <- addTagRow(dfTag,"p5","Price/Sales")
  dfTag <- addTagRow(dfTag,"j4","EBITDA")
  # dfTag <- addTagRow(dfTag,"m3","50 Day Moving Average")
  # dfTag <- addTagRow(dfTag,"m7","Change from 50 Day Moving Average")
  # dfTag <- addTagRow(dfTag,"m8","Percent Change from 50 Day Moving Average")
  # dfTag <- addTagRow(dfTag,"m4","200 Day Moving Average")
  # dfTag <- addTagRow(dfTag,"m5","Change from 200 Day Moving Average")
  # dfTag <- addTagRow(dfTag,"m6","Percent Change from 200 Day Moving Average")
  # dfTag <- addTagRow(dfTag,"s1","Shares Owned")
  # dfTag <- addTagRow(dfTag,"p1","Price Paid")
  # dfTag <- addTagRow(dfTag,"c3","Commission")
  # dfTag <- addTagRow(dfTag,"v1","Holdings Value")
  # dfTag <- addTagRow(dfTag,"w1","Day’s Value Change")
  # dfTag <- addTagRow(dfTag,"g1","Holdings Gain Percent")
  # dfTag <- addTagRow(dfTag,"g4","Holdings Gain")
  # dfTag <- addTagRow(dfTag,"d2","Trade Date")
  # dfTag <- addTagRow(dfTag,"g3","Annualized Gain")
  # dfTag <- addTagRow(dfTag,"l2","High Limit")
  # dfTag <- addTagRow(dfTag,"l3","Low Limit")
  # dfTag <- addTagRow(dfTag,"n4","Notes")
  # dfTag <- addTagRow(dfTag,"k1","Last Trade (Real-time) with Time")
  # dfTag <- addTagRow(dfTag,"b3","Bid (Real-time)")
  # dfTag <- addTagRow(dfTag,"b2","Ask (Real-time)")
  # dfTag <- addTagRow(dfTag,"k2","Change Percent (Real-time)")
  # dfTag <- addTagRow(dfTag,"c6","Change (Real-time)")
  # dfTag <- addTagRow(dfTag,"v7","Holdings Value (Real-time)")
  # dfTag <- addTagRow(dfTag,"w4","Day’s Value Change (Real-time)")
  # dfTag <- addTagRow(dfTag,"g5","Holdings Gain Percent (Real-time)")
  # dfTag <- addTagRow(dfTag,"g6","Holdings Gain (Real-time)")
  # dfTag <- addTagRow(dfTag,"m2","Day’s Range (Real-time)")
  # dfTag <- addTagRow(dfTag,"j3","Market Cap (Real-time)")
  dfTag <- addTagRow(dfTag,"r2","P/E (Real-time)")
  # dfTag <- addTagRow(dfTag,"c8","After Hours Change (Real-time)")
  # dfTag <- addTagRow(dfTag,"i5","Order Book (Real-time)")
  dfTag <- addTagRow(dfTag,"x","Stock Exchange")  
  return (dfTag)
}

listSymbols <- function(){
  vCompany <- vector()
  #dfCompany <- read.csv("Symbols.txt", sep=";")
  #vCompany <- as.character(dfCompany[, 1])
  dfCompany <- read.csv("SGXSymbol.csv", sep=";")
  vCompany <- unlist(Map(function(x){paste(x,".SI",sep="")},  as.character(dfCompany[, 1])))
  return (vCompany)
}

# http://www.investopedia.com/terms/g/graham-number.asp
addGrahamNumber <- function(dfData){
  dfData$"Earnings/Share Num" <- as.numeric(as.character(dfData$"Earnings/Share"))  
  # Filter off Negative earning/share
  dfData <- dfData[dfData$'Earnings/Share Num' > 0,]
  # Calculate the grahamNumber
  dfTemp <- within(dfData, {
    GrahamNumber <- round(sqrt(22.5 * dfData$"Earnings/Share Num" * dfData$"Book Value"),digits=3)
  })
  # Retrieve only the require column, names(dfTemp)
  dfGrahamNumber <- dfTemp[,c("Symbol","Name","Last Trade (without time)","GrahamNumber","Earnings/Share","Book Value","P/E Ratio")]  
  return(dfGrahamNumber)
}

plotGrahmNumber <- function(dfData){
  # Define 2 vectors
  colors <- c("blue", "red")
  labels <- c("LastTrade", "Graham Number")
  plot(dfData$`Last Trade (without time)`, type="o", col=colors[1], pch="T",xaxt='n',ann=FALSE)
  lines(dfData$GrahamNumber, type="o", col=colors[2], pch="G")
  legend(20, 40, legend=labels,col=colors, lty=1, cex=0.6)
  axis(1, at=1:nrow(dfData), labels=dfData$Symbol,par(las=3))
}


