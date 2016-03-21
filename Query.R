source("Function.R")

# Get the required data
dfTag <- generateTagRow()

# Yahoo finance csv download url
szURL <- paste0("http://download.finance.yahoo.com/d/quotes.csv?s=",paste(listSymbols(),collapse=","),"&f=", paste(dfTag$tag, collapse=""),"&e=.csv")
dfData <- read.csv(szURL,header=FALSE)
names(dfData) <- dfTag$description
dfData <- addGrahamNumber(dfData)
print(dfData)
