plot4 <- function() { 
  
  ## Unzip assignment input data file and reads the data in  
  unzip("exdata_data_NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ## create plot4.png and specifies the number of pixels  
  png(file = "plot4.png",width = 480, height = 480, units="px")
  coalcombus <- grep("[Cc][Oo][Aa][lL]",SCC$SCC.Level.Three)
  ind <- which(as.character(NEI$SCC) %in% SCC[coalcombus,1])
  plotdata <- tapply(NEI$Emissions[ind],NEI$year[ind],FUN=sum)
  plot(as.numeric(names(plotdata)),plotdata,xaxt="n",xlim=c(1998,2008),
       xlab="Year",ylab="Total emissions from coal combustion-related sources (Tons)",main="Total emissions from coal combustion-related sources",type="b",pch=1)
  lines(plotdata,type="l",col="blue")
  axis(1, at = 1998:2008)
  dev.off()
}