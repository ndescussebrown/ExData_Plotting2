plot1 <- function() { 

  ## Unzip assignment input data file and reads the data in  
  unzip("exdata_data_NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")

  ## create plot1.png and specifies the number of pixels  
  png(file = "plot1.png",width = 480, height = 480, units="px")
  plotdata <- tapply(NEI$Emissions,NEI$year,FUN=sum)
  plot(as.numeric(names(plotdata)),plotdata,xaxt="n",xlim=c(1998,2008),
       xlab="Year",ylab="Total PM2.5 Emission (Tons)",main="Total PM2.5 Emission from all Sources",type="b",pch=1)
  lines(plotdata,type="l",col="blue")
  axis(1, at = 1998:2008)
  dev.off()
}