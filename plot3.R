plot3 <- function() { 
  
  library("ggplot2")
  library("reshape2")
  
  ## Unzip assignment input data file and reads the data in  
  unzip("exdata_data_NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ## create plot3.png and specifies the number of pixels  
  baltimoredata <- grep("24510",NEI$fips)
  
  ## calculates total emission per type and year
  plotdata <- tapply(NEI$Emissions[baltimoredata],list(NEI$type[baltimoredata],NEI$year[baltimoredata]),FUN=sum)
  
  ## reformats data for plotting
  plotdata2 <- melt(plotdata)
  names(plotdata2)[1] <-"type" 
  
  ## plots data
  ggplot(plotdata2,aes(x=Var2,y=value,color=type,group=type)) + geom_line() + 
    geom_point() + xlab("Year") + ylab("Total PM2.5 Emission in Tons") +
    ggtitle("Total PM2.5 Emission from all Sources in Baltimore City")
  ggsave(file="plot3.png")
}