plot6 <- function() { 
  
  library("ggplot2")
  library("reshape2")
  
  ## Unzip assignment input data file and reads the data in  
  unzip("exdata_data_NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ## create plot6.png and specifies the number of pixels  
  png(file = "plot6.png",width = 480, height = 480, units="px")
  motorvscc <- grep("[Mm][Oo][Tt][Oo][Rr] [Vv]",SCC$SCC.Level.Three)
  motorvnei <- which(as.character(NEI$SCC) %in% SCC[motorvscc,1])
  baltimoredata <- grep("24510",NEI$fips)
  losangelesdata <- grep("06037",NEI$fips)
  indbalti <- intersect(motorvnei,baltimoredata)
  indlosang <- intersect(motorvnei,losangelesdata)
  
  ## calculates total emissionfrom motor vehicle sources for Baltimore and Los Angeles per type and year
  plotdatabalti <- tapply(NEI$Emissions[indbalti],NEI$year[indbalti],FUN=sum)
  plotdatalosang <- tapply(NEI$Emissions[indlosang],NEI$year[indlosang],FUN=sum)
  
  ## reformats data for plotting
  plotdatabaltim <- melt(plotdatabalti,variable.name="emissions")
  plotdatabaltim$city <- "Baltimore"
    plotdatalosangm <- melt(plotdatalosang,variable.name="emissions")
  plotdatalosangm$city <- "Los Angeles"
  plotdataall <- rbind(plotdatalosangm,plotdatabaltim)
  
  
  ## plots data
  ggplot(plotdataall,aes(x=Var1,y=value,color=city,group=city)) + geom_line() + 
    geom_point() + xlab("Year") + ylab("Total PM2.5 Emission from Motor Vehicle Sources in Tons") +
    ggtitle("Total PM2.5 Emission from Motor Vehicle Sources\nin Baltimore and Los Angeles") +
    theme(plot.margin = unit(c(1,0.5,1,1), "cm"))
  ggsave(file="plot6.png")
}

