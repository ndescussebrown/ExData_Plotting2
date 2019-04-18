plot5 <- function() { 
  
  ## Unzip assignment input data file and reads the data in  
  unzip("exdata_data_NEI_data.zip")
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  ## create plot5.png and specifies the number of pixels  
  png(file = "plot5.png",width = 480, height = 480, units="px")
  motorvscc <- grep("[Mm][Oo][Tt][Oo][Rr] [Vv]",SCC$SCC.Level.Three)
  motorvnei <- which(as.character(NEI$SCC) %in% SCC[motorvscc,1])
  baltimoredata <- grep("24510",NEI$fips)
  ind <- intersect(motorvnei,baltimoredata)
  plotdata <- tapply(NEI$Emissions[ind],NEI$year[ind],FUN=sum)
  plot(as.numeric(names(plotdata)),plotdata,xaxt="n",xlim=c(1998,2008),
       xlab="Year",ylab="Total emissions from motor vehicles sources (Tons)",main="Total emissions from motor vehicles sources in Baltimore",type="b",pch=1)
  lines(plotdata,type="l",col="blue")
  axis(1, at = 1998:2008)
  dev.off()
}

