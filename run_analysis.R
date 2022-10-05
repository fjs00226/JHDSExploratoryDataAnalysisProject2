# load and have a look at the data
unzip(zipfile= "./exdata_data_NEI_data.zip",exdir="./raw_data")

NEI<-readRDS("./raw_data/summarySCC_PM25.rds")
head(NEI)
str(NEI)

SCC <- readRDS("./raw_data/Source_Classification_Code.rds")
head(SCC)
str(SCC)

# Have total emissions from PM2.5 decreased in the United States from 
# 1999 to 2008? Using the base plotting system, make a plot showing the 
# total PM2.5 emission from all sources for each of the years 1999, 2002,
# 2005, and 2008.
Emission_year<-tapply(NEI$Emissions,list(NEI$year),sum)
png(file="./plot1.png", width=480, height=480)
barplot(Emission_year,xlab = "Year", ylab="Total PM2.5 / tons", main="Total PM2.5 Emission")
dev.off()
# Total PM 2.5 emission decreased from 1999 to 2008.

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
# (fips == "24510") from 1999 to 2008? Use the base plotting system to make 
# a plot answering this question.
library(ggplot2)
data_BC<-subset(NEI, fips=="24510")
Emission_BC_year<-aggregate(data_BC$Emissions,list(data_BC$year),sum)
colnames(Emission_BC_year)<-c("year","total_emission")

png(file="./plot2.png", width=480, height=480)
g2<-ggplot(Emission_BC_year,mapping=aes(x=factor(year),y=total_emission))+geom_col()
g2+ggtitle("Total PM2.5 Emission of Baltimore City")+xlab("Year")+ ylab("Total PM2.5 / tons")
dev.off()
# Overall decrease trend. Not a continuous decrease.

# Of the four types of sources indicated by the type (point, nonpoint, onroad, 
# nonroad) variable, which of these four sources have seen decreases in emissions 
# from 1999–2008 for Baltimore City? Which have seen increases in emissions from 
# 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
data_BC_3<-aggregate(data_BC$Emissions,list(data_BC$year,data_BC$type),sum)
colnames(data_BC_3)<-c("year","type","total_PM2.5")
png(file="./plot3.png", width=960, height=240)
g3<-ggplot(data_BC_3,mapping=aes(x=factor(year),y=total_PM2.5,fill=type))+geom_col()+facet_grid(.~type)
g3+ggtitle("PM2.5 emission of four types of sources")+xlab("Year")+ ylab("Total PM2.5 / tons")
dev.off()
# Decrease trend for non-road, non-point and on-road.

# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?
# get combustion and coal related SCC
SCC$EI.Sector<-as.character(SCC$EI.Sector)
SCC_good<-grepl("Comb",SCC$EI.Sector) & grepl("Coal",SCC$EI.Sector)
SCC_num<-SCC[SCC_good,"SCC"]

data_Comb_Coal<-subset(NEI,NEI$SCC %in% SCC_num)
data_Comb_Coal_year<-aggregate(data_Comb_Coal$Emissions,list(data_Comb_Coal$year),sum)
colnames(data_Comb_Coal_year)<-c("year","total_emission")
png(file="./plot4.png", width=480, height=480)
g4<-ggplot(data_Comb_Coal_year,mapping=aes(x=factor(year),y=total_emission))+geom_col()
g4+ggtitle("PM2.5 emission of coal combustion-related sources")+xlab("Year")+ylab("Total emission / tons")
dev.off()

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
SCC$Short.Name<-as.character(SCC$Short.Name)
SCC_good<-grepl("Motor",SCC$Short.Name)
SCC_num<-SCC[SCC_good,"SCC"]

data_Motor<-subset(NEI,NEI$SCC %in% SCC_num)
data_Motor_BC<-subset(data_Motor, fips=="24510")
data_Motor_BC_year<-aggregate(data_Motor_BC$Emissions,list(data_Motor_BC$year),sum)
colnames(data_Motor_BC_year)<-c("year","total_emission")
png(file="./plot5.png", width=480, height=480)
g5<-ggplot(data_Motor_BC_year,mapping=aes(x=factor(year),y=total_emission))+geom_col()
g5+ggtitle("PM2.5 emission of Motor Vehicle source in Baltimore City")+xlab("Year")+ylab("Total emission / tons")
dev.off()

# Compare emissions from motor vehicle sources in Baltimore City with emissions 
# from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?
state<-rep("BC",4)
data_Motor_BC_year<-cbind(data_Motor_BC_year,state)

data_Motor_LA<-subset(data_Motor, fips=="06037")
data_Motor_LA_year<-aggregate(data_Motor_LA$Emissions,list(data_Motor_LA$year),sum)
colnames(data_Motor_LA_year)<-c("year","total_emission")
state<-rep("LA",4)
data_Motor_LA_year<-cbind(data_Motor_LA_year,state)

data_Motor_BC_LA<-rbind(data_Motor_BC_year,data_Motor_LA_year)

png(file="./plot6.png", width=480, height=240)
g6<-ggplot(data_Motor_BC_LA,mapping=aes(x=factor(year),y=total_emission,fill=state))+geom_col()
g6<-g6+facet_grid(.~state)
g6+ggtitle("Comparison of Motor Vehicle source PM2.5 emission in \n Baltimore City and Los Angeles")+xlab("Year")+ylab("Total emission / tons")
dev.off()











