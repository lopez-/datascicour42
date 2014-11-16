# set the working directory on which the datasets are stored
setwd("C:/Users/Victor/Desktop/Project 2")

# load all the libraries that will be required below
library(dplyr)
library(ggplot2)

# load the data
code <- readRDS("Source_Classification_Code.rds")
summary <- readRDS("summarySCC_PM25.rds")

# create plot 1
# 1.1 - sum all the emissions from summary and group it by year through dplyr
totals <- summary %>%
        group_by(year) %>%
        summarise(sum(Emissions))

# 1.2 - in order to have a clearer view on the plot, divide emissions by 1m
totals[,2] <- totals[,2]/1000000

# 1.3 - plot the graph
barplot(totals[,2],names.arg=totals[,1],space=F,col=rainbow(4),main="Emissions of PM2.4",xlab="Year",ylab="Emissions of PM2.4(millions)",las=1)

# create plot 2
# 2.1 - create a data frame "balt" from its corresponding code, fips, from summary in order to have data only for Baltimore
balt <- subset(summary,fips=="24510")

# 2.2. - similarly to 1.1 sum and group emissions by year
totals_balt <- balt %>%
        group_by(year) %>%
        summarise(sum(Emissions))

# 2.3 - plot the graph
barplot(totals_balt[,2],names.arg=totals_balt[,1],space=F,col=rainbow(4),main="Emissions of PM2.4",xlab="Year",ylab="Emissions of PM2.4",las=1)

# create plot 3
# 3.1 - leveraging the "balt" dataframe created on 2.1, create another, totals_type, which
# stores data for Baltimore and sum and groups by year and type
totals_type <- balt %>%
        group_by(year,type) %>%
        summarise(sum(Emissions))

# 3.2 - change the name of the third column so qplot doesn't prompt an error message
colnames(totals_type)[3] <- "Emissions"

# 3.3 - divide emissions by 1000 for the plot to be clearer and be expressed in thousands
totals_type[3] <- totals_type[3]/1000

# 3.4 - plot the graph
qplot(year,Emissions,data=totals_type,color=type,facets=type~.,ylab="Emissions of PM2.4 (thousands)") + geom_line()

# plot 4
coal.code <- grep("Coal",code$EI.Sector)

sccs <- code[coal.code,][,1]

sccs <- data.frame("SCC"=sccs)

sum.sccs <- merge(summary,sccs,by="SCC")

totals.sccs <- sum.sccs %>%
        group_by(year) %>%
        summarise(sum(Emissions))

colnames(totals.sccs)[2] <- "Emissions"

qplot(year,Emissions,data=totals.sccs,ylab="Emissions of PM2.4") + geom_line()

# plot 5
onroad <- subset(summary,summary$type=="ON-ROAD")

totals.onroad <- onroad %>%
        group_by(year) %>%
        summarise(sum(Emissions))

colnames(totals.onroad)[2] <- "Emissions"

qplot(year,Emissions,data=totals.onroad,ylab="Emissions of PM2.4") + geom_line()

# plot 6
onroad.balt.la <- subset(summary,summary$type=="ON-ROAD" & (summary$fips=="24510" | summary$fips=="06037"))

onroad.la <- subset(summary,summary$type=="ON-ROAD" & summary$fips=="06037")

totals.onroad.balt.la <- onroad.balt.la %>%
        group_by(year,fips) %>%
        summarise(sum(Emissions))

for(i in 1:nrow(totals.onroad.balt.la)){
        if(totals.onroad.balt.la[i,2]=="06037"){
                totals.onroad.balt.la[i,2] <- "Los Angeles"
        } else {
                totals.onroad.balt.la[i,2] <- "Baltimore"
        }
}

colnames(totals.onroad.balt.la)[3] <- "Emissions"

qplot(year,Emissions,data=totals.onroad.balt.la,color=fips,facets=fips~.,ylab="Emissions of PM2.4 (thousands)") + geom_line()


