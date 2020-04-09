---
title: "Assignment 1"
output: "Parking Violations for Chryslers in LA FY2017"
---
  
## Question 2. 
parkingLA2017.df <- read.csv("parkingLA2017.csv", header = TRUE) # load data
dim(parkingLA2017.df) # find the dimensions of the dataframe 

## Question 3. 
library(dplyr)
CHRY <- filter(parkingLA2017.df, Make == "CHRY") # create a new DF with 'CHRY' as a filter
dim(CHRY) # find the new dimensions

## Question 4. a) 
anyNA(CHRY) # determine if any missing values exist with dataframe CHRY 

## Question 4. b) 
anyNA(CHRY$RP.State.Plate) # determine if variabble RP.State.Plate has missing values

## Question 4. c)
sd(CHRY$Fine.amount) # Find the standard deviation for 'Fine.amount' variable
anyNA(CHRY$Fine.amount) # Alternative determine if NA's exist for var 'Fine.amount' 
median(CHRY$Fine.amount, na.rm = TRUE) # Determine if the median is reasonble
mean(CHRY$Fine.amount, na.rm = TRUE) # Determine if the mean is reasonable
range(CHRY$Fine.amount, na.rm = TRUE) # Observe the range for the variable
hist(CHRY$Fine.amount) # Use the histogram to observe any outliers
CHRY$Fine.amount[is.na(CHRY$Fine.amount)] <- median(CHRY$Fine.amount, na.rm = TRUE) # use median
anyNA(CHRY$Fine.amount) # determine if NAs were replaced
sd(CHRY$Fine.amount) # re-run the standard deviation 

## Question 4. d)
CHRY$Location[CHRY$Location==""] <- "NA" # replace blanks with NA's
anyNA(CHRY$Location) # check for any NA's

## Question 4. e)
anyNA(CHRY$AGENCY.SHORT.NAME) # check to see if NA's exist within that variable
library(tidyr)
CHRY <- drop_na(CHRY, AGENCY.SHORT.NAME) # drop all NA's from 
table(CHRY$AGENCY.SHORT.NAME) # after checking, NA's do exist
CHRY$AGENCY.SHORT.NAME <- droplevels(CHRY$AGENCY.SHORT.NAME) # alternate to dropping NA's
View(CHRY) 

## Question 5. a)
CHRY$Issue.Date <- as.Date(CHRY$Issue.Date, format="%Y-%m-%d" ) # change the date based from View
str(CHRY) # recall the structure

## Question 5. b)
View(CHRY$Issue.Date) # filter by issue date or use View overall
CHRY <- arrange(CHRY, Issue.Date) # arrange the date in chronological order
View(CHRY) # recall View and the dates are in order

## Question 5. c)
november <- subset(CHRY, Issue.Date >= "2017-11-01" & Issue.Date <= "2017-11-30") # use the first/last of Nov
View(november)

## Question 6. 
CHRY2 <- CHRY[,-c(2)] # use the = symbol rather than <- if you were to completely remove on CHRY
View(CHRY2)

## Question 7. Creating summary for Fine.Amounts
summary(CHRY2$Fine.amount)

## Question 8. 
summary(CHRY2$Violation.Description) # shows all summary for violations desc for top five

## Question 9. 
top5vio <- as.data.frame(sort(table(CHRY2$Violation.Description), decreasing = TRUE)[1:5]) # create a new df
test <- filter(CHRY2, Violation.Description %in% top5vio$Var1) # pull the top5
dim(test) # check for varaibles
View(top5vio) # view your new dataframe

## Question 10.
library(ggplot2)
library(wesanderson)
top5bar <- ggplot(test, aes(Violation.Description, fill=Violation.Description)) + geom_bar() + scale_fill_manual(values = wes_palette(n=5, "BottleRocket2"))
top5bar <- top5bar + labs(title = "Top 5 Parking Violations for Chryslers in LA FY2017") # add title
top5bar <- top5bar + theme(axis.text.x = element_text(angle=45, hjust=1)) # adjust the labels on horizontal line
top5bar

## Question 11. 
Chrysler <- group_by(test, AGENCY.SHORT.NAME)
Chrysler2 <- summarise(Chrysler, AvgFine = mean(Fine.amount))
Chrysler2
table(test$AGENCY.SHORT.NAME)
AvgBar <- ggplot(Chrysler2, aes(x=AGENCY.SHORT.NAME, y=AvgFine)) + geom_bar(stat="identity", fill=topo.colors(n=8))
AvgBar <- AvgBar + labs(title = "Average Fines Per Agency in LA FY2017 - Chrysler models") 
AvgBar <- AvgBar + theme(axis.text.x = element_text(angle=45, hjust=1))
AvgBar

## Question 12
install.packages("Hmisc")
FineViolin <- ggplot(Chrysler, aes(x=AGENCY.SHORT.NAME, y=Fine.amount, fill=AGENCY.SHORT.NAME)) + geom_violin(trim=FALSE)
FineViolin <- FineViolin + stat_summary(fun.data="mean_sdl", mult=1, geom="crossbar", width=0.04 )
FineViolin <- FineViolin + labs(title = "Fine amounts per Agency in LA FY2017 - Chrysler models")
FineViolin <- FineViolin + theme(axis.text.x = element_text(angle=45, hjust=1))
FineViolin

## Question 14
TicketHist <- ggplot(Chryler, aes(x=Issue.time, fill=Violation.Description)) + geom_histogram(color="black")
TicketHist <- TicketHist + ggtitle("Ticket Issuance Per Hour") + xlab("Time Per Day (Military time)") + xlim(0, 2300)  
TicketHist
