setwd("~/Desktop/R/Baseball/Bullpen")
require("nlme")
require("lattice")
require("latticeExtra")

###Load and pre-process 2011-2014 PECOTA projections

pecota11 <- read.csv("PECOTA_2011_PITCHERS.csv")
pecota11$YEAR <- 2011
pecota11$BB <- pecota11$BB + pecota11$HBP
pecota11 <- pecota11[, c("LASTNAME", "FIRSTNAME", "YEAR", "LG", "G", "GS", "IP", "HR", 
                       "BB", "SO", "ERA")]

pecota12 <- read.csv("PECOTA_2012_PITCHERS.csv")
pecota12$YEAR <- 2012
pecota12 <- pecota12[,c("LASTNAME", "FIRSTNAME", "YEAR", "LG", "G", "GS", "IP", "HR", 
                        "BB", "SO", "ERA")]

pecota13 <- read.csv("PECOTA_2013_PITCHERS.csv")
pecota13$YEAR <- 2013
pecota13 <- pecota13[,c("LASTNAME", "FIRSTNAME", "YEAR", "LG", "G", "GS", "IP", "HR", 
                        "BB", "SO", "ERA")]

pecota14 <- read.csv("PECOTA_2014_PITCHERS.csv")
pecota14$YEAR <- 2014
pecota14 <- pecota14[,c("LASTNAME", "FIRSTNAME", "YEAR", "LG", "G", "GS", "IP", "HR", 
                        "BB", "SO", "ERA")]
###Merge dataframes

pecota <- rbind(pecota11, pecota12, pecota13, pecota14)
pecota <- pecota[complete.cases(pecota),]
pecota <- cbind(NAME = paste(pecota$FIRSTNAME, pecota$LASTNAME, sep = " "), pecota)
pecota <- pecota[, c(1, 4:12)]

###Add FIP and FIP-

pecota$FIP <- (((13*pecota$HR) + (3 * pecota$BB) - (2 * pecota$SO)) / pecota$IP) + 3.2
lgavgFIP <- aggregate(pecota$FIP, by = list(pecota$YEAR), FUN = mean)
pecota$FIPminus <- 0
for(i in 1:length(pecota$NAME)){
  pecota$FIPminus[i] <- pecota$FIP[i] / lgavgFIP$x[pecota$YEAR[i] == lgavgFIP$Group.1]
}

###Load and pre-process LI of relievers with 50+ innings

LIdata <- read.csv("RP 50+IP 2011-2014.csv")

###Merge to create final data set

data <- merge(pecota, LIdata[, c(1:3, 5)], by = c("NAME", "YEAR"))
data <- data[data$TEAM != "- - -",]
data <- data[order(data$TEAM),]

write.csv(data, "finaldata.csv")

###Create model of current bullpen usage
currentregression <- lm(data$FIPminus ~ data$pLI)

###Create model of optimum bullpen usage for each team
optimal <- with(data, as.data.frame(cbind(as.character(TEAM), FIPminus, pLI)))
colnames(optimal) <- c("TEAM", "FIPminus", "pLI")

optimal <- with(optimal, optimal[order(TEAM, FIPminus),])

optimal2 <- with(optimal, optimal[order(TEAM, pLI, decreasing = TRUE),])
optimal2$pLI <- as.numeric(as.character(optimal2$pLI))
optimal2 <- optimal2[order(optimal2$TEAM, -optimal2$pLI),]

optimal$pLI <- optimal2$pLI
optimal$FIPminus <- as.numeric(as.character(optimal$FIPminus))

lms <- lmList(FIPminus ~ pLI | TEAM, data = optimal)

###Add predicted FIP- values to dataframe
data$predFIPminus <- predict(lms, newdata = data)

###Calculate RMSE
teamrmse <- aggregate((data$FIPminus - data$predFIPminus)^2, 
                       by = list(data$TEAM), FUN = mean)
teamrmse$x<- sqrt(teamrmse$x)
colnames(teamrmse) <- c("TEAM", "RMSE")
teamrmse <- teamrmse[order(-teamrmse[,2], decreasing = TRUE), ]
teamrmse

###Plots

####Blue Jays
jays <- xyplot(data$FIPminus[data$TEAM == "Blue Jays"] ~ 
                 data$pLI[data$TEAM == "Blue Jays"], type = c("p", "r"), xlab = "pLI",
                 ylab = "ProjFIP", main = "Toronto Blue Jays Bullpen Usage 2011-2014",
                 panel=function(x, y, ...) {
                 panel.xyplot(x, y, ...);
                 ltext(x=x, y=y, labels=data$NAME[data$TEAM == "Blue Jays"], 
                       pos=1, cex=0.8)})

optijays <- xyplot(FIPminus[TEAM == "Blue Jays"] ~ pLI[TEAM == "Blue Jays"], data = optimal,
                   type = "r", col = "red")
jays + optijays

####All Teams

allteams <- xyplot(data$FIPminus ~ data$pLI | data$TEAM, 
                      xlab = "Leverage Index Entering Game", 
                      ylab = "Preseason Projected FIP", type = c("p", "r"))

optiallteams <- xyplot(optimal$FIPminus ~ optimal$pLI | data$TEAM, 
                       type = "r", col = "red")

allteams + optiallteams


