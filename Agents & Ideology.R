#Agents & Ideology####
#Harold Walbert
#haroldwalbert@gmail.com

#Getting Started####
#Load Libraries
library(igraph);library(RNetLogo);library(skewt);library(dplyr);library(xlsx);library(ggplot2)
library(ggfortify);library(cluster);library(fpc);library(rgl);
library(RColorBrewer);library(scales);library(psych);library(car)
library(fitdistrplus);library(scatterplot3d)
#Make sure you set your working directory to where you ahve the data saved
setwd("C:/Users/Harold/Documents/CSS/Computational Econ CSS695/")

#Get the data...one is the full dataset and one is the ID to Name crosswalk
CountryData <- read.csv("PARTY19502011.csv")
CountryIDXWalk <- read.csv("CountryIDXwalk.csv")

#Clean and Prep data####
CCD <- CountryData
CCD[CCD == '-999'] <- NA

#Replace coid with Country Name
CCD <- left_join(CCD, CountryIDXWalk, by = "coid")
  
#What types of data do we have?
#coid is the country & year is year, elect1 is election year dummy,they should be coded as a factor
CCD$Country <- as.factor(CCD$Country); CCD$year <- as.factor(CCD$year); CCD$elect1 <- as.factor(CCD$elect1)
#Dont care about election month or day & will remove duplicative coid column
CCD$elmon <- NULL; CCD$elday <- NULL; CCD$coid <- NULL

#Keep only rows with complete data
#When this was not done it resulted in the NAs causing the graph to have obvious errors due
#to the presence of the NAs
#Remove Rows with NA 
CCD <- na.omit(CCD)

#Data Analysis####

#We just made sure the data was put in the appropriate classes
#We can now use this cleaned dataset to create a distance matrix, see:
#Gower, J. C. (1971) A general coefficient of similarity and some of its properties, Biometrics 27, 857-874.
#Kaufman, L. and Rousseeuw, P.J. (1990) Finding Groups in Data: An Introduction to Cluster Analysis. Wiley, New York.

#This creates a distance matrix of the remaining columns, 
#This daisy() function can create a distance matrix when there are different classes in the dataset
CCD_diff <- daisy(CCD, metric = "gower")
#We have to reduce the dimensionality. We will do that by using Prinical Component Analysis
PC_CCD <- prcomp(CCD_diff, scale = TRUE)

#Now that we have reduced dimensionality with PCA we can cluster
CCDclusters <- CCD
for (i in 1:20) {
  KM_CCD <- kmeans(CCD_diff, centers = i)
  CCDclusters[,(length(CCD) + i)] <- as.factor(KM_CCD$cluster)
  names(CCDclusters[,(31 + i)]) <- paste("NoCl", i, sep = "_")
}
#Put all the data in one place:
FinalData <- bind_cols(CCDclusters, data.frame(PC_CCD$rotation[,1:3]))

#New Stuff
FinalData$EF <- scales::rescale(x=FinalData$PC1, c(-8, 8))
FinalData$PF <- scales::rescale(x=FinalData$PC2, c(-8, 8))
FinalData$DT <- scales::rescale(x=FinalData$PC3, c(-8, 8))

#colnames(FinalData, c("V33", "V34"), c("clust2","clust3"))

print(paste("The Standard deviation of scaled PC1 (EF) is", sd(FinalData$EF)))
print(paste("The Standard deviation of scaled PC2 (PF) is", sd(FinalData$PF)))
print(paste("The Standard deviation of scaled PC3 (DT) is", sd(FinalData$DT)))

#Analysis & Visualization####
#View some plots
#We see that the first three PCs account for ~80% of the variance
Summary_PC_CCD <- summary(PC_CCD)
Summary_PC_CCD$importance[1:3,1:6]
plot(PC_CCD, main = "Variances of Princical Components")

#We are interested in the first three components since they explain most of what is going on in the data
#Lets look at the first two
plot(PC_CCD$rotation[,1:2], pch = 20,
     main = "First two Empirical Ideology Dimensions",
     sub = "Rotations of Principal Components 1 & 2"
     )


#Kinda cool:
# smoothScatter(PC_CCD$rotation[,1:2])

#Great article here:https://www.r-bloggers.com/scatterplot-matrices-in-r/
#Used it and tweaked it to get the following:
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}

#Then use function above to plot to see all three dimentions and their correlations:
scatterplotMatrix(~PC_CCD$rotation[,1] + PC_CCD$rotation[,2] + PC_CCD$rotation[,3],
                  upper.panel=panel.cor, pch = 20,
                  var.labels=c("PC1_Rotation","PC2_Rotation", "PC3_Rotation")
                  )
title(sub = "Distribtuions & Correlations of Dimensions of Political Ideology")



#Now lets take a look at all three dimensions

#This function allows you to plot in 3d either the x values or rotation values of a PCA
#the default is to view the rotation for three clusters using the rotated values
plotIdeologyClusters <- function(centers = 3, type = "rotation"){
  KM_CCD <- kmeans(CCD_diff, centers = centers)
  if (type == "x") {plot3d(PC_CCD$x[,1:3], col = KM_CCD$cluster); scatterplot3d(PC_CCD$x[,1:3], color = KM_CCD$cluster, pch = 20)}
  if (type == "rotation") {plot3d(PC_CCD$rotation[,1:3], col = KM_CCD$cluster); scatterplot3d(PC_CCD$rotation[,1:3], color = KM_CCD$cluster, pch = 20)}
  title(main = "Dimensions of Political Ideology", sub = paste("Colors show", centers, "clusters"))
  KM_CCD <<- KM_CCD
}

#Now that we have the clusters for each observation we use 3 clusters as example with rotation
plotIdeologyClusters(centers = 3)

#This is just interesting. Will not go into detail on this
#Plot by country
#Static 2D
plot(PC_CCD$rotation[,1:3], col = as.factor(as.integer(CCD$Country)))
#Interactive 3D
plot3d(PC_CCD$rotation[,1:3], col = as.factor(as.integer(CCD$Country)))
scatterplot3d(FinalData[,52:54], color = as.integer(FinalData$Country), pch = 20)

#Show specific countries ideological distributions:
PlotCountryIdeology <- function(CountryToFilter = "United States"){
  CountryFinalData <- filter(FinalData, Country == CountryToFilter)
  scatterplot3d(CountryFinalData[,52:54], highlight.3d = TRUE,
                main = paste("3D Ideological Distribution:", CountryToFilter), cex.lab=0.75)
  
  #scatterplot3d(CountryFinalData[,52:54], color = CountryFinalData$V35, pch = 20)
  plot3d(CountryFinalData[,52:54])
  
  CountryFinalData <- data.frame(CountryFinalData$year, CountryFinalData$Country, CountryFinalData[,33:37], CountryFinalData[,52:54])
  
  CountryFinalData <<- CountryFinalData
}


#Understanding and creating empirically based distributions####
#What about the first 3 principal components? Are they distributed normally?
#Lets check using the shapiro test for normality 
shapiro.test(PC_CCD$rotation[,1])
shapiro.test(PC_CCD$rotation[,2])
shapiro.test(PC_CCD$rotation[,3])
#We see that we can reject the null hypothesis of normality
ks.test(PC_CCD$rotation[,1], "pnorm", mean=mean(PC_CCD$rotation[,1]), sd=sd(PC_CCD$rotation[,1]))

#now test other distributions
fitdistr(abs(PC_CCD$rotation[,1]), "weibull")
ks.test(PC_CCD$x[,1], "pweibull", scale=.0300335846, shape=3.5980409221)






# 
# #####################################
# # #Save image of data for RMarkDown
# # save.image(file = "PoliticalSpectrumClustering.RData")
# # # ###Load the Rational Voter NetLogo Model
# NLLoadModel('C:/Users/Harold/Documents/CSS/Computational Econ CSS695/RationalVoterModel_v.2.nlogo')
# #Use 3D Netlogo
# setwd('C:/Program Files (x86)/NetLogo 5.2.1')
# ###This code starts the NetLogo GUI
# nl.path <- getwd()
# NLStart(nl.path, is3d = TRUE)
# NLLoadModel('C:/Users/Harold/Documents/CSS/Computational Econ CSS695/3d_Ideological_Agents.nlogo3d')



#So lets just sample from the empirical data
#This function gets us a number of observations:
getVoterPopulation <- function(numAgents){
  VoterPopulation <- sample_n(FinalData, numAgents, replace = TRUE)
  #scatterplot3d(VoterPopulation[,52:54], highlight.3d = TRUE)
  
  #Need to rescale for netlogo
  VoterPopulation$EF <- scales::rescale(x=VoterPopulation$PC1, c(-8, 8))
  VoterPopulation$PF <- scales::rescale(x=VoterPopulation$PC2, c(-8, 8))
  VoterPopulation$DT <- scales::rescale(x=VoterPopulation$PC3, c(-8, 8))
  VoterPopulation$breed <- "Voter"
  
  VoterPopulation <<- VoterPopulation
  
}

setupAgents <-function(numAgents = 1000, graphType = "SmallWorld"){
#random graph
if(graphType == "SmallWorld") {g <- sample_smallworld(1, numAgents, 5, 0.05)}
if(graphType == "Random") {g <- erdos.renyi.game(numAgents, 1/100)}
if(graphType == "ScaleFree") {g <- barabasi.game(numAgents, power = 1)}

getVoterPopulation(numAgents = numAgents)
V(g)$EF=as.numeric(VoterPopulation$EF[1:numAgents])
V(g)$PF=as.numeric(VoterPopulation$PF[1:numAgents])
V(g)$DT=as.numeric(VoterPopulation$DT[1:numAgents])
V(g)$breed=as.character(VoterPopulation$breed[1:numAgents])
#V(g)$candidate = as.character(VoterPopulation$candidate[1:numAgents])
plot(g, edge.arrow.size=0.5, vertex.label = NA, vertex.size = 2)
g <<- g
write.graph(g, paste(graphType,"GraphForNetLogoModel.graphml", sep = "_"), format=c("graphml"))

}
# 
# plot(degree.distribution(g), type = "l")
# hist(degree.distribution(g))
# plot(g, edge.arrow.size=0.5, vertex.label = NA, vertex.size = 2)
# summary(g)
# graph.density(g)
# 
# plot(degree.distribution(g), xlab="degree",ylab="frequency", log="xy", pch=3, col=3, type="l")
# 
# #add properties to the agents in the network
# V(Year1945Graph)$milex_percap=as.numeric(NMC1945$milex_percap[match(V(Year1945Graph)$name,NMC1945$ccode)])
# V(g)$EF=as.numeric(FinalData$PC1[1:vcount(g)])
# 
# 
# 
# 
# par(mfrow=c(2,2))
# PlotCountryIdeology("Australia")
# PlotCountryIdeology("Austria")
# PlotCountryIdeology("Belgium")
# PlotCountryIdeology("Canada")
# PlotCountryIdeology("Denmark")
# PlotCountryIdeology("Finland")
# PlotCountryIdeology("France")
# PlotCountryIdeology("West Germany")
# PlotCountryIdeology("Ireland")
# PlotCountryIdeology("Italy")
# PlotCountryIdeology("Japan")
# PlotCountryIdeology("Netherlands")
# PlotCountryIdeology("New Zealand")
# PlotCountryIdeology("Norway")
# PlotCountryIdeology("Sweden")
# PlotCountryIdeology("Switzerland")
# PlotCountryIdeology("United Kingdom")
# PlotCountryIdeology("United States")
# PlotCountryIdeology("Greece")
# PlotCountryIdeology("Portugal")
# PlotCountryIdeology("Spain")
# 
# 
# 
# 
# #Now Analyze the experiment results####
# test <- read.csv("C:/Users/Harold/Documents/CSS/Computational Econ CSS695/Experiments/3d_Ideological_Agents_v1 ScaleFree_-table.csv", skip = 6)
# test <- test[,2:ncol(test)]
# names(test) <- c("NetworkType","RandomInitialCandidatePositions","Corruption","discountL","discoutR",
#                  "VotingCost", "Cost", "tick", "NumTurtles", "WinPercent","Winner","Flip")
# test <- filter(test, tick == 20)
# test$Winner <- as.character(test$Winner)
# test$Winner <- gsub("[[:punct:]]", "", as.list(test$Winner))
# test <- mutate(test, UniqueConcat = paste(NetworkType, RandomInitialCandidatePositions, Corruption, discountL,
#                                           discoutR, VotingCost, Cost, NumTurtles, sep = "-"))
# 
# results <- summarise(group_by(test, UniqueConcat), winPercent = ((sum(test$Winner == "CandidateL") / sum(test$Winner == "CandidateR"))*100))
# 



