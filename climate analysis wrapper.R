#Wrapper script to do climate experiments with tinker toy model
#mosaics group 23 Oct 2019

#Load main tinker toy functions
source('tinkerToy_analytical.R')
source('addRandomLake.R')

#Import Savannah River network structure
network1 <- read.csv("SORTED_savannah_river_v2.csv",stringsAsFactors=FALSE)

#There are a few reaches in this network for which localArea=0. This creates problems in calculating qL, qN, residence time.
#See code currently at line 118 for details.
#Fill in mean of non-zero localArea for all first order streams.
network1$localArea[network1$localArea==0 & network1$order==1] <- mean(network1$localArea[network1$localArea>0 & network1$order==1])

##
#Set up a couple different network structures

#First - a network with lots of headwater lakes
#>>Note that some of these lakes may be quite long - e.g. see hist(network1$length[network1$order==1])). Consider re-writing addRandomLake so that lakes get put only in shorter reaches
networkHead <- addRandomLake(network=network1,order=rep(1,1000),width=100,depth=10,lake_d=0.005,verbose=TRUE)

#Second - a network with a few tailwater lakes
#Make the total lake area (and volume) the same between networkHead and networkTail
#Calculate total area across all lakes, in m2
areaHeadLakes <- sum(networkHead$length[networkHead$lake==1]*networkHead$width[networkHead$lake==1])
print(paste('Area of headwater lakes (m2) is',areaHeadLakes))
print(paste('Volume of headwater lakes (m3) is',sum(networkHead$length[networkHead$lake==1]*networkHead$width[networkHead$lake==1]*networkHead$depth[networkHead$lake==1])))
#Now make the network with one tailwater lake that has this area
networkTail <- network1
whichReach <- sample(network1$reachID[network1$order==7],size=1)
networkTail[networkTail$reachID==whichReach,"width"] <- areaHeadLakes/networkTail[networkTail$reachID==whichReach,"length"]
networkTail[networkTail$reachID==whichReach,"depth"] <- 10
#Check - what is the area and volume of the one tailwater lake?
print(paste("Area of tailwater lake (m2) is",networkTail[networkTail$reachID==whichReach,"width"]*networkTail[networkTail$reachID==whichReach,"length"]))
print(paste("Volume of tailwater lake (m3) is",networkTail[networkTail$reachID==whichReach,"width"]*networkTail[networkTail$reachID==whichReach,"length"]*networkTail[networkTail$reachID==whichReach,"depth"]))

##
#Run model with base parameters and each of the network configurations

outNetwork1 <- solveNetwork(network1)
#Proportion of input C that is respired in network
outNetwork1$networkSummary$CLost
#Mass of C that is respired in the network
outNetwork1$networkSummary$CIn - outNetwork1$networkSummary$COut

outNetworkHead <- solveNetwork(networkHead)
#Proportion of input C that is respired in network
outNetworkHead$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkHead$networkSummary$CIn - outNetworkHead$networkSummary$COut

outNetworkTail <- solveNetwork(networkTail)
#Proportion of input C that is respired in network
outNetworkTail$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkTail$networkSummary$CIn - outNetworkTail$networkSummary$COut

##
#Run model with higher carbon concentration of inputs localCin (double the default - localCin goes from 10 to 20)

network1HiC <- network1
network1HiC$localCin <- 20
outNetwork1HiC <- solveNetwork(network1HiC)
#Proportion of input C that is respired in network
outNetwork1HiC$networkSummary$CLost
#Mass of C that is respired in the network
outNetwork1HiC$networkSummary$CIn - outNetwork1HiC$networkSummary$COut

networkHeadHiC <- networkHead
networkHeadHiC$localCin <- 20
outNetworkHeadHiC <- solveNetwork(networkHeadHiC)
#Proportion of input C that is respired in network
outNetworkHeadHiC$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkHeadHiC$networkSummary$CIn - outNetworkHeadHiC$networkSummary$COut

networkTailHiC <- networkTail
networkTailHiC$localCin <- 20
outNetworkTailHiC <- solveNetwork(networkTailHiC)
#Proportion of input C that is respired in network
outNetworkTailHiC$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkTailHiC$networkSummary$CIn - outNetworkTailHiC$networkSummary$COut

#Run model with higher water yield (precip - evap) localPET (double the default - localPET goes from 0.005 to 0.010)

network1HiP <- network1
network1HiP$localPET <- 0.010
outNetwork1HiP <- solveNetwork(network1HiP)
#Proportion of input C that is respired in network
outNetwork1HiP$networkSummary$CLost
#Mass of C that is respired in the network
outNetwork1HiP$networkSummary$CIn - outNetwork1HiP$networkSummary$COut

networkHeadHiP <- networkHead
networkHeadHiP$localPET <- 0.010
outNetworkHeadHiP <- solveNetwork(networkHeadHiP)
#Proportion of input C that is respired in network
outNetworkHeadHiP$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkHeadHiP$networkSummary$CIn - outNetworkHeadHiP$networkSummary$COut

networkTailHiP <- networkTail
networkTailHiP$localPET <- 0.010
outNetworkTailHiP <- solveNetwork(networkTailHiP)
#Proportion of input C that is respired in network
outNetworkTailHiP$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkTailHiP$networkSummary$CIn - outNetworkTailHiP$networkSummary$COut


#Summary of results so far
#For network1
print(paste(outNetwork1$networkSummary$CLost, outNetwork1HiC$networkSummary$CLost, outNetwork1HiP$networkSummary$CLost))
#For networkHead
print(paste(outNetworkHead$networkSummary$CLost, outNetworkHeadHiC$networkSummary$CLost, outNetworkHeadHiP$networkSummary$CLost))
#For networkTail
print(paste(outNetworkTail$networkSummary$CLost, outNetworkTailHiC$networkSummary$CLost, outNetworkTailHiP$networkSummary$CLost))

##
# #Looking into why the proportion removed is so low for networkTailHiP

# head(outNetworkTailHiP$reachSummary)
# #The Inf residence time is weird
# head(network1)
# length(which(network1$localArea==0))
# table(network1$order[network1$localArea==0])
# #How many Inf residence times are there?
# length(which(is.infinite(outNetworkTailHiP$reachSummary$resTimeReach)))
# outNetworkTailHiP$reachSummary[is.infinite(outNetworkTailHiP$reachSummary$resTimeReach),]
# which(is.infinite(outNetworkTailHiP$reachSummary$resTimeReach))
# #Line below shows that all of the reaches with localArea=0 are first order
# networkTailHiP[which(is.infinite(outNetworkTailHiP$reachSummary$resTimeReach)),]
# #Go back to top of code and insert fix

# head(outNetworkTailHiP$reachSummary)
# 
# #Plot the reach-level residence times in the base Savannah River scenario and the tailwater lake scenario
# plot(outNetworkTailHiP$reachSummary$resTimeReach~outNetwork1HiP$reachSummary$resTimeReach)
# abline(0,1)
# plot(outNetworkTail$reachSummary$resTimeReach~outNetwork1$reachSummary$resTimeReach)
# abline(0,1)


#Run model with higher carbon concentration and higher water yield

network1HiCP <- network1
network1HiCP$localCin <- 20
network1HiCP$localPET <- 0.010
outNetwork1HiCP <- solveNetwork(network1HiCP)
#Proportion of input C that is respired in network
outNetwork1HiCP$networkSummary$CLost
#Mass of C that is respired in the network
outNetwork1HiCP$networkSummary$CIn - outNetwork1HiCP$networkSummary$COut

networkHeadHiCP <- networkHead
networkHeadHiCP$localCin <- 20
networkHeadHiCP$localPET <- 0.010
outNetworkHeadHiCP <- solveNetwork(networkHeadHiCP)
#Proportion of input C that is respired in network
outNetworkHeadHiCP$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkHeadHiCP$networkSummary$CIn - outNetworkHeadHiCP$networkSummary$COut

networkTailHiCP <- networkTail
networkTailHiCP$localCin <- 20
networkTailHiCP$localPET <- 0.010
outNetworkTailHiCP <- solveNetwork(networkTailHiCP)
#Proportion of input C that is respired in network
outNetworkTailHiCP$networkSummary$CLost
#Mass of C that is respired in the network
outNetworkTailHiCP$networkSummary$CIn - outNetworkTailHiCP$networkSummary$COut

#Summary of results
#For network1
print(paste(outNetwork1$networkSummary$CLost, outNetwork1HiC$networkSummary$CLost, outNetwork1HiP$networkSummary$CLost, outNetwork1HiCP$networkSummary$CLost))
#For networkHead
print(paste(outNetworkHead$networkSummary$CLost, outNetworkHeadHiC$networkSummary$CLost, outNetworkHeadHiP$networkSummary$CLost, outNetworkHeadHiCP$networkSummary$CLost))
#For networkTail
print(paste(outNetworkTail$networkSummary$CLost, outNetworkTailHiC$networkSummary$CLost, outNetworkTailHiP$networkSummary$CLost, outNetworkTailHiCP$networkSummary$CLost))
