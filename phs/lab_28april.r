setwd("~/phs")
install.packages("igraph")
library(igraph)


friend.data.w1 <- as.matrix(read.table("class126.txt"))
friend.data.w2 <- as.matrix(read.table("class126_t2.txt"))
attr126<-read.table("attr126.txt",header=TRUE,stringsAsFactors=FALSE)

# Set the wave 2 friendship data to missing if the wave 2 data are missing
for (i in 1:nrow(attr126)) {if (attr126[i,50] > 98) {friend.data.w2[i,]=9}}
friend.data.w1[friend.data.w1 %in% c(6,9)] <- NA
friend.data.w2[friend.data.w2 %in% c(6,9)] <- NA

# Create the wave 1 and wave 2 alcohol frequency matrix
alcinit<-cbind(attr126[,60],attr126[,51])
alcinit[alcinit %in% c(99)] <- NA

# Check dimensions to verify that adjacency and attribute matrices match
dim(friend.data.w1)
dim(alcinit)

# Plot the wave 1 connections
g1<-graph_from_adjacency_matrix(friend.data.w1)
plot(g1)




install.packages("netdiffuseR")
library(netdiffuseR)

# create the ‘time of adoption’ toa vector 

# attr126[,60] indicates student alcohol drinking status at wave 1
attr126[,60]

# attr126[,51] is an indicator of drinking at wave 2
attr126[,51]

# The following code creates a vector that is valued 1 if student has started
# drinking prior to wave 1, valued 2 if student was nondrinker at wave 1 and
# drinker at wave 2, and valued NA if the student was a nondrinker
# throughout. Students lost to follow-up at wave 2 had alcohol use value 99
# at wave 2. The statements below recode toa to 1 if such a student recorded
# drinking at wave 1 and NA otherwise.

toa <- 2*attr126[,51] - attr126[,60]
toa[toa %in% c(197)] <- 1
toa[toa %in% c(198)] <- 0
toa[toa %in% c(0)] <- NA
toa

ah_pvt <- array(attr126[,36])
age <- attr126[,38]
male <- attr126[,40]
parentdrkfrq <- attr126[,53]
tobacco <- attr126[,56]

diffnet <- as_diffnet(friend.data.w1, toa)
diffnet

X   <- cbind(ah_pvt = ah_pvt, age=age, male=male, 
             parentdrkfrq=parentdrkfrq, tobacco=tobacco)

dn  <- new_diffnet(friend.data.w1, toa=toa, vertex.static.attrs = X)
dn[["cohesive_expo"]] <- cbind(NA, exposure(dn)[,-nslices(dn)])
dn[["adopt"]]         <- dn$cumadopt

dn


summary(diffnet)
plot_diffnet(diffnet)

# Generating the data and running the null model
dat <- as.data.frame(dn)
ans <- glm(adopt ~ cohesive_expo,
           data = dat,
           family = binomial(link="probit"),
           subset = is.na(toa) | (per <= toa))
summary(ans)

# Generating the data and running the full model
dat <- as.data.frame(dn)
ans <- glm(adopt ~ cohesive_expo + ah_pvt + male + age +parentdrkfrq + tobacco,
           data = dat,
           family = binomial(link="probit"),
           subset = is.na(toa) | (per <= toa))
summary(ans)

