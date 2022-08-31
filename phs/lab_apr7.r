setwd("~/phs")

install.packages('RSiena')
install.packages('statnet') 
install.packages('coda')
install.packages('igraph')
install.packages('intergraph')
library(RSiena)
library(statnet)
library(coda)
library(igraph)
library(intergraph)



friend.data.w1 <- as.matrix(read.table("class5809.txt"))
friend.data.w2 <- as.matrix(read.table("class5809_t2.txt"))
attr5809<-read.table("attr5809.txt",header=TRUE,stringsAsFactors=FALSE)

# Set the wave 2 friendship data to missing if the wave 2 data are missing

for (i in 1:nrow(attr5809)) {if (attr5809[i,50] > 98) {friend.data.w2[i,]=9}}
friend.data.w1[friend.data.w1 %in% c(6,9)] <- NA
friend.data.w2[friend.data.w2 %in% c(6,9)] <- NA

# Create the wave 1 and wave 2 alcohol frequency matrix

alcfrq<-cbind(attr5809[,45],attr5809[,48])
alcfrq[alcfrq %in% c(4,5,6)] <- 4
alcfrq[alcfrq %in% c(99)] <- NA

# Check dimensions to verify that adjacency and attribute matrices match

dim(friend.data.w1)
dim(alcfrq)

# Create the sienaNet object

friendties <- sienaNet(array(c(friend.data.w1, friend.data.w2),
                             dim=c(nrow(attr5809), nrow(attr5809), 2)))
alcdrinkbeh <- sienaNet(alcfrq, type="behavior")

# Name the attribute variables to be included in the models

ah_pvt <- coCovar(attr5809[,36])
age <- coCovar(attr5809[,38])
male <- coCovar(attr5809[,40])
parentdrkfrq <- coCovar(attr5809[,53])
parentdrkfiv <- coCovar(attr5809[,54])
tobacco <- coCovar(attr5809[,56])



# Create the siena data object for running RSiena

data5809 <- sienaDataCreate(friendties,ah_pvt,age,male,parentdrkfrq,
                            parentdrkfiv,tobacco,alcdrinkbeh)
eff5809 <- getEffects(data5809)

# Export statistics about the data objects

print01Report(data5809,modelname = 'class5809_init')



# Examine null model effects to be included

eff5809

# Run the null model 

model5809 <- sienaModelCreate(useStdInits = FALSE, 
                              projname = 'class5809_run1')
ans5809 <- siena07(model5809, data=data5809, effects=eff5809, 
                   batch=FALSE, verbose=FALSE)
ans5809
summary(ans5809)




# Examine possible effects to include in the model

eff5809$effectName

# Include network co-evolution variables

dataset.5809 <- sienaDataCreate(friendties,ah_pvt,age,male,parentdrkfrq,tobacco,alcdrinkbeh)
effects.5809 <- getEffects(dataset.5809)
effects.5809 <- includeEffects(effects.5809,transTrip,type="eval")
effects.5809 <- includeEffects(effects.5809,cycle3,type="eval")
effects.5809 <- 
  includeEffects(effects.5809,simX,type="eval",interaction1="age") #age similarity
effects.5809 <- 
  includeEffects(effects.5809,sameX,type="eval",interaction1="male") #same gender
effects.5809 <- includeEffects(effects.5809,simX,type="eval",interaction1="ah_pvt") #similar scholastic aptitude
effects.5809 <- includeEffects(effects.5809,simX,type="eval",interaction1="alcdrinkbeh") #similar alcoholic use
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",maxAlt,interaction1="friendties") #maximum exposure
# effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",avRecAlt,interaction1="friendties") #average exposure
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="age") #age effect
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="parentdrkfrq") #parent drinking effect
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="tobacco") #tobacco effect
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="ah_pvt") #ah_pvt effect

# Run the full model

model5809 <- sienaModelCreate(useStdInits = FALSE, 
                              projname = 'class5809_run2')
ans5809 <- siena07(model5809, data=dataset.5809, effects=effects.5809, 
                   batch=FALSE, verbose=FALSE)
ans5809
summary(ans5809)



