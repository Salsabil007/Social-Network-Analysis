setwd("~/phs")

install.packages('RSiena')
install.packages('statnet') 
library(RSiena)
library(statnet)




friend.data.w1 <- as.matrix(read.table("class7.txt"))
friend.data.w2 <- as.matrix(read.table("class7_t2.txt"))
attr7<-read.table("attr7.txt",header=TRUE,stringsAsFactors=FALSE)
for (i in 1:nrow(attr7)) {if (attr7[i,48] > 98) {friend.data.w2[i,]=9}}
friend.data.w1[friend.data.w1 %in% c(6,9)] <- NA
friend.data.w2[friend.data.w2 %in% c(6,9)] <- NA
alcdrinkbeh<-cbind(attr7[,45],attr7[,48])
alcdrinkbeh[alcdrinkbeh %in% c(99)] <- NA
friendties <- sienaNet(array(c(friend.data.w1,friend.data.w2),dim=c(nrow(attr7),nrow(attr7),2)))
alcdrinkbeh <- sienaNet(alcdrinkbeh,type="behavior")
ah_pvt <- coCovar(attr7[,36])
age <- coCovar(attr7[,38])
male <- coCovar(attr7[,40])
parentdrkfrq <- coCovar(attr7[,53])
tobacco <- coCovar(attr7[,56])
dataset.7 <- sienaDataCreate(friendties,ah_pvt,age,male,parentdrkfrq,tobacco,alcdrinkbeh)
effects.7 <- getEffects(dataset.7)
effectsDocumentation(effects.7)

effects.7 <- includeEffects(effects.7,transTrip,type="eval")
effects.7 <- includeEffects(effects.7,cycle3,type="eval")
effects.7 <- includeEffects(effects.7,simX,type="eval",interaction1="age") #age similarity
effects.7 <- includeEffects(effects.7,sameX,type="eval",interaction1="male") #same gender
effects.7 <- includeEffects(effects.7,simX,type="eval",interaction1="ah_pvt") #similar scholastic aptitude
effects.7 <- includeEffects(effects.7,simX,type="eval",interaction1="alcdrinkbeh") #similar alcoholic use
effects.7 <- includeEffects(effects.7,name="alcdrinkbeh",maxAlt,interaction1="friendties") #average exposure
# effects.7 <- includeEffects(effects.7,name="alcdrinkbeh",avRecAlt,interaction1="friendties") #average exposure
effects.7 <- includeEffects(effects.7,name="alcdrinkbeh",effFrom,type="eval",interaction1="age") #age effect
effects.7 <- includeEffects(effects.7,name="alcdrinkbeh",effFrom,type="eval",interaction1="parentdrkfrq") #parent drinking effect
effects.7 <- includeEffects(effects.7,name="alcdrinkbeh",effFrom,type="eval",interaction1="tobacco") #tobacco effect
effects.7 <- includeEffects(effects.7,name="alcdrinkbeh",effFrom,type="eval",interaction1="ah_pvt") #ah_pvt effect

modelall <- sienaModelCreate(useStdInits = FALSE, projname = 'AlcoholBehavior')

ans.7 <- siena07(modelall, data=dataset.7, effects=effects.7, batch=FALSE)
ans.7

friend.data.w1 <- as.matrix(read.table("class5809.txt"))
friend.data.w2 <- as.matrix(read.table("class5809_t2.txt"))
attr5809<-read.table("attr5809.txt",header=TRUE,stringsAsFactors=FALSE)
for (i in 1:nrow(attr5809)) {if (attr5809[i,48] > 98) {friend.data.w2[i,]=9}}
friend.data.w1[friend.data.w1 %in% c(6,9)] <- NA
friend.data.w2[friend.data.w2 %in% c(6,9)] <- NA
alcdrinkbeh<-cbind(attr5809[,45],attr5809[,48])
alcdrinkbeh[alcdrinkbeh %in% c(99)] <- NA
friendties <- sienaNet(array(c(friend.data.w1,friend.data.w2),dim=c(nrow(attr5809),nrow(attr5809),2)))
alcdrinkbeh <- sienaNet(alcdrinkbeh,type="behavior")
ah_pvt <- coCovar(attr5809[,36])
age <- coCovar(attr5809[,38])
male <- coCovar(attr5809[,40])
parentdrkfrq <- coCovar(attr5809[,53])
tobacco <- coCovar(attr5809[,56])
dataset.5809 <- sienaDataCreate(friendties,ah_pvt,age,male,parentdrkfrq,tobacco,alcdrinkbeh)
effects.5809 <- getEffects(dataset.5809)

effects.5809 <- includeEffects(effects.5809,transTrip,type="eval")
effects.5809 <- includeEffects(effects.5809,cycle3,type="eval")
effects.5809 <- includeEffects(effects.5809,simX,type="eval",interaction1="age") #age similarity
effects.5809 <- includeEffects(effects.5809,sameX,type="eval",interaction1="male") #same gender
effects.5809 <- includeEffects(effects.5809,simX,type="eval",interaction1="ah_pvt") #similar scholastic aptitude
effects.5809 <- includeEffects(effects.5809,simX,type="eval",interaction1="alcdrinkbeh") #similar alcoholic use
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",maxAlt,interaction1="friendties") #average exposure
# effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",avRecAlt,interaction1="friendties") #average exposure
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="age") #age effect
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="parentdrkfrq") #parent drinking effect
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="tobacco") #tobacco effect
effects.5809 <- includeEffects(effects.5809,name="alcdrinkbeh",effFrom,type="eval",interaction1="ah_pvt") #ah_pvt effect

modelall <- sienaModelCreate(useStdInits = FALSE, projname = 'AlcoholBehavior')

ans.5809 <- siena07(modelall, data=dataset.5809, effects=effects.5809, batch=FALSE)
ans.5809

friend.data.w1 <- as.matrix(read.table("class8.txt"))
friend.data.w2 <- as.matrix(read.table("class8_t2.txt"))
attr8<-read.table("attr8.txt",header=TRUE,stringsAsFactors=FALSE)
for (i in 1:nrow(attr8)) {if (attr8[i,48] > 98) {friend.data.w2[i,]=9}}
friend.data.w1[friend.data.w1 %in% c(6,9)] <- NA
friend.data.w2[friend.data.w2 %in% c(6,9)] <- NA
alcdrinkbeh<-cbind(attr8[,45],attr8[,48])
alcdrinkbeh[alcdrinkbeh %in% c(99)] <- NA
friendties <- sienaNet(array(c(friend.data.w1,friend.data.w2),dim=c(nrow(attr8),nrow(attr8),2)))
alcdrinkbeh <- sienaNet(alcdrinkbeh,type="behavior")
ah_pvt <- coCovar(attr8[,36])
age <- coCovar(attr8[,38])
male <- coCovar(attr8[,40])
parentdrkfrq <- coCovar(attr8[,53])
tobacco <- coCovar(attr8[,56])
dataset.8 <- sienaDataCreate(friendties,ah_pvt,age,male,parentdrkfrq,tobacco,alcdrinkbeh)
effects.8 <- getEffects(dataset.8)

effects.8 <- includeEffects(effects.8,transTrip,type="eval")
effects.8 <- includeEffects(effects.8,cycle3,type="eval")
effects.8 <- includeEffects(effects.8,simX,type="eval",interaction1="age") #age similarity
effects.8 <- includeEffects(effects.8,sameX,type="eval",interaction1="male") #same gender
effects.8 <- includeEffects(effects.8,simX,type="eval",interaction1="ah_pvt") #similar scholastic aptitude
effects.8 <- includeEffects(effects.8,simX,type="eval",interaction1="alcdrinkbeh") #similar alcoholic use
effects.8 <- includeEffects(effects.8,name="alcdrinkbeh",maxAlt,interaction1="friendties") #average exposure
# effects.8 <- includeEffects(effects.8,name="alcdrinkbeh",avRecAlt,interaction1="friendties") #average exposure
effects.8 <- includeEffects(effects.8,name="alcdrinkbeh",effFrom,type="eval",interaction1="age") #age effect
effects.8 <- includeEffects(effects.8,name="alcdrinkbeh",effFrom,type="eval",interaction1="parentdrkfrq") #parent drinking effect
effects.8 <- includeEffects(effects.8,name="alcdrinkbeh",effFrom,type="eval",interaction1="tobacco") #tobacco effect
effects.8 <- includeEffects(effects.8,name="alcdrinkbeh",effFrom,type="eval",interaction1="ah_pvt") #ah_pvt effect

modelall <- sienaModelCreate(useStdInits = FALSE, projname = 'AlcoholBehavior')

ans.8 <- siena07(modelall, data=dataset.8, effects=effects.8, batch=FALSE)
ans.8

friend.data.w1 <- as.matrix(read.table("class88.txt"))
friend.data.w2 <- as.matrix(read.table("class88_t2.txt"))
attr88<-read.table("attr88.txt",header=TRUE,stringsAsFactors=FALSE)
for (i in 1:nrow(attr88)) {if (attr88[i,48] > 98) {friend.data.w2[i,]=9}}
friend.data.w1[friend.data.w1 %in% c(6,9)] <- NA
friend.data.w2[friend.data.w2 %in% c(6,9)] <- NA
alcdrinkbeh<-cbind(attr88[,45],attr88[,48])
alcdrinkbeh[alcdrinkbeh %in% c(99)] <- NA
friendties <- sienaNet(array(c(friend.data.w1,friend.data.w2),dim=c(nrow(attr88),nrow(attr88),2)))
alcdrinkbeh <- sienaNet(alcdrinkbeh,type="behavior")
ah_pvt <- coCovar(attr88[,36])
age <- coCovar(attr88[,38])
male <- coCovar(attr88[,40])
parentdrkfrq <- coCovar(attr88[,53])
tobacco <- coCovar(attr88[,56])
dataset.88 <- sienaDataCreate(friendties,ah_pvt,age,male,parentdrkfrq,tobacco,alcdrinkbeh)
effects.88 <- getEffects(dataset.88)

effects.88 <- includeEffects(effects.88,transTrip,type="eval")
effects.88 <- includeEffects(effects.88,cycle3,type="eval")
effects.88 <- includeEffects(effects.88,simX,type="eval",interaction1="age") #age similarity
effects.88 <- includeEffects(effects.88,sameX,type="eval",interaction1="male") #same gender
effects.88 <- includeEffects(effects.88,simX,type="eval",interaction1="ah_pvt") #similar scholastic aptitude
effects.88 <- includeEffects(effects.88,simX,type="eval",interaction1="alcdrinkbeh") #similar alcoholic use
effects.88 <- includeEffects(effects.88,name="alcdrinkbeh",maxAlt,interaction1="friendties") #average exposure
# effects.88 <- includeEffects(effects.88,name="alcdrinkbeh",avRecAlt,interaction1="friendties") #average exposure
effects.88 <- includeEffects(effects.88,name="alcdrinkbeh",effFrom,type="eval",interaction1="age") #age effect
effects.88 <- includeEffects(effects.88,name="alcdrinkbeh",effFrom,type="eval",interaction1="parentdrkfrq") #parent drinking effect
effects.88 <- includeEffects(effects.88,name="alcdrinkbeh",effFrom,type="eval",interaction1="tobacco") #tobacco effect
effects.88 <- includeEffects(effects.88,name="alcdrinkbeh",effFrom,type="eval",interaction1="ah_pvt") #ah_pvt effect

modelall <- sienaModelCreate(useStdInits = FALSE, projname = 'AlcoholBehavior')

ans.88 <- siena07(modelall, data=dataset.88, effects=effects.88, batch=FALSE)
ans.88

meta <- siena08(ans.5809,ans.88,ans.7,ans.8)
summary(meta) 

