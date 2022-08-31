setwd("~/phs")
library(statnet)
library(network)
library(ergm)
F2FData <- read.table("PrimaryCareF2FNet.txt", header=TRUE, row.names=1, check.names=FALSE)
F2FMatrix <- as.matrix(F2FData)
StaffAttr<-read.table("PrimaryCareAttributes.txt",header=TRUE, stringsAsFactors=FALSE)

F2FData

F2FMatrix

StaffAttr

F2FNet=network(F2FMatrix,matrix.type="adjacency",directed=TRUE)
F2FNet%v%'vertex.names'<- StaffAttr$SubjID
F2FNet%v%'Job_Category'<- StaffAttr$Job_Category
F2FNet%v%'Years_Clinic'<- StaffAttr$Years_Clinic
F2FNet%v%'FTE'<- StaffAttr$FTE
F2FNet%v%'Female'<- StaffAttr$Female
F2FNet%v%'Job_Satisfaction' <- StaffAttr$Job_Satisfaction

gden(F2FNet)
gtrans(F2FNet)
centralization(F2FNet, betweenness)

F2FBetw <- (betweenness(F2FNet)/max(betweenness(F2FNet))*2)+0.5
Jobcat<- F2FNet%v%'Job_Category' 
gplot(F2FNet, vertex.cex= F2FBetw,vertex.col=Jobcat,usearrows=TRUE)
legend("bottomleft", legend = c("MD","RN","MA","Office","Lab"),fill = c(1:5),
       cex = 0.8)

table (Jobcat)
mixingmatrix (F2FNet, "Job_Category") #find # of connection of each job category with others

F2Fmodel.0 <- ergm(F2FNet ~edges, verbose=TRUE) #null model, basically density indicator
summary(F2Fmodel.0)

F2Fmodel.1 <- ergm(F2FNet ~edges + nodefactor("Job_Category"), verbose=TRUE) ##using job_category as a predictor of whether a connection exists or not.
summary(F2Fmodel.1) ##takes category 1 as a comparison group and find how much activity others have compared to 1.

anova(F2Fmodel.0, F2Fmodel.1) ##we want to see if the new model have added significant new predictive power by adding the job_category info

F2Fmodel.2 <- ergm(F2FNet ~edges + nodefactor("Job_Category") + nodematch("Job_Category",diff=TRUE), verbose=TRUE)
summary(F2Fmodel.2)
anova(F2Fmodel.1, F2Fmodel.2)

F2Fmodel.3 <- ergm(F2FNet ~edges + nodefactor("Job_Category") + nodematch("Job_Category",diff=TRUE) + nodecov("Job_Satisfaction") + 
                     absdiff("Years_Clinic"), verbose=TRUE)
summary(F2Fmodel.3)
anova(F2Fmodel.2, F2Fmodel.3)

gof(F2Fmodel.3, GOF = ~ distance + espartners + idegree + odegree + triadcensus, verbose=TRUE, burnin=1000, interval=1000)


F2Fmodel.4 <- ergm(F2FNet ~edges + mutual + gwesp(0.25,fixed=TRUE) + nodefactor("Job_Category") + nodematch("Job_Category",diff=TRUE)+ nodecov("Job_Satisfaction") + absdiff("Years_Clinic"), verbose=TRUE)
summary(F2Fmodel.4)


gof(F2Fmodel.4, GOF = ~ distance + espartners + idegree + odegree + triadcensus, verbose=TRUE, burnin=1000, interval=1000)
