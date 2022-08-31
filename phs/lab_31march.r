setwd("~/phs")
library(statnet)
library(network)
library(ergm)




F2FData <- read.table("PrimaryCareF2FNet.txt", header=TRUE, row.names=1, check.names=FALSE)
F2FMatrix <- as.matrix(F2FData)
StaffAttr<-read.table("PrimaryCareAttributes.txt",header=TRUE,
                      stringsAsFactors=FALSE)
F2FNet=network(F2FMatrix,matrix.type="adjacency",directed=TRUE)
F2FNet%v%'vertex.names'<- StaffAttr$SubjID
F2FNet%v%'Job_Category'<- StaffAttr$Job_Category
F2FNet%v%'Years_Clinic'<- StaffAttr$Years_Clinic
F2FNet%v%'FTE'<- StaffAttr$FTE
F2FNet%v%'Female'<- StaffAttr$Female
F2FNet%v%'Job_Satisfaction' <- StaffAttr$Job_Satisfaction

F2Fmodel.5 <- ergm(F2FNet ~edges + mutual + gwesp(0.25,fixed=TRUE) + ctriple + nodefactor("Job_Category") + nodematch("Job_Category",diff=TRUE)+ nodecov("Job_Satisfaction") + absdiff("Years_Clinic"), verbose=TRUE)
summary(F2Fmodel.5)





gof(F2Fmodel.5, GOF = ~ distance + espartners + idegree + odegree + triadcensus, verbose=TRUE, burnin=1000, interval=1000)






EHRData <- read.table("PrimaryCareEHRNet.txt", header=TRUE, row.names=1, check.names=FALSE)
EHRMatrix <- as.matrix(EHRData)
StaffAttr<-read.table("PrimaryCareAttributes.txt",header=TRUE,
                      stringsAsFactors=FALSE)
EHRNet=network(EHRMatrix,matrix.type="adjacency",directed=TRUE)
EHRNet%v%'vertex.names'<- StaffAttr$SubjID
EHRNet%v%'Job_Category'<- StaffAttr$Job_Category
EHRNet%v%'Years_Clinic'<- StaffAttr$Years_Clinic
EHRNet%v%'FTE'<- StaffAttr$FTE
EHRNet%v%'Female'<- StaffAttr$Female
EHRNet%v%'Job_Satisfaction' <- StaffAttr$Job_Satisfaction

gden(EHRNet)
gtrans(EHRNet)
centralization(EHRNet, betweenness)

EHRBetw <- (betweenness(EHRNet)/max(betweenness(EHRNet))*2)+0.5
Jobcat<- EHRNet%v%'Job_Category' 
gplot(EHRNet, vertex.cex= EHRBetw,vertex.col=Jobcat,usearrows=TRUE)
legend("bottomleft", legend = c("MD","RN","MA","Office","Lab"),fill = c(1:5), cex = 0.8)

table (Jobcat)
mixingmatrix (EHRNet, "Job_Category") 





EHRmodel.0 <- ergm(EHRNet ~edges, verbose=TRUE)
summary(EHRmodel.0)

EHRmodel.1 <- ergm(EHRNet ~edges + nodefactor("Job_Category"), verbose=TRUE)
summary(EHRmodel.1)






EHRmodel.2 <- ergm(EHRNet ~edges + nodefactor("Job_Category") + nodematch("Job_Category",diff=TRUE), verbose=TRUE)
summary(EHRmodel.2)






EHRmodel.3 <- ergm(EHRNet ~edges + nodefactor("Job_Category") + nodecov("Job_Satisfaction") + absdiff("Years_Clinic"), verbose=TRUE)
summary(EHRmodel.3)







EHRmodel.4 <- ergm(EHRNet ~edges + mutual + ctriple + esp(1)  + nodefactor("Job_Category") + nodecov("Job_Satisfaction") + absdiff("Years_Clinic"), verbose=TRUE)
summary(EHRmodel.4)







