setwd("~/phs")

data <- read.csv("heart_data.csv")
print(data)

heart.disease.lm<-lm(heart.disease ~ biking+smoking, data = data)
summary(heart.disease.lm)