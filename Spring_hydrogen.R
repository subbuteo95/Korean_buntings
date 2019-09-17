# Migration pattern of Yellow-throated Buntings revealed by isotope-based geographic assignment

# Spring data (hydrogen only & no site): glm & stepwise regression
# Sex (Female: 0, Male: 1)
library(MuMIn)
spring <- read.csv ("C:/spring.csv")
head(spring)
options(na.action = "na.fail")
springout <- glm(h ~ date + sex + date*sex, data=spring)
summary(springout)
reduced.model=step(springout)
summary(reduced.model)

