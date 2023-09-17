library(tidyverse)
library(data.table)
library(car)
library(ggeffects)

setwd("C:/Users/Reiki/OneDrive")
getwd()

dat <- fread("C:/Users/reiki/OneDrive/data_0416.csv", header=TRUE)

dat <- fread("datafile", header=TRUE)


#omit incorrect trials in a recognition task
dat <- subset(dat,
              (dat$inserted_position == "#9" & dat$retention_task == 2) |
                (dat$inserted_position == "#6" & dat$retention_task == 1))



#Analysis of Experiment 1

dat1 <- dat[dat$first_target==0,] #select trials of Exp. 1

dat1 <- mutate(dat1, category = 3) #other strategies


dat1$category[rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                dat1$"c9" == 1] <-2 # selecting a recent event


dat1$category[(dat1$second_target ==4 &
                 rowSums(dat1[, paste0("c", 1:9)]) ==1 &
                 dat1$"c4" ==1) |
                (dat1$second_target ==5 &
                   rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                   dat1$"c5" ==1) |
                (dat1$second_target ==6 &
                   rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                   dat1$"c6" == 1) |
                (dat1$second_target ==7 &
                   rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                   dat1$"c7" == 1) |
                (dat1$second_target ==8 &
                   rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                   dat1$"c8" == 1)|
                (dat1$second_target ==9 &
                   rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                   dat1$"c9" == 1)] <- 1 #selecting a rare event


dat1 <- mutate(dat1, cat = if_else(category == 1, 1, 0))

dat1 <- mutate(dat1, pos_contrast = ifelse(second_target == 7, -1, ifelse(second_target == 8, 0, 1)))
dat1 <- mutate(dat1, pos_contrast_step = ifelse(second_target == 7, -0.5, ifelse(second_target == 8,-0.5,1)))

l1 <- glm(cat ~ pos_contrast, data = subset(dat1,dat1$age =="adults" & (dat1$second_target ==7 | dat1$second_target ==8 | dat1$second_target ==9)), family = binomial)
l2 <- glm(cat ~ pos_contrast_step, data = subset(dat1,dat1$age =="adults" & (dat1$second_target ==7 | dat1$second_target ==8 | dat1$second_target ==9)), family = binomial)

summary(l1)#linear model
summary(l2)#step model

plot(ggpredict(l1), rawdata = T)
plot(ggpredict(l2), rawdata = T)


#Analysis of Experiment 2

dat2 <- dat[dat$inserted_position =="#6",] #select trials of Exp. 2

dat2 <- mutate(dat2, category = 3)#other strategies

dat2$category[(rowSums(dat2[, paste0("c", 1:9)]) == 1 &
                 ((dat2$first_target == 4 &
                     dat2$"c4" == 1) |
                    (dat2$first_target == 5 &
                       dat2$"c5" == 1) |
                    (dat2$first_target == 6 &
                       dat2$"c6" == 1)))] <- 1 #selecting a rare event

dat2$category[rowSums(dat2[, paste0("c", 1:9)]) == 1 &
                dat2$"c6" == 1] <- 2 #selecting a recent event

dat2$category[dat2$first_target == 6 & dat2$category == 2] <- 1 #replace "selecting a recent event" from "selecting a rare event" in #9 condition

dat2 <- dat2[dat2$second_target == 7,]

dat2 <- mutate(dat2, cat = if_else(category == 1, 1, 0))

dat2 <- mutate(dat2, pos_contrast = ifelse(first_target == 4, -1, ifelse(first_target == 5, 0, 1)))
dat2 <- mutate(dat2, pos_contrast_step = ifelse(first_target == 4, -0.5, ifelse(first_target == 5,-0.5,1)))

l1 <- glm(cat ~ pos_contrast, data = subset(dat2,dat2$age =="children"), family = binomial)
l2 <- glm(cat ~ pos_contrast_step, data = subset(dat2,dat2$age =="children"), family = binomial)

summary(l1)#linear model
summary(l2)#step model

plot(ggpredict(l1), rawdata = T)
plot(ggpredict(l2), rawdata = T)



#Analysis of Experiment 3

dat3 <- dat[dat$inserted_position =="#9",]

dat3 <- mutate(dat3, category = 4)#other strategies


inserted_cond <- dat3$inserted_position == "#9"
row_sum_cond <- rowSums(dat3[, paste0("c", 1:9)]) == 1
second_targets <- dat3$second_target %in% 7:9
first_target_not_zero <- dat3$first_target != 0

dat3$category[inserted_cond & row_sum_cond] <- ifelse(
  inserted_cond & row_sum_cond & ((second_targets & dat3[, paste0("c", dat3$second_target)] == 1) |
                                    (first_target_not_zero & dat3$second_target == 9 & dat3$c9 == 1)),
  1,
  dat3x$category)#selecting a rare event


targets <- expand.grid(first = 4:6, second = 7:9)
row_sum_cond <- rowSums(dat1[, paste0("c", 1:9)]) == 2
inserted_cond <- dat3$inserted_position == "#9"

dat3$category[inserted_cond & row_sum_cond] <- sapply(1:nrow(dat3), function(i) {
  if (!inserted_cond[i] || !row_sum_cond[i]) return(dat3$category[i])
  
  for (j in 1:nrow(targets)) {
    first_target_col <- paste0("c", targets$first[j])
    second_target_col <- paste0("c", targets$second[j])
    
    if (dat3$first_target[i] == targets$first[j] &&
        dat3$second_target[i] == targets$second[j] &&
        dat3[[first_target_col]][i] == 1 &&
        dat3[[second_target_col]][i] == 1) {
      return(2)
    }
  }
  
  return(dat3$category[i])
})#selecting a rare event and distractor



dat3$category[dat3$inserted_position == "#9" &
                dat3$second_target != 9 &
                rowSums(dat1[, paste0("c", 1:9)]) == 1 &
                dat1$"c9" == 1] <-3 #selecting a recent event

dat3 <- mutate(dat3, cat = if_else(category == 1, 1, 0))

dat3 <- dat1[dat1$first_target == 4,]#example

dat3 <- mutate(dat3, pos_contrast = ifelse(first_target == 7, -1, ifelse(second_target == 8, 0, 1)))
dat3 <- mutate(dat3, pos_contrast_step = ifelse(first_target == 7, -0.5, ifelse(second_target == 8,-0.5,1)))

l1 <- glm(cat ~ pos_contrast, data = subset(dat1,dat1$age =="children"), family = binomial)
l2 <- glm(cat ~ pos_contrast_step, data = subset(dat1,dat1$age =="children"), family = binomial)

summary(l1)#linear model
summary(l2)#step model

plot(ggpredict(l1), rawdata = T)
plot(ggpredict(l2), rawdata = T)