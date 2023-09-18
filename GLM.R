library(data.table)
library(car)
library(multcomp)
library(ggeffects)


#Single-Rare-Event Condition
get_count <- function(target, age_group, category_response) {
  nrow(dat1[dat1$second_target == target &
              dat1$age == age_group &
              dat1$category == category_response])
}

create_row <- function(target, age_group) {
  sapply(1:4, function(cat) get_count(target, age_group, cat))
}

all_targets <- c(4, 5, 6, 7, 8)
all_ages <- c("children", "adults")

result <- do.call(rbind, lapply(all_targets, function(t) {
  do.call(rbind, lapply(all_ages, function(a) {
    c <- c(create_row(t, a))
    cbind(c[1],c[2],c[3],c[4], t, a)
  }))
}))



d <- data.frame(recent = c(sum(as.integer(result[seq(12, 20, 2)])), sum(as.integer(result[seq(11, 19, 2)]))),
                total = c(sum(as.integer(result[seq(2, 40, 2)])),sum(as.integer(result[seq(1, 39, 2)]))),
                group = c("adults", "children"))


d$group <- as.factor(d$group)
l <- glm(cbind(recent, total - recent) ~ group, data = d, family = binomial(link = logit))
summary(l)
Anova(l)
summary(glht(l, linfct = mcp(group = "Tukey")))
