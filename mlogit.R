library(ggeffects)
library(broom)

# Create a data frame with the response variable
category <-  c("A", "B", "C")
frequency <- c(140, 30, 44)
data <- data.frame(category, frequency)

# Fit multinomial logit model
fit <- glm(frequency ~ category, family = poisson(link = "log"), data = data)

# Display model summary
summary(fit)


tidy(fit, exponentiate = FALSE, conf.int = TRUE) # log odds ratio

tidy(fit, exponentiate = TRUE, conf.int = TRUE)  # raw odds ratio

# Visualization
plot(ggpredict(fit))