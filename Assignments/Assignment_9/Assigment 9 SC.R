data <- read.csv("GradSchool_Admissions.csv")
head(data)

str(data)
summary(data)

data$rank <- as.factor(data$rank)

table(data$admit)

boxplot(gre ~ admit, data = data,
        main = "GRE vs Admission",
        xlab = "Admission (0 = No, 1 = Yes)",
        ylab = "GRE Score")

boxplot(gpa ~ admit, data = data,
        main = "GPA vs Admission",
        xlab = "Admission",
        ylab = "GPA")

table(data$rank, data$admit)

model1 <- glm(admit ~ gre + gpa + rank,
              data = data,
              family = binomial)

summary(model1)


model2 <- glm(admit ~ gpa + rank,
              data = data,
              family = binomial)

model3 <- glm(admit ~ rank,
              data = data,
              family = binomial)

AIC(model1, model2, model3)

data$predicted <- predict(model1, type = "response")
head(data)

exp(coef(model1))

anova(model3, model2, model1, test = "Chisq")