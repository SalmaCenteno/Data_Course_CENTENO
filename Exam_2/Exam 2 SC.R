library(tidyverse)
unicef_u5mr <- read_csv("unicef-u5mr.csv")


unicef_tidy <- unicef_u5mr %>%
  pivot_longer(
    cols = starts_with("U5MR"),
    names_to = "Year",
    values_to = "U5MR"
  ) %>%
  mutate(
    Year = as.numeric(gsub("U5MR.", "", Year))
  ) %>%
  drop_na(U5MR)

plot1 <- ggplot(unicef_tidy, aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~ Continent) +
  labs(
    title = "U5MR Over Time by Country",
    x = "Year",
    y = "U5MR"
  ) +
  theme_minimal()

ggsave("Centeno_Plot_1.png", plot1, width = 10, height = 6)

continent_mean <- unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarise(mean_U5MR = mean(U5MR), .groups = "drop")

plot2 <- ggplot(continent_mean, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(size = 1) +
  labs(
    title = "Mean U5MR by Continent",
    x = "Year",
    y = "Mean U5MR"
  ) +
  theme_minimal()

ggsave("Centeno_Plot_2.png", plot2, width = 10, height = 6)

mod1 <- lm(U5MR ~ Year, data = unicef_tidy)

mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)

mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)

AIC(mod1, mod2, mod3)

#Because mod3 incorporates the relationship between Year and Continent, it is the best model.
#It typically has the best model fit, as indicated by the lowest AIC.

unicef_tidy$pred1 <- predict(mod1)
unicef_tidy$pred2 <- predict(mod2)
unicef_tidy$pred3 <- predict(mod3)

ggplot(unicef_tidy, aes(x = Year, y = U5MR)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(y = pred1), color = "red") +
  geom_line(aes(y = pred2), color = "blue") +
  geom_line(aes(y = pred3), color = "green") +
  facet_wrap(~ Continent) +
  labs(title = "Model Predictions vs Actual Data") +
  theme_minimal()

ecuador_data <- data.frame(
  Year = 2020,
  Continent = "Americas"
)

pred <- predict(mod3, newdata = ecuador_data)

real <- 13
difference <- abs(pred - real)

pred
difference

mod4 <- lm(log(U5MR) ~ Year * Continent, data = unicef_tidy)

pred4 <- exp(predict(mod4, newdata = ecuador_data))

abs(pred4 - 13)
