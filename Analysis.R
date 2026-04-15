names(data)
# Load data
data <- read.csv("Data SC/pet_adoption_data.csv")

# Load libraries
library(dplyr)
library(ggplot2)

# Look at structure
str(data)

# Summary statistics
summary(data)

#  Adoption likelihood by pet type
data %>%
  group_by(PetType) %>%
  summarise(avg_adoption = mean(AdoptionLikelihood))

# plot
ggplot(data, aes(x = AgeMonths, y = AdoptionLikelihood)) +
  geom_point() +
  geom_smooth()
