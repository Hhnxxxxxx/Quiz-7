library(ggplot2)

# Create a scatter plot of number of floors vs. year of construction
ggplot(buildings_df, aes(x = year_of_construction, y = number_of_floors)) +
  geom_point(alpha = 0.5) +  # Use semi-transparent points to handle overplotting
  theme_minimal() +  # Use a minimal theme for a clean look
  labs(x = "Year of Construction", y = "Number of Floors", title = "Building Floors vs. Year of Construction")

# If you haven't installed rstanarm, uncomment and run the following line:
# install.packages("rstanarm")

library(rstanarm)

# Fit a Bayesian linear regression model
model <- stan_glm(number_of_floors ~ year_of_construction, data = buildings_df, family = gaussian(), chains = 2, iter = 2000)

# Print the summary of the model
print(model)

# To view a summary of the posterior distribution of the parameters
summary(model)
