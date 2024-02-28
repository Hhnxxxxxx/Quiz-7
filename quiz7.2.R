# Set seed for reproducibility
set.seed(123)

# Simulate data
n <- 1000  # Number of buildings to simulate
year_of_construction <- sample(1850:2023, n, replace = TRUE)  # Year of construction
building_use <- sample(1:3, n, replace = TRUE)  # Building use: 1 for residential, 2 for commercial, 3 for office
location <- sample(1:3, n, replace = TRUE)  # Location: 1 for CBD, 2 for suburban, 3 for historical

# Modify the simulation of the number of floors to ensure it's always positive
number_of_floors <- round(
  3 + 
    (2023 - year_of_construction) / 100 * (-0.5) +  # Adjusted impact of age, less reduction
    building_use * 0.5 +  # Variation by use
    location * (-0.25) +  # Reduced impact of location
    rnorm(n, mean = 0, sd = 2)  # Adding some random noise
)

# Ensure all buildings have at least 1 floor
number_of_floors <- pmax(number_of_floors, 1)

# Create dataframe
buildings_df <- tibble(year_of_construction, building_use, location, number_of_floors)

# View the first few rows of the dataframe
head(buildings_df)

library(testthat)
library(dplyr)

# Assuming buildings_df is your dataframe
# Here are examples of tests based on the specifications:

# 1. Test if the dataset is not empty
test_that("Dataset is not empty", {
  expect_false(nrow(buildings_df) == 0)
})

# 2. Test if the dataset has the expected number of columns
test_that("Dataset has the correct number of columns", {
  expect_equal(ncol(buildings_df), 4)
})

# 3. Test if the column names are correctly named
test_that("Column names are correct", {
  expect_equal(colnames(buildings_df), c("year_of_construction", "building_use", "location", "number_of_floors"))
})

# 4. Test if the year of construction is within the expected range
test_that("Year of construction is within the expected range", {
  expect_true(all(buildings_df$year_of_construction >= 1850 & buildings_df$year_of_construction <= 2023))
})

# 5. Test if the building use column only contains values 1, 2, or 3
test_that("Building use contains correct values", {
  expect_true(all(buildings_df$building_use %in% 1:3))
})

# 6. Test if the location column only contains values 1, 2, or 3
test_that("Location contains correct values", {
  expect_true(all(buildings_df$location %in% 1:3))
})

# 7. Test if the number of floors is a positive integer
test_that("Number of floors is positive", {
  expect_true(all(buildings_df$number_of_floors > 0))
})

# 8. Test if there are no missing values in any column
test_that("No missing values in the dataset", {
  expect_true(sum(is.na(buildings_df)) == 0)
})

# 9. Test if the data type of each column is as expected
test_that("Data types are as expected", {
  expect_true(is.numeric(buildings_df$year_of_construction) & 
                is.numeric(buildings_df$building_use) & 
                is.numeric(buildings_df$location) & 
                is.numeric(buildings_df$number_of_floors))
})

# 10. Test if the number of floors varies by building use as expected
test_that("Number of floors varies by building use", {
  variances <- buildings_df %>% group_by(building_use) %>% summarise(variance = var(number_of_floors))
  expect_true(variances$variance[2] > variances$variance[1] && variances$variance[3] > variances$variance[1])
})

# Run the tests
test_dir("path/to/your/tests")

