set.seed(42) # For reproducibility

# Number of observations
n <- 1000

# Simulating independent variables
race <- sample(c("Race1", "Race2", "Race3"), size=n, replace=TRUE)
gender <- sample(c("Male", "Female"), size=n, replace=TRUE)

# Simulating dependent variable with an imperfect relationship
vote_preference <- vector(length=n)

for (i in 1:n) {
  if (race[i] == "Race1" && gender[i] == "Male") {
    vote_preference[i] <- sample(c("Candidate A", "Candidate B"), size=1, prob=c(0.6, 0.4))
  } else if (race[i] == "Race1") {
    vote_preference[i] <- sample(c("Candidate A", "Candidate B"), size=1, prob=c(0.55, 0.45))
  } else if (gender[i] == "Male") {
    vote_preference[i] <- sample(c("Candidate A", "Candidate B"), size=1, prob=c(0.5, 0.5))
  } else {
    vote_preference[i] <- sample(c("Candidate A", "Candidate B"), size=1, prob=c(0.45, 0.55))
  }
}

# Creating a dataframe
data <- data.frame(Race=race, Gender=gender, Vote_Preference=vote_preference)

head(data)
