# Exploring the correlation between GRE score and GPA score
# using linear and Bayesian regression
# The data is from https://stats.idre.ucla.edu/stat/data/binary.csv

# Load relevant libraries
library(Bolstad)

# Read in data
data <- read.csv("data/grad_school_admission.csv")

# Select relevant data
gre = data[,2]
gpa = data[,3]

# Plot data
plot(gpa,gre)

# Linear regression
model <- lm(gre ~ gpa)
abline(model)

# Bayesian regression
model<- bayes.lin.reg(gre, gpa, plot.x = TRUE)
