# Read in data covering whether candidates gain admission into 
# graduate school. Shown are GRE score, GPA (grade), and prestige 
# undergraduate institution.
# The data is from https://stats.idre.ucla.edu/stat/data/binary.csv

data <- read.csv("data/grad_school_admission.csv")
head(data)

# Pick out columns
admit = data[,1]
gre = data[,2]
gpa = data[,3]
rank = data[,4]

# View data
plot(gre,admit) # plot effect of gre
plot(gpa,admit) # plot effect of gpa
plot(rank,admit) # plot effect of rank

# Logistic regression 
logit = glm(formula = admit ~ gre + gpa + rank, family="binomial", data=data)

# Output summary
summary(logit)

# Get coefficients
intercept <- logit$coefficients[1]
coeff_gre <- logit$coefficients[2]
coeff_gpa <- logit$coefficients[3]
coeff_rank <- logit$coefficients[4]

# Plot regression probabilities
gre_array <- seq(0,1000, length.out=10)
P_gre <- 1/(1 + exp(-(coeff_gre*gre_array + intercept)))
plot(gre,admit) # original data
lines(gre_array,P_gre, type="l") # fit 

gpa_array <- seq(0,5, length.out=10)
P_gpa <- 1/(1 + exp(-(coeff_gpa*gpa_array + intercept)))
plot(gpa,admit) # original data
lines(gpa_array,P_gpa, type="l") # fit 

rank_array <- seq(0,5, length.out=10)
P_rank <- 1/(1 + exp(-(coeff_rank*rank_array + intercept)))
plot(rank,admit) # original data
lines(rank_array,P_rank, type="l") # fit




