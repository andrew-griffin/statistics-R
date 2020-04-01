# This routine uses the ANOVA test to explore the effect of music 
# on the test scores of students
# Using the data table of 
# https://www.analyticsvidhya.com/blog/2018/01/anova-analysis-of-variance/

# Test scores
constant_sound = c(7,9,5,8,6,8,6,10,7,4)
variable_sound = c(4,3,6,2,7,5,5,4,1,3)
no_sound = c(6,1,3,5,3,4,6,5,7,3)

# Combine for boxplot
data1 <- data.frame(constant_sound, variable_sound, no_sound)

# Make boxplot
boxplot(data1)

# Prepare data for ANOVA test
scores = c(constant_sound, variable_sound, no_sound)
types= c("c", "c", "c", "c", "c", "c", "c", "c", "c", "c", "v", "v", "v", "v", "v", "v", "v", "v", "v", "v", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n")

# Combine into a data frame
data2 <- data.frame(scores, types)
data2

# ANOVA test
res.aov <- aov(scores~types,data=data2)
summary(res.aov)

# Check the ANOVA test

# Check homogeneity of variances
plot(res.aov, 1)

# Normality plot of residuals
plot(res.aov, 2)
