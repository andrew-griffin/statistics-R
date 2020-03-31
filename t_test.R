# Using the t-test to compare two distributions

# The two distributions A and B
A = c(13.8,14.5,13.9,15.2,13.9,14.7,14.3,14.8,14.2,11.2,16.9,14.4)
B = c(17.8,16.3,19.4,15.4,16.6,16.8,17.8,16.9,14.5,16.9,16.2,16.3)

# Plot distributions
bins <- seq(10.5, 19.5, length.out = 10)
hA <- hist(A, breaks=bins)
hB <- hist(B, breaks=bins)
plot(hA, col=rgb(0,0,1,1/4))
plot(hB, col=rgb(1,0,0,1/4), add=T)

# Mean and standard deviations
mean_A = mean(A)
sd_A = sd(A)
mean_B = mean(B)
sd_B = sd(B)

# t-test
t = (mean_B - mean_A)/(sqrt(sd_A^2 / length(A) + sd_B^2 / length(B)))
t
