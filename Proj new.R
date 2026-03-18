############################################################
# UNIT I – INTRODUCTION TO R
############################################################

# Clear environment (prevents closure errors)
rm(list = ls())

# Load CSV file (make sure file is in working directory)
df <- read.csv("SuperMarket Analysis.csv")

# Check structure of dataset
str(df)

# View first few rows
head(df)

############################################################
# UNIT II – DESCRIPTIVE STATISTICS
############################################################

# Ensure Sales column is numeric
df$Sales <- as.numeric(df$Sales)

# Measures of Location
mean_sales   <- mean(df$Sales, na.rm = TRUE)
median_sales <- median(df$Sales, na.rm = TRUE)

# Measures of Variation
variance_sales <- var(df$Sales, na.rm = TRUE)
sd_sales       <- sd(df$Sales, na.rm = TRUE)

cat("Mean Sales:", mean_sales, "\n")
cat("Median Sales:", median_sales, "\n")
cat("Variance:", variance_sales, "\n")
cat("Standard Deviation:", sd_sales, "\n")

############################################################
# FREQUENCY TABLE
############################################################

product_freq <- table(df$Product.line)
print(product_freq)

############################################################
# BAR DIAGRAM
############################################################

barplot(product_freq,
        col = "lightblue",
        las = 2,
        main = "Bar Diagram of Product Line",
        ylab = "Frequency")

############################################################
# HISTOGRAM
############################################################

hist(df$Sales,
     breaks = 30,
     col = "skyblue",
     main = "Histogram of Sales",
     xlab = "Sales Amount",
     ylab = "Frequency")

abline(v = mean_sales, col = "red", lwd = 2)
abline(v = median_sales, col = "blue", lwd = 2, lty = 2)

legend("topright",
       legend = c("Mean", "Median"),
       col = c("red", "blue"),
       lty = 1:2)

############################################################
# HEATMAP (Branch vs Product Line)
############################################################

heat_data <- table(df$Branch, df$Product.line)
heat_matrix <- as.matrix(heat_data)

heatmap(heat_matrix,
        col = heat.colors(10),
        main = "Heatmap: Branch vs Product Line")

############################################################
# BAYES' RULE (UNIT II)
############################################################

# P(Member)
p_member <- mean(df$Customer.type == "Member")

# P(Electronics)
p_elec <- mean(df$Product.line == "Electronic accessories")

# P(Electronics | Member)
p_elec_given_member <- mean(
  df$Product.line[df$Customer.type == "Member"] 
  == "Electronic accessories"
)

# Bayes Formula
p_member_given_elec <- (p_elec_given_member * p_member) / p_elec

cat("P(Member | Electronics):", round(p_member_given_elec, 4), "\n")

############################################################
# UNIT III – BINOMIAL DISTRIBUTION
############################################################

# Probability exactly 3 Members out of 10 customers
dbinom(3, size = 10, prob = p_member)

############################################################
# POISSON DISTRIBUTION (Customer Arrivals per Hour)
############################################################

# Extract hour from Time column (format HH:MM)
df$Hour <- as.numeric(substr(df$Time, 1, 2))

arrival_counts <- as.data.frame(table(df$Date, df$Hour))
lambda <- mean(arrival_counts$Freq)

x <- 0:10
y <- dpois(x, lambda = lambda)

plot(x, y,
     type = "b",
     pch = 19,
     main = paste("Poisson Distribution (Lambda =", round(lambda,2), ")"),
     xlab = "Customers per Hour",
     ylab = "Probability")

############################################################
# NORMAL DISTRIBUTION
############################################################

x_norm <- seq(min(df$Sales), max(df$Sales), length = 100)
y_norm <- dnorm(x_norm, mean_sales, sd_sales)

hist(df$Sales,
     probability = TRUE,
     breaks = 30,
     col = "lightgreen",
     main = "Normal Distribution Fit",
     xlab = "Sales")

lines(x_norm, y_norm, col = "red", lwd = 2)

############################################################
# UNIFORM DISTRIBUTION (Example)
############################################################

# Example: Uniform between 0 and 1000
dunif(500, min = 0, max = 1000)

