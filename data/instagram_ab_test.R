# Load required packages
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate sample size
n <- 1000

# Create user demographics
data_instagram <- data.frame(
    user_id = 1:n,
    age = round(rnorm(n, mean = 25, sd = 5)),
    gender = sample(c("M", "F"), n, replace = TRUE, prob = c(0.45, 0.55)),
    account_age_days = round(runif(n, 30, 730))
)

# Generate pre-treatment activity metrics
data_instagram <- data_instagram %>%
    mutate(
        pre_posts = round(rpois(n, lambda = 3)), # Average 3 posts per month
        pre_likes = round(rpois(n, lambda = 30)), # Average 30 likes per month
        pre_comments = round(rpois(n, lambda = 10)), # Average 10 comments per month
        pre_total_activity = pre_posts + pre_likes + pre_comments
    )

# Assign treatments randomly (30% treatment A, 30% treatment B, 40% control)
data_instagram$treatment <- sample(c("control", "A", "B"), n,
    replace = TRUE,
    prob = c(0.4, 0.3, 0.3)
)

# Convert treatment to factor with control as baseline
data_instagram$treatment <- factor(data_instagram$treatment,
    levels = c("control", "A", "B")
)

# Set different treatment effects
treatment_effect_A <- 0.3 # 30% increase for treatment A
treatment_effect_B <- 0.8 # 80% increase for treatment B

# Generate post-treatment activity metrics with different treatment effects
data_instagram <- data_instagram %>%
    mutate(
        post_posts = round(rpois(n, lambda = 3 * (1 +
            treatment_effect_A * (treatment == "A") +
            treatment_effect_B * (treatment == "B")))),
        post_likes = round(rpois(n, lambda = 30 * (1 +
            treatment_effect_A * (treatment == "A") +
            treatment_effect_B * (treatment == "B")))),
        post_comments = round(rpois(n, lambda = 10 * (1 +
            treatment_effect_A * (treatment == "A") +
            treatment_effect_B * (treatment == "B")))),
        post_total_activity = post_posts + post_likes + post_comments
    )

# Save the dataset
write.csv(data_instagram, "instagram_ab_test.csv", row.names = FALSE)
