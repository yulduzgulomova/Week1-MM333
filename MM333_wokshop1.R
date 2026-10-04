#change in working directory
setwd("D:/Reading/Advanced data analytics/Week 1")

#library download


library(readr)
library(tidyverse)
library(janitor)
library(psych)
library(effectsize)
library(broom)

#import a dataset
customers <- read_csv("MM333_customer_marketing.csv")
View(customers)

# to know about the structure and quality of dataset
customers |> glimpse()
customers |> names()
customers |> summary()
customers |> summarise(
  rows = n(),
  missing_values = sum(is.na(across(everything())))
)

# to check variable classes:
customers |> summarise(across(everything(), class))

# to check duplicate customer IDs:
customers |>
  count(customer_id) |>
  filter(n > 1)


# To calculate Descriptive Statistics
customers |>
  group_by(campaign) |>
  summarise(
    n = n(),
    mean_spend = mean(monthly_spend_gbp, na.rm = TRUE),
    sd_spend = sd(monthly_spend_gbp, na.rm = TRUE),
    median_spend = median(monthly_spend_gbp, na.rm = TRUE),
    min_spend = min(monthly_spend_gbp, na.rm = TRUE),
    max_spend = max(monthly_spend_gbp, na.rm = TRUE)
  )

customers |>
  dplyr::group_by(campaign) |>
  dplyr::group_modify(
    ~ psych::describe(.x$monthly_spend_gbp) |>
      tibble::as_tibble()
  )



#to visualise the outcomes - boxplots

ggplot(customers, aes(x = campaign, y = monthly_spend_gbp, fill = campaign)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.25, show.legend = FALSE) +
  labs(
    x = "Campaign exposure",
    y = "Monthly spending (£)",
    title = "Monthly spending by campaign exposure"
  ) +
  theme_minimal()


#To inspect distributions

# Histogram
ggplot(customers, aes(x = monthly_spend_gbp)) +
  geom_histogram(bins = 20, colour = "white") +
  facet_wrap(~ campaign) +
  theme_minimal()


# Q-q plot

ggplot(customers, aes(sample = monthly_spend_gbp)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ campaign) +
  theme_minimal()

# Formal variance check only as supporting evidence
customers |>
  summarise(
    variance_campaign = var(monthly_spend_gbp[campaign == "Campaign"], na.rm = TRUE),
    variance_control = var(monthly_spend_gbp[campaign == "Control"], na.rm = TRUE)
  )


# Welch independent samples t-test
t_test_result <- t.test(
  monthly_spend_gbp ~ campaign,
  data = customers,
  var.equal = FALSE,
  alternative = "two.sided"
)
t_test_result


# to Calculate a standardised effect size:
effectsize::cohens_d(
  monthly_spend_gbp ~ campaign,
  data = customers,
  pooled_sd = FALSE
  )
  
#Tidy the test result:
broom::tidy(t_test_result)
 

#Linear Regression Model
model_1 <- lm(
  monthly_spend_gbp ~ campaign + website_visits + loyalty_status + age,
  data = customers
)
summary(model_1)
broom::tidy(model_1, conf.int = TRUE)

#to check the reference category
contrasts(factor(customers$campaign))


# Regression diagnostics
par(mfrow = c(2, 2))
plot(model_1)
par(mfrow = c(1, 1))

#To check the interaction whether campaign works differently to loyalty and non-loyalty customers
model_2 <- lm(
  monthly_spend_gbp ~ campaign * loyalty_status + website_visits + age,
  data = customers
)
summary(model_2)

par(mfrow = c(2, 2))
plot(model_2)
par(mfrow = c(1, 1))



