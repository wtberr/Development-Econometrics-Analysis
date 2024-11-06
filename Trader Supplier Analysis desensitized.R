---
  title: "New trader supplier analysis"
author: "Will Berriman"
date: "`r Sys.Date()`"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
#Loading in the newly cleaned dataset#
```{r}
all <- read.csv("Data csv")
```
#Investigating outliers#
```{r}
# Load ggplot2 for plotting
library(ggplot2)

# Histogram for total_profit_3m with highlighted percentiles
ggplot(all, aes(x = total_profit_3m)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  geom_vline(xintercept = quantile(all$total_profit_3m, probs = 0.01, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = quantile(all$total_profit_3m, probs = 0.99, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Total Profit 3m", x = "Total Profit (3 Months)", y = "Frequency") +
  theme_minimal()

# Histogram for total_sales_3m with highlighted percentiles
ggplot(all, aes(x = total_sales_3m)) +
  geom_histogram(binwidth = 500, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = quantile(all$total_sales_3m, probs = 0.01, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = quantile(all$total_sales_3m, probs = 0.99, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Total Sales 3m", x = "Total Sales (3 Months)", y = "Frequency") +
  theme_minimal()
```
#Further investigation of the 3 outliers#
```{r}
# Calculate the 1st and 99th percentiles for each variable
profit_percentiles <- quantile(all$total_profit_3m, probs = c(0.01, 0.99), na.rm = TRUE)
sales_percentiles <- quantile(all$total_sales_3m, probs = c(0.01, 0.99), na.rm = TRUE)

# Filter data above and below the percentiles
profit_below <- all$total_profit_3m[all$total_profit_3m <= profit_percentiles[1]]
profit_above <- all$total_profit_3m[all$total_profit_3m >= profit_percentiles[2]]

sales_below <- all$total_sales_3m[all$total_sales_3m <= sales_percentiles[1]]
sales_above <- all$total_sales_3m[all$total_sales_3m >= sales_percentiles[2]]

# Calculate means
mean_total_profit <- mean(all$total_profit_3m, na.rm = TRUE)

mean_total_sales <- mean(all$total_sales_3m, na.rm = TRUE)


# Print results
cat("Total Profit 3m:\n")
cat("1st Percentile:", profit_percentiles[1], "\n")
cat("99th Percentile:", profit_percentiles[2], "\n")
cat("Overall Mean:", mean_total_profit, "\n")
cat("Below 1st Percentile:", na.omit(profit_below), "\n")
cat("Above 99th Percentile:", na.omit(profit_above), "\n\n")

cat("Total Sales 3m:\n")
cat("1st Percentile:", sales_percentiles[1], "\n")
cat("99th Percentile:", sales_percentiles[2], "\n")
cat("Overall Mean:", mean_total_sales, "\n")
cat("Below 1st Percentile:", na.omit(sales_below), "\n")
cat("Above 99th Percentile:", na.omit(sales_above), "\n")
```
#The observation with total_profit_3m = 108000000 and total_sales_3m = 32400000 must be an error, it implies negative cost, we trim it
#Trimming all observations above the 99th percentile for consistency#
```{r}
library(dplyr)
all <- all %>% filter(is.na(total_profit_3m) | total_profit_3m <= profit_percentiles[2])
```
#Now we check our distributions again#
```{r}
# Calculate the 1st and 99th percentiles for each variable
profit_percentiles <- quantile(all$total_profit_3m, probs = c(0.01, 0.99), na.rm = TRUE)
sales_percentiles <- quantile(all$total_sales_3m, probs = c(0.01, 0.99), na.rm = TRUE)

# Filter data above and below the percentiles
profit_below <- all$total_profit_3m[all$total_profit_3m <= profit_percentiles[1]]
profit_above <- all$total_profit_3m[all$total_profit_3m >= profit_percentiles[2]]

sales_below <- all$total_sales_3m[all$total_sales_3m <= sales_percentiles[1]]
sales_above <- all$total_sales_3m[all$total_sales_3m >= sales_percentiles[2]]

# Calculate means
mean_total_profit <- mean(all$total_profit_3m, na.rm = TRUE)

mean_total_sales <- mean(all$total_sales_3m, na.rm = TRUE)


# Print results
cat("Total Profit 3m:\n")
cat("1st Percentile:", profit_percentiles[1], "\n")
cat("99th Percentile:", profit_percentiles[2], "\n")
cat("Overall Mean:", mean_total_profit, "\n")
cat("Below 1st Percentile:", na.omit(profit_below), "\n")
cat("Above 99th Percentile:", na.omit(profit_above), "\n\n")

cat("Total Sales 3m:\n")
cat("1st Percentile:", sales_percentiles[1], "\n")
cat("99th Percentile:", sales_percentiles[2], "\n")
cat("Overall Mean:", mean_total_sales, "\n")
cat("Below 1st Percentile:", na.omit(sales_below), "\n")
cat("Above 99th Percentile:", na.omit(sales_above), "\n")
```
#Check the effects of our trimming on the distribution with visualization#
```{r}
# Histogram for total_profit_3m with highlighted percentiles
ggplot(all, aes(x = total_profit_3m)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  geom_vline(xintercept = quantile(all$total_profit_3m, probs = 0.01, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = quantile(all$total_profit_3m, probs = 0.99, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Total Profit 3m", x = "Total Profit (3 Months)", y = "Frequency") +
  theme_minimal()

# Histogram for total_sales_3m with highlighted percentiles
ggplot(all, aes(x = total_sales_3m)) +
  geom_histogram(binwidth = 500, fill = "lightgreen", color = "black") +
  geom_vline(xintercept = quantile(all$total_sales_3m, probs = 0.01, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = quantile(all$total_sales_3m, probs = 0.99, na.rm = TRUE), color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Histogram of Total Sales 3m", x = "Total Sales (3 Months)", y = "Frequency") +
  theme_minimal()
```

#creating some new varaibles#
```{r}
all$cost_calc = all$total_sales_3m - all$total_profit_3m
all$baseline_cost_calc = all$baseline_total_profit_3m - all$baseline_total_sales_3m
all$baseline_cost_active = all$baseline_total_profit_active - all$baseline_total_sales_active

all$move_mark_buy <- ifelse(is.na(all$buymarketsinmidline_ratio), NA,
                            ifelse(all$buymarketsinmidline_ratio == 1, 0, 1))

all$gender <- ifelse(all$baseline_gender == "Male", 1, 
                     ifelse(all$baseline_gender == "Female", 0, NA))

all$malesupmaletrad <- ifelse(is.na(all$gender) | is.na(all$pre_gender), NA, 
                              ifelse(all$gender == 1 & all$pre_gender == 1, 1, 0))

all$malesupfemtrad <- ifelse(is.na(all$gender) | is.na(all$pre_gender), NA, 
                             ifelse(all$gender == 1 & all$pre_gender == 0, 1, 0))

all$femsupfemtrad <- ifelse(is.na(all$gender) | is.na(all$pre_gender), NA, 
                            ifelse(all$gender == 0 & all$pre_gender == 0, 1, 0))

all$femsupmaletrad <- ifelse(is.na(all$gender) | is.na(all$pre_gender), NA, 
                             ifelse(all$gender == 0 & all$pre_gender == 1, 1, 0))

all$genderpair <- ifelse(all$malesupmaletrad == 1, 0,
                         ifelse(all$malesupfemtrad == 1, 1,
                                ifelse(all$femsupfemtrad == 1, 2,
                                       ifelse(all$femsupmaletrad == 1, 3, NA))))

all$PureFarmer <- ifelse(all$farmer_yn == "Yes" & is.na(all$prop_trader), 1, 0)

all$markup <- (all$total_sales_3m - all$total_profit_3m) / all$total_sales_3m

all$price_dif <- all$s_price_mgood_3m - all$baseline_s_price_mgood_3m
all$price_percent <- (all$s_price_mgood_3m - all$baseline_s_price_mgood_3m) / all$baseline_s_price_mgood_3m

all$unit_kg <- as.numeric(ifelse(gsub("[^0-9]", "", all$unit_mgood_3m) == "", NA, gsub("[^0-9]", "", all$unit_mgood_3m)))
all$price_p_kg <- all$s_price_mgood_3m / all$unit_kg

all$baseline_unit_kg <- as.numeric(ifelse(gsub("[^0-9]", "", all$baseline_unit_mgood_3m) == "", NA, gsub("[^0-9]", "", all$baseline_unit_mgood_3m)))
all$baseline_price_p_kg <- all$baseline_s_price_mgood_3m / all$baseline_unit_kg

all$num_clients <- ifelse(
  is.na(all$num_clients_check_3m) & is.na(all$num_clients_mkts_3m), 
  NA,
  ifelse(
    is.na(all$num_clients_check_3m), 
    all$num_clients_mkts_3m, 
    ifelse(
      is.na(all$num_clients_mkts_3m), 
      all$num_clients_check_3m, 
      all$num_clients_check_3m + all$num_clients_mkts_3m
    )
  )
)


# Load necessary libraries
library(tidyverse)
library(stargazer)
echo = FALSE
```
```{r}
# Create salesGProfit variable
all <- all %>% 
  mutate(salesGProfit = ifelse(is.na(total_profit_3m) | is.na(total_sales_3m), NA, 
                               ifelse(total_sales_3m >= total_profit_3m, 1, 0)))

all$salesGProfit
```
```{r}

all$moveandtreat <- ifelse(all$move_mark_buy == 0 & all$pre_treat_trader == 0, "Control Stable Trader",
                           ifelse(all$move_mark_buy == 1 & all$pre_treat_trader == 0, "Control Trader Moved",
                                  ifelse(all$move_mark_buy == 0 & all$pre_treat_trader == 1, "Treated Stable Trader",
                                         ifelse(all$move_mark_buy == 1 & all$pre_treat_trader == 1, "Treated Trader Moved", NA))))
all$Gender_Levels <- ifelse(all$gender == 0, "Female",
                            ifelse(all$gender == 1, "Male", NA))
all$Profit_Over_Three_Months <- all$total_profit_3m

# Summarize data to calculate the average profit for each group
all_summary <- all %>%
  group_by(moveandtreat, Gender_Levels) %>%
  summarize(Avg_Profit = mean(Profit_Over_Three_Months, na.rm = TRUE))

# Plot with summarized data
ggplot(data=all_summary, aes(x=moveandtreat, y=Avg_Profit, fill=Gender_Levels)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(Avg_Profit, 2)), 
            vjust=-0.3, color="black", size=2, 
            position=position_dodge(width=0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Average Profit Over Three Months", x = "",
       title = "Average Profit by Movement and Treatment for Each Gender Level")
```
```{r}
# Summarize data to calculate the average profit for each group
all_summary2 <- all %>%
  group_by(moveandtreat, Gender_Levels) %>%
  summarize(Avg_Markup = mean(markup, na.rm = TRUE))

# Plot with summarized data
ggplot(data=all_summary2, aes(x=moveandtreat, y=Avg_Markup, fill=Gender_Levels)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(Avg_Markup, 2)), 
            vjust=-0.3, color="black", size=2, 
            position=position_dodge(width=0.9)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Mean Inverse Markup (Cost/Sales) Over 3 Months", x = "",
       title = "Mean Inverse Markup by Movement and Treatment for Each Gender Level")
```


```{r}
library(readr)
library(stringr)
library(tableone)
echo = FALSE
```

#Visualizing the new genderparing data#
```{r}
values = c(mean(all$malesupmaletrad, na.rm = TRUE), mean(all$malesupfemtrad, na.rm = TRUE), mean(all$femsupfemtrad, na.rm = TRUE), mean(all$femsupmaletrad, na.rm = TRUE))
names = c("Male Supplier and Male Trader", "Male Supplier Female Trader", "Female Supplier Female Trader", "Female Supplier Male Trader")
# Create a bar plot
bar_heights <- barplot(
  values, 
  names.arg = names,     # Names for each bar
  col = "lightblue",     # Color for the bars
  ylim = c(0, 1),
  main = "Supplier Trader Gender-Based Pairing Proportions", # Main title
  xlab = "Gender-Pairings",       # X-axis label
  ylab = "Proportion Observed in Study"         # Y-axis label
)
# Add text labels on top of each bar
text(
  x = bar_heights,                # X-coordinates of the bars
  y = values,                     # Y-coordinates (the height of the bars)
  labels = round(values, 2),      # Labels (rounded values to 2 decimal places)
  pos = 3                         # Position 3: Above the bars
)

```
```{r}
variables <- c("gender", "pre_gender", "baseline_total_profit_active", "baseline_total_cost_active", "cost_calc","num_crops", 
               "crop_revenue_total", "prop_farmer", "prop_trader", "baseline_age")
strata <- "pre_treat_trader"

# Create the balance table
balance_table <- CreateTableOne(vars = variables, strata = strata, data = all, test = TRUE, addOverall = TRUE)

# Print the table with 'n' included for each group
print(balance_table, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE, includeNA = TRUE)

```
```{r}
g3 <- subset(all, 
             !is.na(baseline_total_profit_3m))
some <- subset(all, is.na(baseline_total_profit_3m))

some$group <- "no baseline profit"
g3$group <- "baseline profit"

# Combine 'some' and 'g3' into one data frame
combined <- rbind(some, g3)

variables <- c("baseline_age", "msup_country_", "total_profit_3m", "total_sales_3m", "cost_calc",
               "markup", "prop_farmer", "prop_trader", "move_mark_buy", "baseline_total_profit_active", "baseline_total_cost_active")

# Create the balance table
balance_table <- CreateTableOne(vars = variables, strata = "group", data = combined)

# Print the balance table
print(balance_table, showAllLevels = TRUE)

```

#Now we begin our single interactions#

#Treatment and Trader/Supplier Gender Parings#
```{r}
# Define the list of models to run ONE DOUBLE INTERACTIONS
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales) (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier", 
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```


```{r}
# Models with total_profit_3m as outcome variable
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("Total Profit 3 Months (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier",
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
# Models with total_sales_3m as outcome variable
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("Total Sales 3 Months (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier", 
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
# Models with cost_calc as outcome variable
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("Total Cost 3 Months (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier", 
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
#Treatment and Gender#
```{r}
# Models with cost_calc as outcome variable
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_cost_calc, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Cost 3 Months "),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Cost 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
# Models with total_profit_3m as outcome variable
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Profit 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_sales_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_sales_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Sales 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
#If supplier is man, they do better off, aligns with gender paring, male supplier effect on markups

#Treatment and Trader Moving Suppliers#

```{r}
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Profit 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
```{r}
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_sales_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_total_sales_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Sales Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
#funky numbers in model 4 are likely do to only 32 observations

```

```{r}
# Models with cost_calc as outcome variable
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_cost_calc, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Cost 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Cost Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
# again, check observation count in model4

```

```{r}
# Models with markup as outcome variable
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
#a bit counterintuitive, but significant, maybe if they lost a client had to increase markup

#Treatment and being a Pure Farmer (as specified in the variable initiation above)#
```{r}
# Models with total_profit_3m as outcome variable
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Profit 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```
```{r}
# Models with total_sales_3m as outcome variable
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_sales_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_total_sales_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)



# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Sales 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Models with cost_calc as outcome variable
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_cost_calc, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)



# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Cost 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Cost 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Models with markup as outcome variable
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)



# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```
#Introduce inverse hyperbolic sine function and apply it to outcomes to reduce noise#
```{r}
asinh <- function(x) {
  log(x + sqrt(x^2 + 1))
}

all$asin_profit_3m <- asinh(all$total_profit_3m)
all$asin_sales_3m <- asinh(all$total_sales_3m)
all$asin_cost_calc <- all$asin_sales_3m - all$asin_profit_3m
all$asin_markup <- all$asin_cost_calc / all$asin_sales_3m
```



#Now we begin our single interactions#

#Treatment and Trader/Supplier Gender Parings#
```{r}
# Define the list of models to run ONE DOUBLE INTERACTIONS
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("ASINH Inverse Markup (Cost/Sales) (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier", 
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```


```{r}
# Models with total_profit_3m as outcome variable
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("ASINH Total Profit 3 Months (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier",
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
# Models with total_sales_3m as outcome variable
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("ASINH Total Sales 3 Months (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier", 
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
# Models with cost_calc as outcome variable
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(genderpair) + factor(pre_treat_trader)*factor(genderpair) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
# Generate the table
stargazer(model1, model2, model3, type = "text", 
          dep.var.labels = c("ASINH Total Cost 3 Months (Male SUpplier Male Trader COntrol)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male Supplier Female Trader",
                               "Female SUpplier Female Trader",
                               "Female Supplier Male Trader",
                               "Age", 
                               "Country of supplier", 
                               "Treatment x Male/Female",
                               "Mixed Treatment x Male/Female",
                               "Treatment x Female/Female",
                               "Mixed Treatment x Female/Female",
                               "Treatment x Female/Male",
                               "Mixed Treatment x Female/Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
#Treatment and Gender#
```{r}
# Models with cost_calc as outcome variable
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_cost_calc, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Cost 3 Months "),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Cost 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
# Models with total_profit_3m as outcome variable
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("TASINH otal Profit 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_sales_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_sales_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Sales 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```

```{r}
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Male",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit 3 Months",
                               "Treatment x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
#If supplier is man, they do better off, aligns with gender paring, male supplier effect on markups

#Treatment and Trader Moving Suppliers#

```{r}
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Profit 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
```{r}
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_sales_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_total_sales_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Sales 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Sales Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
#funky numbers in model 4 are likely do to only 32 observations

```

```{r}
# Models with cost_calc as outcome variable
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_cost_calc, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Cost 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Cost Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
# again, check observation count in model4

```

```{r}
# Models with markup as outcome variable
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "move_mark_buy", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)

# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Moved Suppliers",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Movement",
                               "Mixed Treatment x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")

```
#on average it seems like not super important, when trader left did not change markup

#a bit counterintuitive, but significant, maybe if they lost a client had to increase markup

#Treatment and being a Pure Farmer (as specified in the variable initiation above)#
```{r}
# Models with total_profit_3m as outcome variable
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Profit 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```
```{r}
# Models with total_sales_3m as outcome variable
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_sales_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_total_sales_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)



# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Sales 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Sales 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Models with cost_calc as outcome variable
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_cost_calc, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_cost_calc")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)



# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Cost 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Cost 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Models with markup as outcome variable
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_profit_3m, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Control means
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)



# Generate the table
stargazer(model1, model2, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Inverse Markup (Cost/sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Pure Farmer",
                               "Age", 
                               "Country of supplier",
                               "Baseline Total Profit 3 Months",
                               "Treatment x Pure Farmer",
                               "Mixed Treatment x Pure Farmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


#Now two double interactions#

#treatmentxgender and treatmentxmovement#
```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$markup[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$markup[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_markup[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_markup[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```
```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$total_profit_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$total_profit_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Profits over 3 months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Profit",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_profit_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_profit_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Profits over 3 months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Profit",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$total_sales_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$total_sales_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_sales_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_sales_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_cost_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$cost_calc[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$cost_calc[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + baseline_age + msup_country_ + baseline_total_cost_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_cost_calc[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_cost_calc[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "Male",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x Male",
                               "Mixed Treatment x Male"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

#Now treatxmove and treatxPureFarmer#

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$markup[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$markup[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_markup[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_markup[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```
```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$total_profit_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$total_profit_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Profits over 3 months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Profit",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_profit_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_profit_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Profits over 3 months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Profit",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$total_sales_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$total_sales_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_sales_3m[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_sales_3m[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_cost_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$cost_calc[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$cost_calc[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TWO DOUBLE INTERACTIONS
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy), data = all)
model2.25 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer)  + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(move_mark_buy) + factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + baseline_age + msup_country_ + baseline_total_cost_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn")]), ]
model5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_")]), ]
model6_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "farmer_yn", "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean5 <- mean(model3_data$asin_cost_calc[model5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean6 <- mean(model4_data$asin_cost_calc[model6_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.25, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed Treatment",
                               "Trader Movement",
                               "PureFarmer",
                               "Age", 
                               "Country of supplier", 
                               "Baseline Sales",
                               "Trader Treated x Movement",
                               "Mixed Treatment x Movement",
                               "Trader Treated x PureFarmer",
                               "Mixed Treatment x PureFarmer"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3)),
                    paste("Control mean for model 5:", round(control_mean5, 3)),
                    paste("Control mean for model 6:", round(control_mean6, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```
#Triple Interaction#

#treatmentxgenderxtrader movement#
```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$total_profit_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Profit over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_profit_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Profit over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_sales_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$total_sales_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$cost_calc[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_cost_calc[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(markup ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$markup[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(gender) + factor(pre_treat_trader)*factor(gender), data = all)
model2.5 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(gender) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(gender) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(gender)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "gender", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_markup[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "Male",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x Male",
                               "Mixed Treatment x Male",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x Male",
                               "Treatment x Male x Movement",
                               "Mixed Treatment x Male x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```
Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

#treatmentxfarmerxmove#

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(total_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(total_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$total_profit_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Profit over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_profit_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_profit_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_profit_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_profit_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_profit_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_profit_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_profit_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Profit over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_sales_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(total_sales_3m ~ factor(pre_treat_trader), data = all)
model2 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(total_sales_3m ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_sales_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$total_sales_3m[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$total_sales_3m[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$total_sales_3m[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$total_sales_3m[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$total_sales_3m[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Sales over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$cost_calc[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_cost_calc ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_cost_calc ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_cost_calc[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_cost_calc[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_cost_calc[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_cost_calc[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_cost_calc[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Total Cost over 3 Months"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$markup[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```

```{r}
# Define the list of models to run TRIPLE INTERACTIONS
model1 <- lm(asin_markup ~ factor(pre_treat_trader), data = all)
model2 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(pre_treat_trader)*factor(PureFarmer), data = all)
model2.5 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy), data = all)
model3 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_, data = all)
model4 <- lm(asin_markup ~ factor(pre_treat_trader) + factor(PureFarmer) + factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer)*factor(move_mark_buy) + factor(pre_treat_trader)*factor(PureFarmer) + factor(pre_treat_trader)*factor(move_mark_buy) + factor(PureFarmer)*factor(move_mark_buy) + baseline_age + msup_country_ + baseline_total_profit_active, data = all)

model1_data <- all[complete.cases(all[c("pre_treat_trader")]), ]
model2_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m")]), ]
model2.5_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                          "baseline_age", "msup_country_")]), ]
model3_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_")]), ]
model4_data <- all[complete.cases(all[c("pre_treat_trader", "PureFarmer", "num_clients_mkts_3m", 
                                        "baseline_age", "msup_country_", "baseline_total_profit_3m")]), ]

# Step 2: Calculate the control means for each model (pre_treat_trader == 0)
control_mean1 <- mean(model1_data$asin_markup[model1_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2 <- mean(model2_data$asin_markup[model2_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean2_5 <- mean(model2.5_data$asin_markup[model2.5_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean3 <- mean(model3_data$asin_markup[model3_data$pre_treat_trader == 0], na.rm = TRUE)
control_mean4 <- mean(model4_data$asin_markup[model4_data$pre_treat_trader == 0], na.rm = TRUE)


# Generate the table
stargazer(model1, model2, model2.5, model3, model4, type = "text", 
          dep.var.labels = c("ASINH Inverse Markup (Cost/Sales)"),
          covariate.labels = c("Treated Trader",
                               "Mixed treatment",
                               "PureFarmer",
                               "Trader Changed Supplier",
                               "Age", 
                               "Country of supplier",
                               "Baseline Profit",
                               "Treatment x PureFarmer",
                               "Mixed Treatment x PureFarmer",
                               "Treatment x Trader Movement",
                               "Mixed Treatment x Trader Movement",
                               "Movement x PureFarmer",
                               "Treatment x PureFarmer x Movement",
                               "Mixed Treatment x PureFarmer x Movement"),
          omit.stat = c("f", "ser", "adj.rsq"),
          column.labels = c("", "", "", ""),
          notes = c("Note: Standard errors robust (reported in brackets).", 
                    "* p<0.1; ** p<0.05; *** p<0.01",
                    paste("Control mean for model 1:", round(control_mean1, 3)),
                    paste("Control mean for model 2:", round(control_mean2, 3)),
                    paste("Control mean for model 2.5:", round(control_mean2_5, 3)),
                    paste("Control mean for model 3:", round(control_mean3, 3)),
                    paste("Control mean for model 4:", round(control_mean4, 3))),
          report = "vc*p",  # This will report coefficients, standard errors, and p-values
          out = "reg_results19.txt")
```


Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
