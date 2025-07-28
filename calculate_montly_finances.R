

# flat details:

# --------------
room1    = 26.04
room2    = 8.56
room3    = 14.55
kitchen  = 6.12
entrance = 8.65
bathroom = 4.13
storage1 = 2.32
lodgia1  = 6.96
lodgia2  = 6.96
storage2 = 5.29


# Property cost in CZK
property_price_czk  <- 8000000
reconstruction_cost <- 1500000
total_property_cost <- property_price_czk + reconstruction_cost

# Total area
# Total area
total_area <- room1 + room2 + room3 + kitchen + entrance + bathroom +
  storage1 + lodgia1 + lodgia2 + storage2

# costs  per living area

living_area <- room1 + room2 + room3 + kitchen + entrance + bathroom
cost_per_m2_living <- property_price_czk / living_area
(living_area)
(cost_per_m2_living)

usable_area <- total_area  # includes storage + lodgias
cost_per_m2_usable <- property_price_czk / usable_area
(usable_area)
(cost_per_m2_usable)


# -----------------------------
# Personal Finance & Mortgage Tracker with Rate Scenarios
# -----------------------------

# Exchange rate
rate <- 24  # 1 EUR = 24 CZK

# Savings (in CZK)
our_savings <- 70000 * rate
parents_savings <- 30000 * rate
total_savings <- our_savings + parents_savings

# Income (in CZK)
net_income_maja <- 39000
net_income_jake <- 50000 #3300 * rate
total_income <- net_income_maja + net_income_jake

# Monthly expenses (in CZK)
expenses_kita <- 5000
expenses_food <- 15000
expenses_living <- 10000
other_expenses <- 10000  # Add extra buffer 
#expenses_living_jake <- 500 * rate
fixed_expenses <- expenses_kita + expenses_food + expenses_living + other_expenses# + expenses_living_jake


# Mortgage loan needed
mortgage_principal <- total_property_cost - total_savings
mortgage_term_years <- 30
months <- mortgage_term_years * 12

# Interest rate scenarios
interest_rates <- c(0.0439)

# Storage for results
results <- data.frame(
  InterestRate = numeric(),
  MonthlyMortgage = numeric(),
  TotalMonthlyExpenses = numeric(),
  NetMonthlyBalance = numeric(),
  ratio_income_living = numeric(),
  MonthlySavings = numeric(),
  YearlySavings = numeric()
)

# Loop over scenarios
for (rate_scenario in interest_rates) {
  monthly_interest <- rate_scenario / 12
  
  # Monthly mortgage payment (annuity formula)
  monthly_mortgage_payment <- mortgage_principal * 
    (monthly_interest * (1 + monthly_interest)^months) / 
    ((1 + monthly_interest)^months - 1)
  
  # Total monthly expenses
  total_expenses <- fixed_expenses + monthly_mortgage_payment
  
  # Net monthly balance
  net_monthly_balance <- total_income - total_expenses
  
  # Ratio of income spent on living
  ratio_income_living <- total_expenses / total_income
  
  # Monthly & yearly savings
  monthly_savings <- net_monthly_balance
  yearly_savings <- net_monthly_balance * 12
  
  # Save results
  results <- rbind(results, data.frame(
    InterestRate = rate_scenario * 100,
    MonthlyMortgage = round(monthly_mortgage_payment),
    TotalMonthlyExpenses = round(total_expenses),
    NetMonthlyBalance = round(net_monthly_balance),
    ratio_income_living = round(ratio_income_living, 2),
    MonthlySavings = round(monthly_savings),
    YearlySavings = round(yearly_savings)
  ))
}

# -----------------------------
# Output Comparison
# -----------------------------
print("------ Mortgage Rate Scenarios ------")
print(results)



# ----------------------------------
# Create amortization table: principal vs interest
# ----------------------------------

# Function to create amortization table
create_amortization_table <- function(principal, annual_rate, term_years, start_date = as.Date("2025-08-20")) {
  months <- term_years * 12
  monthly_rate <- annual_rate / 12
  
  # Annuity payment
  monthly_payment <- principal * (monthly_rate * (1 + monthly_rate)^months) / 
    ((1 + monthly_rate)^months - 1)
  
  # Preallocate data frame
  schedule <- vector("list", months)
  balance <- principal
  dates <- seq(start_date, by = "month", length.out = months)
  
  for (i in 1:months) {
    interest <- balance * monthly_rate
    principal_paid <- monthly_payment - interest
    balance <- balance - principal_paid
    
    schedule[[i]] <- data.frame(
      Payment_No = i,
      Date = dates[i],
      Payment = round(monthly_payment),
      Principal = round(principal_paid),
      Interest = round(interest),
      Remaining_Balance = round(balance)
    )
  }
  
  dplyr::bind_rows(schedule)
}

# Generate amortization table using current mortgage principal
table_with_savings <- create_amortization_table(
  principal = mortgage_principal,
  annual_rate = interest_rates[1],
  term_years = 30
)

library(ggplot2)
library(dplyr)
library(tidyr)

table_with_savings %>%
  select(Date, Principal, Interest) %>%
  pivot_longer(cols = c(Principal, Interest), names_to = "Type", values_to = "Amount") %>%
  ggplot(aes(x = Date, y = Amount, color = Type)) +
  geom_line(size = 1) +
  labs(title = "Monthly Payment Breakdown: Principal vs Interest",
       x = "Date",
       y = "CZK",
       color = "") +
  theme_classic()



# ---------------------------------
# Payment & saving Maya vs Jake
# ---------------------------------

# Income shares
share_maja <- net_income_maja / total_income
share_jake <- net_income_jake / total_income


# Monthly mortgage payment
monthly_mortgage_payment <- mortgage_principal * 
  (monthly_interest * (1 + monthly_interest)^months) / 
  ((1 + monthly_interest)^months - 1)

# Split mortgage payment
maja_mortgage_payment <- monthly_mortgage_payment * share_maja
jake_mortgage_payment <- monthly_mortgage_payment * share_jake

# Parent loan repayment (for Maja only)
parent_loan_monthly_payment <- parents_savings / 60  # 5 years

# Shared expenses: assume split 50/50 or adjust similarly
maja_expenses <- fixed_expenses * 0.5
jake_expenses <- fixed_expenses * 0.5

# Maja monthly costs
maja_total_outflow <- maja_mortgage_payment + maja_expenses + parent_loan_monthly_payment
maja_net_balance <- net_income_maja - maja_total_outflow

# Jake monthly costs
jake_total_outflow <- jake_mortgage_payment + jake_expenses
jake_net_balance <- net_income_jake - jake_total_outflow

# -----------------------------
# Output Summary
# -----------------------------
cat("---- Monthly Split Summary ----\n")
cat("Maja's Share of Mortgage:", round(share_maja * 100), "%\n")
cat("Jake's Share of Mortgage:", round(share_jake * 100), "%\n\n")

cat("Monthly Mortgage Payment (Total):", round(monthly_mortgage_payment), "\n")
cat(" - Maja pays:", round(maja_mortgage_payment), "\n")
cat(" - Jake pays:", round(jake_mortgage_payment), "\n\n")

cat("Maja's Monthly Loan Repayment to Parents:", round(parent_loan_monthly_payment), "\n")

cat("\nMaja's Total Outflow:", round(maja_total_outflow), "\n")
cat("Jake's Total Outflow:", round(jake_total_outflow), "\n\n")

cat("Maja's Net Monthly Balance:", round(maja_net_balance), "\n")
cat("Jake's Net Monthly Balance:", round(jake_net_balance), "\n")
