

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
net_income_jake <- 2900 * rate
total_income <- net_income_maja + net_income_jake

# Monthly expenses (in CZK)
expenses_kita <- 5000
expenses_food <- 15000
expenses_living <- 9000
other_expenses <- 10000  # Add extra buffer if needed
fixed_expenses <- expenses_kita + expenses_food + expenses_living + other_expenses

# Property cost in CZK
property_price_czk <- 8500000
reconstruction_cost <- 1500000
total_property_cost <- property_price_czk + reconstruction_cost

# Mortgage loan needed
mortgage_principal <- total_property_cost - total_savings
mortgage_term_years <- 30
months <- mortgage_term_years * 12

# Interest rate scenarios
interest_rates <- c(0.046, 0.049, 0.052)

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

# Optional: Plot Net Monthly Balance by Scenario
# Uncomment if ggplot2 is installed
# library(ggplot2)
# ggplot(results, aes(x = factor(InterestRate), y = NetMonthlyBalance)) +
#   geom_col(fill = "steelblue") +
#   labs(title = "Net Monthly Balance by Mortgage Interest Rate",
#        x = "Interest Rate (%)",
#        y = "Net Monthly Balance (CZK)")
