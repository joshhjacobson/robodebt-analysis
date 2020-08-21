
## Social Security payment recieved

library(tidyverse)


set_situation <- function(
  max_payment,
  thresholds,
  rates,
  income_cap
) {
  # Construct situation object
  # args:
  #   max_payment: maximum fortnightly payment possible
  #   thresholds: income levels at which payment is reduced by corresponding rates
  #   rates: fraction of each dollar payment is reduced by when income exceeds corresponding threshold
  #   income_cap: maximum income before payment reduces to $0
  # value:
  #   situation: list storing each of the input values
  return (
    list(
      max_payment = max_payment,
      t1 = thresholds[1],
      r1 = rates[1],
      t2 = thresholds[2],
      r2 = rates[2],
      step = (thresholds[2] - thresholds[1])*rates[1],
      income_cap = income_cap
    )
  )
}

payment_received <- function(x, situation) {
  # Calculate the payment received in a given fortnight based on situation and corresponding fortnightly income.
  # args:
  #   x: fortnightly income (scalar or vector valued)
  #   situation: object describing the individual's sitation
  # value:
  #   payment: social security payment recieved
  ifelse(x < situation$t1, situation$max_payment,
         ifelse(x < situation$t2, situation$max_payment - (x-situation$t1)*situation$r1,
                ifelse(x < situation$income_cap, situation$max_payment - situation$step - (x-situation$t2)*situation$r2, 0)))
}



# Case analysis -----------------------------------------------------------

income <- seq(0, 1500, 5)

single_under18_home <- set_situation(
  max_payment = 462.5,
  thresholds = c(437.0, 524.0),
  rates = c(0.5, 0.6),
  income_cap = 880.0
)

single_children <- set_situation(
  max_payment = 606.0,
  thresholds = c(437.0, 524.0),
  rates = c(0.5, 0.6),
  income_cap = 1476.84
)

couple_children <- set_situation(
  max_payment = 507.9,
  thresholds = c(437.0, 524.0),
  rates = c(0.5, 0.6),
  income_cap = 1310.84
)

payment_data <- data.frame(
  income = income,
  s1 = payment_received(income, single_under18_home),
  s2 = payment_received(income, single_children),
  s3 = payment_received(income, couple_children)
)


png("payment_recieved_example.png", units="in", width=6, height=5, pointsize=9, res=150)
pivot_longer(payment_data, cols = starts_with("s"), names_to = "Situation") %>%
  ggplot(aes(x = income, y = value)) +
  geom_line(size=0.5, aes(linetype = Situation)) +
  scale_linetype_manual(
    labels=c(
      "Single, under 18, living at home", 
      "Single with children", 
      "In a couple, with children"),
    values = c("dashed", "dotdash", "dotted")
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.85)
  ) +
  labs(x = "Fortnightly Income", y = "Payment Recieved", title = "Payment Recieved: Austudy")
dev.off()
 
