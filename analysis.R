
## Social Security payment recieved

library(tidyverse)


calc_income_cap <- function(
  max_payment,
  thresholds,
  rates
) {
  # Calculate maximum income before payment actually reduces to $0.
  uniroot(function(x) max_payment - (thresholds[2] - thresholds[1])*rates[1] - (x - thresholds[2])*rates[2], interval = c(500, 2000))$root
}

set_situation <- function(
  label,
  max_payment,
  thresholds,
  rates
  # income_cap
) {
  # Construct situation object
  # args:
  #   label: situation label
  #   max_payment: maximum fortnightly payment possible
  #   thresholds: income levels at which payment is reduced by corresponding rates
  #   rates: fraction of each dollar payment is reduced by when income exceeds corresponding threshold
  #   income_cap: maximum income before payment reduces to $0
  # value:
  #   situation: list storing each of the input values
  return (
    list(
      label = label,
      max_payment = max_payment,
      t1 = thresholds[1],
      r1 = rates[1],
      t2 = thresholds[2],
      r2 = rates[2],
      step = (thresholds[2] - thresholds[1])*rates[1],
      # income_cap = income_cap
      income_cap = calc_income_cap(max_payment, thresholds, rates)
    )
  )
}

payment_received <- function(situation, x) {
  # Calculate the payment received in a given fortnight based on situation and corresponding fortnightly income.
  # args:
  #   situation: list describing the individual's sitation
  #   x: fortnightly income (scalar or vector valued)
  # value:
  #   payment: social security payment recieved
  ifelse(x < situation$t1, situation$max_payment,
         ifelse(x < situation$t2, situation$max_payment - (x-situation$t1)*situation$r1,
                ifelse(x < situation$income_cap, situation$max_payment - situation$step - (x-situation$t2)*situation$r2, 0)))
}

# stated_income_cap <- function(situation) {
#   # Retreive stated maximum income before payment reduces to $0 from situation.
#   situation$income_cap
# }
# 
# calc_income_cap <- function(situation) {
#   # Calculate maximum income before payment actually reduces to $0.
#   root <- uniroot(function(x) situation$max_payment - situation$step - (x-situation$t2)*situation$r2, interval = c(500, 2000))$root
#   ifelse(root < situation$income_cap, root, situation$income_cap)
# }


# Case analysis -----------------------------------------------------------

income <- seq(0, 1500, 5)

situations <- list(
  
  single_under18_home = set_situation(
    label = "Single, under 18, living at home",
    max_payment = 462.5,
    thresholds = c(437.0, 524.0),
    rates = c(0.5, 0.6)
    # income_cap = 880.0
  ),
  
  single_children = set_situation(
    label = "Single with children",
    max_payment = 606.0,
    thresholds = c(437.0, 524.0),
    rates = c(0.5, 0.6)
    # income_cap = 1476.84
  ),
  
  couple_children = set_situation(
    label = "In a couple, with children",
    max_payment = 507.9,
    thresholds = c(437.0, 524.0),
    rates = c(0.5, 0.6)
    # income_cap = 1310.84
  )
  
)


payment_data <- data.frame(
  income = income,
  lapply(situations, payment_received, income)
)

# stated_income_caps <- data.frame(
#   income = sapply(situations, stated_income_cap),
#   value = 0,
#   row.names = NULL
# )
# 
# calculated_income_caps <- data.frame(
#   income = sapply(situations, calc_income_cap),
#   value = 0,
#   row.names = NULL
# )


png("payment_recieved_example.png", units="in", width=6, height=5, pointsize=9, res=160)

pivot_longer(payment_data, cols = 2:last_col(), names_to = "Situation") %>%
  ggplot(aes(x = income, y = value)) +
  geom_line(size = 0.8, alpha = 0.6, aes(color = Situation)) +
  geom_vline(xintercept = c(437, 524), linetype = "dotted") +
  geom_point(data = stated_income_caps, size = 2, shape = 25, fill = "black") +
  geom_point(data = calculated_income_caps, size = 2, shape = 3, colour = "red") +
  scale_colour_brewer(
    palette = "Set1",
    labels = lapply(situations, function(s) s$label)
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.85)
  ) +
  labs(
    x = "Fortnightly Income", 
    y = "Payment Recieved", 
    title = "Payment Recieved: Austudy"
  ) +
  xlim(c(400, 550))

dev.off()



png("payment_recieved_slope.png", units="in", width=6, height=5, pointsize=9, res=160)

pivot_longer(payment_data, cols = 2:last_col(), names_to = "Situation") %>%
  ggplot(aes(x = income, y = value)) +
  geom_line(size = 0.8, alpha = 0.6, aes(color = Situation)) +
  geom_vline(xintercept = c(437, 524), linetype = "dotted") +
  scale_colour_brewer(
    palette = "Set1",
    labels = lapply(situations, function(s) s$label)
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.55, 0.4)
  ) +
  labs(
    x = "Fortnightly Income", 
    y = "Payment Recieved", 
    title = "Payment Recieved: Austudy"
  ) +
  xlim(c(400, 550))

dev.off()



png("payment_recieved_points.png", units="in", width=6, height=5, pointsize=9, res=160)

pivot_longer(payment_data, cols = 2:last_col(), names_to = "Situation") %>%
  ggplot(aes(x = income, y = value)) +
  geom_line(size = 0.8, alpha = 0.6, aes(color = Situation)) +
  geom_vline(xintercept = c(437, 524), linetype = "dotted") +
  geom_point(data = stated_income_caps, size = 2, shape = 25, fill = "black") +
  geom_point(data = calculated_income_caps, size = 2, shape = 3, colour = "red") +
  scale_colour_brewer(
    palette = "Set1",
    labels = lapply(situations, function(s) s$label)
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.8, 0.85)
  ) +
  labs(
    x = "Fortnightly Income", 
    y = "Payment Recieved", 
    title = "Payment Recieved: Austudy"
  ) +
  xlim(c(800, 1500))

dev.off()



income_cap_diff <- data.frame(
  Situation = sapply(situations, function(s) s$label),
  Calculated = sapply(situations, calc_income_cap),
  Stated = sapply(situations, stated_income_cap)
) %>%
  mutate(Difference = Stated - Calculated)



# Realistic income analysis -----------------------------------------------

robodebt_difference <- function(
  situation,
  N,
  mean,
  sd
) {
  # Simulate N fortnightly income periods and calculate Robodebt algorithm differential.
  # args:
    # situation: situation list object
    # N: number of fortnightly periods
    # mean: vector of means from which truncated Gaussian income will be simulated
    # sd: vector of standard deviations from which truncated Gaussian income will be simulated
  # value:
    # df: data frame containing simulated income and details on payment differential
  
  ## Collect parameters used to simulate N samples from a truncated Gaussian distribution
  params <- expand.grid(mean, sd)
  colnames(params) <- c("mean", "sd")
  
  ## Simulate N fornightly samples for each parameter set
  sim_income <- t(apply(params, 1, function(p) msm::rtnorm(N, mean=p[1], sd=p[2], lower=0)))
  colnames(sim_income) <- paste0("X", 1:dim(X)[2])
  
  ## Gather parameters with simulations and compute payment entitled, recieved and the difference
  df <- cbind(params, rowMeans(sim_income), sim_income) %>%
    rename("fnight_avg_income" = "rowMeans(sim_income)") %>%
    pivot_longer(cols=starts_with("X"), names_to="fortnight", values_to="sim_income") %>%
    select(fortnight, everything()) %>%
    mutate(
      fortnight = as.numeric(gsub("[^0-9.-]", "", fortnight)),
      payment_entitled = payment_received(situation, fnight_avg_income),
      payment_received = payment_received(situation, sim_income),
      difference = payment_entitled - payment_received
    )
  
  return(df)
}

N <- 26
mu <- seq(400, 1000, 100)
sig <- seq(100, 400, 100)

df1 <- robodebt_difference(situations$single_under18_home, N, mu, sig)
df2 <- robodebt_difference(situations$single_children, N, mu, sig)
df3 <- robodebt_difference(situations$couple_children , N, mu, sig)


df1 %>% 
  ggplot(aes(x=fortnight, y=difference, color=interaction(mean, sd))) +
  geom_point()

df1 %>%
  ggplot(aes(x=fortnight, y=difference)) +
  geom_point() +
  facet_grid(mean ~ sd)
