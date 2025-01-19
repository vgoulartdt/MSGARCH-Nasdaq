library(MSGARCH)
library(quantmod)
library(ggplot2)


## Fetch NASDAQ data
getSymbols("^IXIC", from = "1980-01-01", to = "2025-01-01")
get_nasdaq <- IXIC
nasdaq_price <- get_nasdaq$IXIC.Adjusted

## Calculate log-returns
nasdaq <- diff(log(nasdaq_price))
nasdaq <- nasdaq[-1]


## Plot NASDAQ log-returns
data_nasdaq <- data.frame(
  Date = index(nasdaq),            # Dates of the series
  Returns = coredata(nasdaq)       # Logarithmic returns
)

ggplot(data_nasdaq, aes(x = Date, y = IXIC.Adjusted)) +
  geom_line(color = "black", size = 0.5) +  # Line with the returns
  scale_x_date(
    date_breaks = "1 year",                 # X-axis division by year
    date_labels = "%Y"                     # Display only the year
  ) +
  labs(
    title = "NASDAQ Log-Returns (%)",
    x = "Date (year)",
    y = "Log-Returns"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # Rotation of X-axis labels
  )


## Maximum Likelihood Estimation (ML)

## Define the MSGARCH model specification
egarch <- CreateSpec(variance.spec = list(model = "eGARCH"),
                        distribution.spec = list(distribution = "sstd"),
                        switch.spec = list(K = 2))

## Fit the model to the NASDAQ log-returns
fit.ml <- FitML(egarch, data = nasdaq)

summary(fit.ml)



## Unconditional Volatility
set.seed(1234)                                      # Set seed for reproducibility
sqrt(252) * sapply(ExtractStateFit(fit.ml), UncVol) # Calculate unconditional volatility for each state
# The ExtractStateFit function creates a fitted object for each state
# The UncVol function calculates the unconditional volatility
vol <- sqrt(252) * Volatility(fit.ml)
head(vol)
options(max.print=30000)
vol



## Estimated Smoothed Probabilities of States
state_probs <- State(fit.ml, type = "smoothed")$SmoothProb

# Graphics
data_fig2 <- data.frame(
  Date = as.Date(dimnames(state_probs)[[1]]),  # Extract the dates from dimension 1
  State1_Prob = state_probs[, 1, 1],          # Probability of state 1 (k=1)
  State2_Prob = state_probs[, 1, 2]           # Probability of state 2 (k=2)
)

ggplot(data_fig2, aes(x = Date)) +
  geom_line(aes(y = State1_Prob, color = "State 1")) +
  geom_line(aes(y = State2_Prob, color = "State 2"), linetype = "dashed") +
  labs(
    title = "Smoothed Probabilities of States (NASDAQ Data)",
    x = "Date",
    y = "Smoothed Probability",
    color = "States"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )



## With Filtered Probabilities of States
filtered_probs <- State(fit.ml, type = "filtered")$FiltProb

# Graphics
data_filtered <- data.frame(
  Date = as.Date(dimnames(filtered_probs)[[1]]),  # Datas
  State1_Prob = filtered_probs[, 1, 1],          # Probabilidade Estado 1
  State2_Prob = filtered_probs[, 1, 2]           # Probabilidade Estado 2
)

ggplot(data_filtered, aes(x = Date)) +
  geom_line(aes(y = State1_Prob, color = "State 1")) +
  geom_line(aes(y = State2_Prob, color = "State 2"), linetype = "dashed") +
  labs(
    title = "Filtered Probabilities of States (NASDAQ Weekly Data)",
    x = "Date",
    y = "Filtered Probability",
    color = "States"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )


############### Graphics for conditional volatility

## Calculate the conditional volatility (annualized) for daily returns
volatility_conditional <- sqrt(252) * Volatility(fit.ml)

# Graphics
data_volatility <- data.frame(
  Date = index(nasdaq),
  Volatility = as.numeric(volatility_conditional)
)

ggplot(data_volatility, aes(x = Date, y = Volatility)) +
  geom_line(color = "blue", size = 0.8) +
  labs(
    title = "Conditional Volatility (Weekly Data, %)",
    x = "Date",
    y = "Volatility (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
