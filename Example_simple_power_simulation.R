############################################################################
### Simple simulation-based power analysis for a linear regression model ###
############################################################################

# Set random number generator
set.seed(1234)

# Sample size
N <- 100

# Variables in the model
soc.connection     <- round(abs(rnorm(N, 2, sd=5)), digits = 2)    # degree of perceived social connection


# Conduct 1000 simulations
for(i in 1:1000) {
  
             # More wellbeing is linked to a higher degree of perceived social connection (0.4*soc.connection)
    # This effect gets stronger over time (interaction: 0.9*soc.connection*time)
             soc.connection    <- round(abs(rnorm(N, 2, sd=5)), digits = 2)    # degree of perceived social connection
             wellbeing         <- 20 + 0.4*soc.connection + rnorm(N, 0, sd = 4)
             model1            <- lm(wellbeing ~ soc.connection)
             p.value[[i]]      <- summary(model1)$coefficients["soc.connection", "Pr(>|t|)"]
             }


# Power to detect a statistically significant effect of 
# perceived social connection on wellbeing (p < .05)

significant  <- ifelse(p.value < 0.05, 1, 0)   # If p-value is < 0.05 recode as 1 ("significant"),
                                               # otherwise as 0 ("not significant")


# Compute power
((sum(significant))/1000)*100 # Amount of significant results divided by number of simulations
                              # multiplied by 100 to obtain percentages

# 86.8 % power


