#power analysis example

library("TOSTER")

## Sample size for alpha = 0.05, 90% power, equivalence bounds of
## Cohen's d = -0.4 and Cohen's d = 0.4, assuming true effect = 0
powerTOSTtwo(alpha=0.05, statistical_power=0.9, low_eqbound_d=-0.4, high_eqbound_d=0.4)
