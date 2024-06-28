
# Comparative Gini calculations
library(DescTools)
library(ineq)
library(REAT)
# Here is a simple distribution of crime across neighborhoods

crimes = c(40, 30, 20, 10)
crimes = rpois(200, 20)

# Here are two one possible population distributions (weights)
pop_1 <- c(25, 25, 25, 25)
pop_2 <- c(40, 40, 15, 05)


# DescTools, REAT and ineq agree about what they refer to as "unbiased", 
#    "standardized" and "finite sample correction" respectively.

DescTools::Gini(crimes, unbiased = TRUE)
# [1] 0.3333333
DescTools::Gini(crimes, unbiased = FALSE)
# [1] 0.25

REAT::gini(x = crimes, coefnorm = TRUE)
# [1] 0.3333333
REAT::gini(x = crimes, coefnorm = FALSE)
# [1] 0.25

REAT::gini2(x = crimes, coefnorm = TRUE)
# [1] 0.3333333
REAT::gini2(x = crimes, coefnorm = FALSE)
# [1] 0.25


ineq::Gini(crimes, corr = TRUE)
# [1] 0.3333333
ineq::Gini(crimes, corr = FALSE)
# [1] 0.25

# Population-correction
DescTools::Gini(crimes, weights = pop_2, unbiased = TRUE)
# [1] 0.2168908
REAT::gini(x = crimes, weighting = pop_2, coefnorm = TRUE)
# [1] 0.3225225
REAT::gini2(x = crimes, weighting = pop_2, coefnorm = TRUE)
# [1] 0.189418

DescTools::Gini(crimes, weights = pop_2, unbiased = FALSE)

REAT::gini(x = crimes, weighting = pop_2, coefnorm = FALSE)
# [1] 0.2418919
REAT::gini2(x = crimes, weighting = pop_2, coefnorm = FALSE)
# [1] 0.1420635

DescTools::Gini(crimes, pop_1)
# [1] 0.3333333


DescTools::Gini(crimes, pop_2)
# [1] 0.2168908

Gini(crimes * pop_2)
# [1] 0.5873016

DescTools::Gini(crimes / pop_2)
# [1] 0.2677596


DescTools::Gini(crimes, pop_1, unbiased = FALSE)
# [1] 0.25


DescTools::Gini(crimes, pop_2, unbiased = FALSE)
# [1] 0.1420635


Gini::gini(pop_1, crimes)
# [1] 0.25
Gini::gini(pop_2, crimes)
# [1] 0.145

DescTools::Gini(crimes, pop_1, unbiased = FALSE)
# [1] 0.25
Gini::gini(pop_2, crimes)
# [1] 0.145
DescTools::Gini(crimes, pop_2, unbiased = FALSE)
# [1] 0.1420635

REAT::gini(x = crimes, weighting = pop_1)
# [1] 0.25
REAT::gini(x = crimes, weighting = pop_2)
# [1] 0.2418919

REAT::gini(x = crimes, coefnorm = TRUE, weighting = pop_1)
# [1] 0.3333333
REAT::gini(x = crimes, coefnorm = TRUE, weighting = pop_2)
# [1] 0.3225225


ineq::Gini(crimes)

