# https://osf.io/c87sm/

# to install muliilevelmediation:
install.packages("devtools")
devtools::install_github("falkcarl/multilevelmediation")

# Load necessary libraries
library(multilevelmediation)
library(boot)

# set random number seed
set.seed(1234)

# Data are from Bauer, Preacher, & Gil (2006)
# For a reference, see help(BPG06dat)
help(BPG06dat)
??BPG06dat
data(BPG06dat)
View(BPG06dat)

fit <- modmed.mlm(BPG06dat,
    L2ID = "id", X = "x", Y = "y", M = "m",
    random.a = TRUE, random.b = TRUE, random.cprime = TRUE,
    control = list(opt = "nlm")
)

#returns all the estimates (fized and random effects) from the model
extract.modmed.mlm(fit)

#returns the indirect effect estimate
extract.modmed.mlm(fit, type = "indirect")

#returns the 'a’ path estimate
extract.modmed.mlm(fit, type = "a")

#returns the 'b’ path estimate
extract.modmed.mlm(fit, type = "b")

#returns the covariance between the random effects for...
## the 'a' and 'b' paths
## (returns nothing if either random.a or random.b are FALSE)
extract.modmed.mlm(fit, type = "covab")

summary(fit$model)


extract.modmed.mlm(fit)


fit_noranda <- modmed.mlm(BPG06dat,
    L2ID = "id", X = "x", Y = "y", M = "m",
    random.a = FALSE, random.b = TRUE, random.cprime = TRUE,
    control = list(opt = "nlm")
)

dat_restruct <- stack_bpg(BPG06dat, L2ID = "id", X = "x", Y = "y", M = "m")

# take a look at the restructured data
head(dat_restruct)


# relevant variables are internally renamed but could be checked against
# the original data
head(BPG06dat)
