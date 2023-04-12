# Check the calculation of the input uncertainties table by Diego

library(data.table)

# this was imported from CSV
XD <- as.data.table(unc_first_table)

# the first cols, to be compared
X <- XD[ , Process:Unc_EF_max]


# Calculate the min/max of EMI uncertainty --------------------------------

X[ , Unc_EMI_min := sqrt(Unc_AD^2 + Unc_EF_min^2)]
X[ , Unc_EMI_max := sqrt(Unc_AD^2 + Unc_EF_max^2)]

# check
check_cols <- c("Unc_EMI_min", "Unc_EMI_max")
all.equal(X[, ..check_cols], XD[, ..check_cols])
# so far so good


# Cap at 230 --------------------------------------------------------------

max_unc <- 230

# First - how many uncertainties are outside this range?
X[Unc_EMI_min >= max_unc | Unc_EMI_max >= max_unc, .N]
# answer is NONE for this data

# Anyway we create the columns...
X[, Unc_EMI_min_cap230 := Unc_EMI_min][
  Unc_EMI_min_cap230 > max_unc, Unc_EMI_min_cap230 := max_unc]
X[, Unc_EMI_max_cap230 := Unc_EMI_max][
  Unc_EMI_max_cap230 > max_unc, Unc_EMI_max_cap230 := max_unc]

# check
check_cols <- c("Unc_EMI_min_cap230", "Unc_EMI_max_cap230")
all.equal(X[, ..check_cols], XD[, ..check_cols])
# still fine...


# Correction factor -------------------------------------------------------

f_correct <-  function(U) ((-0.72 + 1.0921*U - 1.63e-3*U^2 + 1.11e-5*U^3)/U)^2

# only apply to values greater than 100
X[ , cf_min := fifelse(Unc_EMI_min_cap230 > 100, f_correct(Unc_EMI_min_cap230), 1)]
X[ , cf_max := fifelse(Unc_EMI_max_cap230 > 100, f_correct(Unc_EMI_max_cap230), 1)]

# check
check_cols <- c("cf_min", "cf_max")
all.equal(X[, ..check_cols], XD[, ..check_cols])
# still good...


# Apply correction factor -------------------------------------------------

X[ , Unc_emi_min_fixed := Unc_EMI_min_cap230*cf_min]
X[ , Unc_emi_max_fixed := Unc_EMI_max_cap230*cf_max]

# check
check_cols <- c("Unc_emi_min_fixed", "Unc_emi_max_fixed")
all.equal(X[, ..check_cols], XD[, ..check_cols])

## GRAND CHECK

all.equal(X, XD)

# Everything checks out!
