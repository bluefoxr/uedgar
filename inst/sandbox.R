# for messing around

# some emissions data (data.table)
emi <- emi_CO2

# in order to calculate uncertainties we have to match this with the uncertainty table
# Since uncertainty table is at depth 3, we convert

emi[ , Process3 := shrink_process_codes(Process.code)]

# do we have all these codes in our table?
emi_codes <- unique(emi$Process3)
uin_codes <- unique(uIN$Process)

# NOPE!

all(uin_codes %in% emi_codes)
