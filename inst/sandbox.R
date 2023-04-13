# for messing around

# How many unique process codes at depth 3 are there more or less?
con <- connect_to_edgar()

dt <- get_emissions_data(con, substances = "CO2", countries = "ITA", years = 2018)

# add some practice uncertainty values
dt[!is.na(Y_2018), unc_min := runif(.N)*100]
dt[!is.na(Y_2018), unc_max := runif(.N)*100]

# add col to group by (sector)
dt[, sector := substr(ad_code, 1, 3)]

length(unique(dt$ad_code))
# here we get 403...

# some emissions data (data.table)
emi <- emi_CO2

# in order to calculate uncertainties we have to match this with the uncertainty table
# Since uncertainty table is at depth 3, we convert

emi[ , Process3 := shrink_process_codes(Process.code)]

# do we have all these codes in our table?
emi_codes <- unique(dt$ad_code)
uin_codes <- unique(uIN$Process)

# NOPE!

all(uin_codes %in% emi_codes)

################

X <- as.data.table(mtcars)

function

# # the answer I want
# X[, .(mean1 = mean(mpg), mean2 = sum(hp)), by = cyl]
#
# # trying to get the same but want to specify the columns to take mean of,
# # AND the by argument, as variables:
# mean1_col <- "mpg"
# mean2_col <- "hp"
# mean_cols <- c(mean1_col, mean2_col)
# by_col <- "cyl"
#
# # I can do the by part...
# X[, .(mean1 = mean(mpg), mean2 = sum(hp)), by = c(by_col)]
#
# # if I only want to summarise one
# X[, lapply(.SD, sum), by = c(by_col), .SDcols = mean_cols]

# # ok this works, but what if the columns to operate on are strings?
# group_col <- "cyl"
# sum_col <- "mpg"
#
# X[, .(mpgsum = mean(sum_col), t1 = sum(mpg)), by = c(group_col)]
#
# X[, .(mpgsum = lapply(.SD, sum)), by = c(group_col), .SDcols = sum_col]
