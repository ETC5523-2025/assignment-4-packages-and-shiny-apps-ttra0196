# data-raw/se_aus_fwi.R

# Simulate small dataset of Fire Weather Index and MSR values
set.seed(123)
year <- 1979:2020
se_aus_fwi <- data.frame(
  year = year,
  fwi  = 30 + 0.3 * (year - 1979) + rnorm(length(year), 0, 2),
  msr  =  8 + 0.2 * (year - 1979) + rnorm(length(year), 0, 1)
)

# Save to data/ folder in the package
usethis::use_data(se_aus_fwi, overwrite = TRUE)
