
require(covidregionaldata)
require(data.table)

.debug <- "."
.args <- if (interactive()) c(
  file.path(.debug, "data", "traveller_cases.rds"),
  file.path(.debug, "data", "country_data.rds")
)

traveller_cases <- readRDS(.args[1])
traveller_cases[, country := "India" ]

date_pick <- as.Date("2021-02-01")
res <- as.data.table(
  get_national_data(c("India", "United Kingdom"))
)[date > date_pick,
  .(country, date, cases_new)
]
setkey(res, country, date)

res[, ma_cases := frollmean(cases_new, 7, align = "center"), by=country ]
res[, seq := NA_integer_ ]

res[
  traveller_cases[,.(date, country, Number_B_1_617_2)],
  seq := Number_B_1_617_2,
  on=.(date, country)
]

red_list <- as.Date("2021-04-23")
india_tot_pre_red <- res[country == "India" & date < red_list, sum(cases_new)]

india_red_list <- as.Date("2021-04-23")
date_pick <- as.Date("2021-02-01")
date_uk_fit <- as.Date("2021-04-23")

all_india <- all_countries %>% filter(country == "India", date>date_pick)
all_uk <- all_countries %>% filter(country == "United Kingdom",date>date_pick)

data_proportion <- head(data_proportion,-2) # Remove last 1 days
#data_proportion$long_dates <- as.Date(data_proportion$long_dates,origin="1970-01-01")


# Importation data

# Imported cases
traveller_cases0 <- traveller_cases0 %>% filter(`Travel Indicator` == "Traveller")
traveller_cases <- traveller_cases0 #head(traveller_cases0,-5)

daily_india_seq <- 0*all_india$cases_new # Imports based on traveller cases
daily_india_seq[match(traveller_cases0$Date,all_india$date)] <- traveller_cases0$Number_B_1_617_2


# Scale imports
south_asia_imports <- 1000 # Assumed initial normalisation
total_cases_india <- all_india %>% filter(country == "India", date<as.Date("2021-04-23")) %>% select(cases_new) %>% sum()
travel_multiplier <- south_asia_imports/total_cases_india

all_india <- all_india %>% mutate(daily_imports = travel_multiplier*cases_new)

# Downweight recent imports based on incubation period (from McAloon et al. 2020)
red_list_point <- as.numeric(india_red_list-date_pick)
total_days <- length(all_india$date)
downweight_imports <- c(rep(1,red_list_point),1-plnorm(1:(total_days-red_list_point),mean=log(5.1),sd=log(1.65)) )

# Format data
ma_India_variant0 <- ma(data_india$B.1.617.2,7) # Moving average of UK cases
l_IV <- length(ma_India_variant0)
final_value <- 1 #tail(ma_India_variant0,4)[1] #1 #
ma_India_variant <- c(ma_India_variant0[1:(l_IV-4)],rep(final_value,total_days-l_IV+4))
ma_India_variant[1:3] <- 0

# Moving average and scaling factors
ma_UK_cases <- ma(all_uk$cases_new,7) # Moving average of UK cases

data_fit <- data_proportion[match(all_uk$date,data_proportion$sample_date),]
t_fit <- nrow(data_fit)

# Set up for fitting
all_uk_fit <- all_uk %>% filter(date<date_uk_fit)
total_days_uk <- length(all_uk_fit$date)
ma_UK_cases_fit <- ma(all_uk_fit$cases_new,7)


# Define parameters
# serial interval from Rai et al: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7448781/
theta_f <- list(serial_mean=log(5.4), serial_sd=log(1.5))
#' TODO magic number 20?
tful <- 20
tfdl <- with(theta_f, dlnorm(0:tful, serial_mean, serial_sd))

#' TODO can be cleverer w/ construction here:
#' (tfdlnorm) ... all (sz) 0 ...
#' 0 (tfdlnorm) sz-1 0s
#' 0 0 (tfdlnorm) sz-2 0s
#' ...
#' ... all 0s ... (tfdlnorm)
#' should be able to use rep
gen_serial_matrix <- function(
  sz, tfd = tfdl, tfdsz = tful
) {
  sermat <- matrix(0, nrow=sz, ncol=(sz+tfdsz+1))
  for(ii in 1:sz){
    sermat[ii, ii+(0:tfdsz)] <- tfd
  }
  sermat[, 1:sz]
}

# Construct table of serial intervals
t_max <- total_days
serial_mat0 <- gen_serial_matrix(total_days)

#Estimates from PHE report (Table 9)
r_phe_report <- c(travel = 128/(250*0.724),non_travel = 98/(287*0.805))


