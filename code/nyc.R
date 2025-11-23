# nyc.R
# 11/23/2025
# Analysis of NYC building data

packages <- c("data.table", "lubridate", "ggplot2")
lapply(packages, library, character.only = TRUE)

#### Data cleaning ####
nyc <- data.table(read.csv('data/DOB_Permit_Issuance_20251123.csv'))

x <- tail(nyc, 1000)

nyc[, `:=`(Filing.Date = as.Date(Filing.Date, format = "%m/%d/%Y"),
           Issuance.Date = as.Date(Issuance.Date, format = "%m/%d/%Y"),
           Expiration.Date = as.Date(Expiration.Date, format = "%m/%d/%Y"),
           Job.Start.Date = as.Date(Job.Start.Date, format = "%m/%d/%Y"),
           Residential = ifelse(Residential=='YES', T, F))]

#### EDA ####
nyc[!is.na(Filing.Date), .(min(Job.Start.Date), max(Job.Start.Date))]
sapply(nyc[, c("Filing.Date", 
            "Issuance.Date",
            "Expiration.Date",
            "Job.Start.Date")], function(x) sum(is.na(x)))

nyc_by_yr <- nyc[, .N, .(date = as.Date(paste0(year(Filing.Date),
                                               '-',
                                               month = month(Filing.Date),
                                               '-01')))]
ggplot(nyc_by_yr, aes(date, N)) + geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(0,25000))

project_length <- nyc[, .(
  first_date = min(Filing.Date, na.rm = TRUE),
  last_date  = max(Filing.Date, na.rm = TRUE),
  days_between = as.numeric(max(Filing.Date, na.rm = TRUE) - min(Filing.Date, na.rm = TRUE))
), by = 'Job..']

ggplot(project_length,
       aes(days_between)) +
  geom_histogram()

#### New construction ####
nyc_nb <- nyc[Job.Type == 'NB']
nyc_nb_by_yr <- nyc_nb[Permit.Sequence.. == 1, .N, .(date = as.Date(paste0(year(Filing.Date),
                                               '-',
                                               month = month(Filing.Date),
                                               '-01')))]
ggplot(nyc_nb_by_yr, aes(date, N)) + geom_line() + 
  geom_point() +
  scale_y_continuous(limits = c(0,2000))





