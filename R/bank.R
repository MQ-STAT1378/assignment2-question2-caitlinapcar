# to generate 100 customer's arrival time and their service times
set.seed(2048)
# customers arrived according to a Poisson Process
# = inter arrival time has an exponential distribution
# Set arrival rate to one per 60 seconds = mean inter-arrival time is 60s
arrival_time <- cumsum(rexp(100, 1/60))
# service time is exponential distributed with mean 150s + 20s greeting
service_time <- rexp(length(arrival_time), 1/150) + 20
bank <- data.frame(arrival_time, service_time)
usethis::use_data(bank, overwrite = TRUE)

