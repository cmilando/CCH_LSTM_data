library(dlnm)
library(splines)
library(tidyverse)

rr <- chicagoNMMAPS

# rr %>% filter(year == 1987) %>% select(temp)
#
# start = 1987, end = 1988
#
# x <- ts(rr %>% filter(year == 1987) %>% select(temp),
#         start = c(1987, 1), end = c(1988, 1), frequency = 365)
#
# m <- decompose(x)
# plot(m)
# length(co2)
# row.names(co2)
# m <- decompose(co2)
# m$figure
# plot(m)
#
# tt <- decompose(ts(rr$temp))
#
# head(rr)
#
# t(days, sin_wave, type="l", col="blue", xlab="Days", ylab="Sine Wave Value", main="Sine Wave Repeating Every 7 Days")
#
# ##
# rr$DOW_sin <- sin(2 * pi * (1:nrow(rr) / 7))
# rr$MONTH_sin <- sin(2 * pi * (1:nrow(rr) / 30.4))
# plot(rr$DOW_sin[1:14])
# plot(rr$MONTH_sin[1:365], type = 'l')

# for each year, get the average value of that day-of-week / month from the previous year
rr <- rr %>% mutate(stratum = paste0(year, ".", month, ".", dow),
                    stratum_cheat = paste0(year+1, ".", month, ".", dow))

stratum_avg <- rr %>%
  group_by(stratum, stratum_cheat) %>%
  summarize(
    .groups = 'keep',
    avg_death = mean(death)
  )

head(stratum_avg)

stratum_avg$stratum <- stratum_avg$stratum_cheat
stratum_avg$stratum_cheat <- NULL

head(stratum_avg)

rr <- rr %>% filter(year >= 1988) %>% left_join(stratum_avg)

rr_simple <- rr[, c('date', 'death', 'avg_death', 'temp',
                    'dptp', 'rhum', 'pm10', 'o3')]

head(rr_simple)

write.csv(rr_simple, "rr_simple.csv", quote = F, row.names = F)


ex <- read.csv("https://raw.githubusercontent.com/SusmitSekharBhakta/Stock-market-price-prediction/main/final_data_adj.csv")

head(ex)

##
## DOW
##
