cb1.pm <- crossbasis(chicagoNMMAPS$pm10, lag=15, argvar=list(fun="lin"),
                       arglag=list(fun="poly",degree=4))

cb1.temp <- crossbasis(chicagoNMMAPS$temp, lag=3, argvar=list(df=5),
                         arglag=list(fun="strata",breaks=1))


library(splines)
model1 <- glm(death ~ cb1.pm + cb1.temp + ns(time, 7*14) + dow,
                family=quasipoisson(), chicagoNMMAPS)


ci.level = 0.95
z <- qnorm(1 - (1 - ci.level)/2)
pred_m <- predict(object = model1, newdata = chicagoNMMAPS, se.fit = T)
exp_est <- exp(pred_m$fit)
exp_ub <- exp(pred_m$fit + z * pred_m$se.fit)
exp_lb <- exp(pred_m$fit - z * pred_m$se.fit)

fit_df <- data.frame(exp_est, exp_ub, exp_lb)

obs_comb_pres <- cbind(chicagoNMMAPS, fit_df)

col1 = 'lightblue'
col2 = 'blue'

ggplot(obs_comb_pres %>% filter(year == 1995)) +
  #theme_pubr() + #coord_cartesian(ylim = YLIM) +
  scale_fill_manual(values = 'lightblue') +
  scale_color_manual(values = 'blue') +
  geom_line(aes(x = date, y = death), color = grey(0.15), linetype = '11') +
  geom_ribbon(aes(x = date, ymin = exp_lb, ymax = exp_ub),
              alpha = 0.5) +
  geom_line(aes(x = date, y = exp_est), show.legend = T)

+
  xlab(NULL) + ylab("Daily ED visits per 100k") +
  coord_cartesian(ylim = c(80, 125)) +
  theme(legend.position.inside = c(0.15, 0.75),
        legend.position = 'inside',
        legend.title = element_blank()) +
  annotate(geom = 'text', x = as.Date('1996-06-01'),
           y = 125, fontface = 'bold',
           label = 'b. Use model to estimate time-series of ED visits in the present',
           hjust = 0,
           family = ff)
