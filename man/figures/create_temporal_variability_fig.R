library(tidyverse)
library(ecat)

d <- tibble::tibble(id = rep(809089L,12),
                    lon = rep(-84.703717,12),
                    lat = rep(39.035665,12),
                    start_date = seq.Date(as.Date("2003-06-03"), as.Date("2004-05-03"), by="month"),
                    end_date = seq.Date(as.Date("2003-07-03"), as.Date("2004-06-03"), by="month"))


d %>%
  mutate(unscaled = calculate_ecat(. , return.LU.vars = FALSE),
         scaling_factors = calculate_scaling_factors(.),
         scaled = unscaled * scaling_factors) %>%
  gather(unscaled, scaled, key="scaled", value="ecat") %>%
  ggplot() +
  geom_line(aes(x=start_date, y=ecat, color=scaled)) +
  theme_bw() +
  labs(y = "ECAT (ug/m3)", color="") +
  theme(axis.title.x = element_blank())
ggsave("figs/temporal_variability.png", height=3, width=5)
