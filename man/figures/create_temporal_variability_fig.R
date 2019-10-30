library(tidyverse)
library(ecat)

d <- tibble::tibble(id = rep(809089L,12),
                    lon = rep(-84.69127387,12),
                    lat = rep(39.24710734,12),
                    start_date = seq.Date(as.Date("2010-11-08"), as.Date("2011-10-08"), by="month"),
                    end_date = seq.Date(as.Date("2010-12-08"), as.Date("2011-11-08"), by="month"))


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
