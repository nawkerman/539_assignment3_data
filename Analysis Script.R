library(tidyverse)
library(gapminder)
library(ggplot2)
library(dplyr)
library(broom)

climate_spending <- read_csv("climate_spending.csv")
energy_spending <- read_csv("energy_spending.csv")
fed_r_d_spending <- read_csv("fed_r_d_spending.csv")



energy_by_year <- ggplot(energy_spending, aes(x=year, y=energy_spending, 
                                              color=department))
ordered_legend <- filter(energy_spending, year == max(year)) %>%
  arrange(desc(energy_spending)) %>% pull(department)
energy_by_year + geom_point() + geom_smooth(method = "lm") +
  scale_color_discrete(breaks = ordered_legend, name = "Department") + 
  ylab("Spending") 



regression_energy = do(by_department, 
   glance(lm(energy_spending ~ year, data = .)))

regression_climate$r.squared[which(regression_climate[1] == "NASA")]





