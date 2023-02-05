install.packages('plotly')
install.packages('hrbrthemes')
install.packages('vctrs')
install.packages('rTools')

install.packages('ggplot2')
install.packages('lubridate')

install.packages('dplyr')
install.packages('tidyverse')


library(readr)
library(vctrs)

library(tidyverse)
library(dplyr)

library(ggplot2)
library(lubridate)

library(hrbrthemes)


Base_HousePriceIndex <- read_csv("HousePriceIndex.csv")

HousePriceIndex <- filter(Base_HousePriceIndex, level == "MSA")

HousePriceIndex <- mutate(HousePriceIndex, Date = HousePriceIndex$yr + (HousePriceIndex$period / 10) )

HousePriceIndex <- mutate(HousePriceIndex, Date = as.character(HousePriceIndex$Date))

HousePriceIndex <- mutate(HousePriceIndex, Date = yq(HousePriceIndex$Date))

choices <- unique(HousePriceIndex$place_name)












