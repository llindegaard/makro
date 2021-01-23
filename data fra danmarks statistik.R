# install.packages("statsDK")
# install.packages("tidyverse")

# install.packages("devtools")
# devtools::install_github("mikkelkrogsholm/statsDK")

library(statsDK)
library(tidyverse)
nat <- sdk_retrieve_data("NAHO2")


# nat %>% 
#   mutate(INDHOLD = str_replace(INDHOLD, "..", "NA"))
nat$INDHOLD <- as.numeric(nat$INDHOLD)


vaekst <- nat %>%
  group_by(TRANSAKT,PRISENHED) %>% 
  arrange(c(TID), .by_group = TRUE) %>%
  mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100)

# 
# GDP <- as.data.frame(nat[which(nat$TRANSAKT == "B.1*g Gross domestic product" & nat$PRISENHED == "2010-prices, chained values"), ])
# # GDP <- as.data.frame(GDP)
# GDP$LAG_GDP <- lag(GDP$INDHOLD)
# GDP <- GDP[-0, ]
# GDP <- GDP[-which(is.null(GDP$LAG_GDP)), ]
# GDP$VAEKST <- GDP$INDHOLD/GDP$LAG_GDP - 1


ggplot(subset(vaekst,TRANSAKT == "B.1*g Gross domestic product" & PRISENHED == "2010-prices, chained values")
    , aes(x= as.integer(TID), y = pct_change)) + 
  geom_line()

ggplot(subset(vaekst,TRANSAKT == "D.1 Compensation of employees" & PRISENHED == "Current prices")
       , aes(x= as.integer(TID), y = pct_change)) + 
  geom_line()


pris112 <- sdk_retrieve_data("PRIS112")
folk2 <- sdk_retrieve_data("FOLK2", Tid = "1980")
statsDK::sdk_retrieve_data("FOLK2", Tid = "1980")



ggplot(subset(pris112,HOVED == "Annual increase"), aes(x= as.integer(TID), y = as.numeric(INDHOLD))) + 
  geom_line()
 
glimpse(folk2)

folk2 <- 
  folk2 %>%  
  group_by(KØN, HERKOMST, TID) %>% 
  summarise(INDHOLD = sum(INDHOLD))

head(folk2)

ggplot(data = folk2, aes(x = factor(TID), y = INDHOLD, fill = factor(KØN))) +
  geom_col(position = "dodge") + # position = "dodge" angiver at søjerne skal stå ved siden af hinanden.
  facet_wrap(~ HERKOMST, scale = "free_y", ncol = 2) 