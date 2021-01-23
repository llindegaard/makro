library(statsDK)
library(tidyverse)
nanasl1 <- sdk_retrieve_data("NASL1")


nanasl1 %>% 
  mutate(INDHOLD = str_replace(INDHOLD, ".", "NA"))
nanasl1$INDHOLD <- as.numeric(nanasl1$INDHOLD)
<<<<<<< HEAD
# vv
=======

>>>>>>> 9f62c77b0a714376423e42bfecdeb993eeb12c55

l1.vaekst <- nanasl1 %>%
  group_by(TRANSAKT, SEKTOR) %>% 
  arrange(c(TID), .by_group = TRUE) %>%
  mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
  # filter(TRANSAKT == "D.1 Compensation of employees" & SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government") %>%
  filter(TID > 1989)
 
distinct(l1.vaekst$TRANSAKT)
l1.vaekst <- as.data.frame(l1.vaekst)

graftype <- c("D.1 Compensation of employees" )
ggplot(l1.vaekst[which(l1.vaekst$TRANSAKT == graftype 
                       & (l1.vaekst$SEKTOR == "S.1 Total economy"  
                       | l1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
                 )), ]
       , aes(x= TID, y = pct_change, color = SEKTOR)) + 
  geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
    ggtitle ( graftype  )

graftype <- c("B.1GF Gross domestic product at factor cost" )
ggplot(l1.vaekst[which(l1.vaekst$TRANSAKT == graftype 
                       & (l1.vaekst$SEKTOR == "S.1 Total economy"  
                          | l1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
                       )), ]
       , aes(x= TID, y = pct_change, color = SEKTOR)) + 
  geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
  ggtitle ( graftype  )