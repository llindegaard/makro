library(statsDK)
library(tidyverse)
NAHB <- sdk_retrieve_data("NAHB")


# nan1 %>% 
#   mutate(INDHOLD = str_replace(INDHOLD, ".", "NA"))

nan1$INDHOLD <- as.numeric(nan1$INDHOLD)

nan1.vaekst <- nan1 %>%
  group_by(TRANSAKT, SEKTOR) %>% 
  arrange(c(TID), .by_group = TRUE) %>%
  mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
  # filter(TRANSAKT == "D.1 Compensation of employees" & SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government") %>%
  filter(TID > 1989)
 
# distinct(nan1.vaekst $TRANSAKT)
nan1.vaekst <- as.data.frame(nan1.vaekst)

graftype <- c("D.1 Compensation of employees" )
ggplot(nan1.vaekst [which(nan1.vaekst $TRANSAKT == graftype 
                       & (nan1.vaekst $SEKTOR == "S.1 Total economy"  
                       | nan1.vaekst $SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
                 )), ]
       , aes(x= TID, y = pct_change, color = SEKTOR)) + 
  geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
    ggtitle ( graftype  )

graftype <- c("B.1GF Gross domestic product at factor cost" )
ggplot(nan1.vaekst [which(nan1.vaekst $TRANSAKT == graftype 
                       & (nan1.vaekst $SEKTOR == "S.1 Total economy"  
                          | nan1.vaekst $SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
                       )), ]
       , aes(x= TID, y = pct_change, color = SEKTOR)) + 
  geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
  ggtitle ( graftype  )