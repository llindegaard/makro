library(statsDK)
library(tidyverse)
library(ggplot2)
library(scales)
library(zoo)

NKN1 <- as.data.frame(sdk_retrieve_data("NKN1", lang = "dk"))
NKN1$INDHOLD <- as.numeric(gsub(",",".",NKN1$INDHOLD))
NKN1 <- NKN1[complete.cases(NKN1), ]
# tables <- statsDK::sdk_retrieve_tables()
# dplyr::glimpse(tables)
# NKN1 %>% 
#   mutate(INDHOLD = str_replace(INDHOLD, ".", "NA"))


NKN1.vaekst <- NKN1 %>%
  group_by(TRANSAKT, PRISENHED) %>% 
  arrange(c(TID), .by_group = TRUE) %>%
  mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
  # filter(TRANSAKT == "D.1 Compensation of employees" & SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government") %>%
  filter(TID > 1989) %>% filter(SÆSON == "Sæsonkorrigeret") 
# %>% filter(pct_change is.null == FALSE)
NKN1.vaekst$Kvartal <- as.Date(as.yearqtr(gsub("K", "Q",NKN1.vaekst$TID)))

graftype <- c("Samlet antal beskæftigede (1000 personer)" )
ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                         & NKN1.vaekst$PRISENHED == "Løbende priser, (mia. kr.)"
                         #    | NKN1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
                         ), ]
       , aes(x = Kvartal, y = INDHOLD)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  ggtitle ( graftype  ) + scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x=element_text(angle=60, hjust=1))


graftype <- c("Samlede præsterede timer (mio. timer)" )
prisenhed <- c("Løbende priser, (mia. kr.)")
ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                         & NKN1.vaekst$PRISENHED == prisenhed
                         #    | NKN1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
), ]
, aes(x = Kvartal, y = INDHOLD)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = "DSt")  + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


graftype <- c("P.31 Privatforbrug" )
prisenhed <- c("Realvækst i forhold til foregående periode (pct.)")
ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                         & NKN1.vaekst$PRISENHED == prisenhed), ]
       , aes(x = Kvartal, y = INDHOLD)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = "DSt") + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x=element_text(angle = 60, hjust = 1))
