library(statsDK)
library(tidyverse)
library(ggplot2)
library(scales)
library(zoo)
dsttabel <- c("NKN1")
NKN1 <- as.data.frame(sdk_retrieve_data(dsttabel, lang = "dk"))
NKN1$INDHOLD <- as.numeric(gsub(",",".",NKN1$INDHOLD))
NKN1 <- NKN1[complete.cases(NKN1), ]
# tables <- statsDK::sdk_retrieve_tables()
# dplyr::glimpse(tables)

NKN1.vaekst <- NKN1 %>%
 group_by(TRANSAKT, PRISENHED) %>% 
 arrange(c(TID), .by_group = TRUE) %>%
 mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
 # filter(TRANSAKT == "D.1 Compensation of employees" & SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government") %>%
 filter(TID > 1989) %>% filter(SÆSON == "Sæsonkorrigeret") 
# %>% filter(pct_change is.null == FALSE)
NKN1.vaekst$Kvartal <- as.Date(as.yearqtr(gsub("K", "Q",NKN1.vaekst$TID)))

graftype <- c("Samlet antal beskæftigede (1000 personer)" )
prisenhed <- c("Løbende priser, (mia. kr.)")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

graftype <- c("Samlede præsterede timer (mio. timer)" )
prisenhed <- c("Løbende priser, (mia. kr.)")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

graftype <- c("P.31 Privatforbrug")
prisenhed <- c("2010-priser, kædede værdier, (mia. kr.)")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

graftype <- c("P.31 Privatforbrug")
prisenhed <- c("Realvækst i forhold til foregående periode (pct.)")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

