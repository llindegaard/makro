library(statsDK)
library(tidyverse)
library(ggplot2)
library(scales)
library(zoo)
dsttabel <- c("TVANG2")
TVANG2 <- as.data.frame(sdk_retrieve_data(dsttabel, lang = "dk"))
TVANG2$INDHOLD <- as.numeric(gsub(",",".",TVANG2$INDHOLD))
TVANG2 <- TVANG2[complete.cases(TVANG2), ]
# tables <- statsDK::sdk_retrieve_tables()
# dplyr::glimpse(tables)

TVANG2.vaekst <- TVANG2 %>%
  group_by(TYPE) %>% 
  arrange(c(TID), .by_group = TRUE) %>%
  mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) 
  # filter(TRANSAKT == "D.1 Compensation of employees" & SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government") %>%
# %>% filter(TID > "1989K4") 
# %>% filter(SÆSON == "Sæsonkorrigeret") 
# %>% filter(pct_change is.null == FALSE)
TVANG2.vaekst$Kvartal <- as.Date(as.yearqtr(gsub("K", "Q",TVANG2.vaekst$TID)))
TVANG2.vaekst <-TVANG2.vaekst %>% filter(Kvartal > "1989-12-31") 

graftype <- c("Enfamiliehuse" )
prisenhed <- c("Tvangsauktioner")
figur <- ggplot(TVANG2.vaekst[which(TVANG2.vaekst$TYPE == graftype 
                         # & TVANG2.vaekst$PRISENHED == prisenhed
                         ), ]
       , aes(x = Kvartal, y = INDHOLD)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt tabel ", dsttabel))  + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 800, width = 1200)
figur
dev.off()

graftype <- c("Samlede præsterede timer (mio. timer)" )
prisenhed <- c("Løbende priser, (mia. kr.)")
figur <- ggplot(TVANG2.vaekst[which(TVANG2.vaekst$TRANSAKT == graftype 
                         & TVANG2.vaekst$PRISENHED == prisenhed), ]
  , aes(x = Kvartal, y = INDHOLD)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt tabel ", dsttabel))  + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 800, width = 1200)
figur
dev.off()

graftype <- c("P.31 Privatforbrug")
prisenhed <- c("2010-priser, kædede værdier, (mia. kr.)")
figur <- ggplot(TVANG2.vaekst[which(TVANG2.vaekst$TRANSAKT == graftype 
                         & TVANG2.vaekst$PRISENHED == prisenhed), ]
       , aes(x = Kvartal, y = INDHOLD)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt tabel ", dsttabel)) + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x=element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 800, width = 1200)
figur
dev.off()

graftype <- c("P.31 Privatforbrug" )
prisenhed <- c("Realvækst i forhold til foregående periode (pct.)")
figur <- ggplot(TVANG2.vaekst[which(TVANG2.vaekst$TRANSAKT == graftype 
                         & TVANG2.vaekst$PRISENHED == prisenhed), ]
       , aes(x = Kvartal, y = INDHOLD)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt tabel ", dsttabel)) + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 800, width = 1200)
figur
dev.off()




# graftype <- c("D.1 Compensation of employees" )
# ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
#                        & (NKN1.vaekst$SEKTOR == "S.1 Total economy"  
#                        | NKN1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
#                  )), ]
#        , aes(x= TID, y = pct_change, color = SEKTOR)) + 
#   geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
#     ggtitle ( graftype  )
# 
# graftype <- c("B.1GF Gross domestic product at factor cost" )
# ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
#                        & (NKN1.vaekst$SEKTOR == "S.1 Total economy"  
#                           | NKN1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government"
#                        )), ]
#        , aes(x= TID, y = pct_change, color = SEKTOR)) + 
#   geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
#   ggtitle ( graftype  )

