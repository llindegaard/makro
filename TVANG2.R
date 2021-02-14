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
TVANG2.vaekst <- TVANG2.vaekst %>% filter(Kvartal > "1989-12-31") 

graftype <- c("Enfamiliehuse", "Ejerlejligheder", "Sommerhuse")
prisenhed <- c("Tvangsauktioner")
figur <- ggplot(TVANG2.vaekst[which(TVANG2.vaekst$TYPE %in% graftype), ]
       , aes(x = Kvartal, y = INDHOLD, color = TYPE)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt tabel ", dsttabel))  + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype, "3",prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

graftype <- c("Ejerlejligheder", "Sommerhuse")
prisenhed <- c("Tvangsauktioner")
figur <- ggplot(TVANG2.vaekst[which(TVANG2.vaekst$TYPE %in% graftype), ]
                , aes(x = Kvartal, y = INDHOLD, color = TYPE)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt tabel ", dsttabel))  + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype, "2", prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

graftype <- c("Enfamiliehuse")
prisenhed <- c("Tvangsauktioner")
figur <- ggplot(TVANG2.vaekst[which(TVANG2.vaekst$TYPE %in% graftype), ]
                , aes(x = Kvartal, y = INDHOLD, color = TYPE)) + 
  geom_line()  + theme(legend.position = "bottom" ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt tabel ", dsttabel))  + 
  scale_x_date(date_breaks = "12 month", date_labels =  "%y") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype, "1", prisenhed,".png"), height = 450, width = 600)
figur
dev.off()