library(statsDK)
library(tidyverse)
library(ggplot2)
# library(scales)
# library(zoo)
dsttabel <- c("NKN3")
NKN3 <- as.data.frame(sdk_retrieve_data(dsttabel, lang = "dk"))

# colnames(NKN3) <- gsub("Æ","A", colnames(NKN3)) 
NKN3$INDHOLD <- as.numeric(gsub(",",".",NKN3$INDHOLD))
NKN3$INDHOLD <- as.numeric(NKN3$INDHOLD)

# NKN3 <- NKN3[complete.cases(NKN3), ]
# tables <- statsDK::sdk_retrieve_tables()
# dplyr::glimpse(tables)

NKN3.vaekst <- NKN3 %>%
 group_by(TRANSAKT, PRISENHED) %>% 
 arrange(c(TID), .by_group = TRUE) %>%
 mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
        mutate(pct_change_aa = (INDHOLD/lag(INDHOLD, 4) - 1) * 100)  %>%
        mutate(change_aa = (INDHOLD - lag(INDHOLD, 4) ))  # %>%
        # mutate(Kvartal = as.Date(as.yearqtr(gsub("K", "Q",NKN3.vaekst$TID))))
        # filter(TID > 1970) %>% na.omit# %>% filter(complete.cases(.)) #
# NKN3.vaekst <- NKN3.vaekst %>% na.omit
# NKN3.vaekst %>% filter(complete.cases(.))

# NKN3.vaekst <- NKN3.vaekst[which(complete.cases(NKN3.vaekst)), ]
# %>% filter(pct_change is.null == FALSE)
NKN3.vaekst$Kvartal <- as.Date(as.yearqtr(gsub("K", "Q",NKN3.vaekst$TID)))

graftype <- c("B.6g Disponibel bruttoindkomst","B.6n Disponibel nettoindkomst")
prisenhed <- c("2010-priser, real værdi, (mia. kr.)")
saeson <- "Årlig vækst, kvt-kvt"
figur <- ggplot(NKN3.vaekst[which(NKN3.vaekst$TRANSAKT %in% graftype 
 & NKN3.vaekst$PRISENHED == prisenhed ), ]
 , aes(x = Kvartal, y = pct_change_aa, color = TRANSAKT)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = paste(graftype, saeson), subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()


graftype <- c("B.6g Disponibel bruttoindkomst" , "B.7g Korrigeret disponibel bruttoindkomst")
prisenhed <- c("2010-priser, real værdi, (mia. kr.)")
saeson <- "Årlig vækst, kvt-kvt"
figur <- ggplot(NKN3.vaekst[which(NKN3.vaekst$TRANSAKT %in% graftype 
                                  & NKN3.vaekst$PRISENHED == prisenhed ), ]
                , aes(x = Kvartal, y = pct_change_aa, color = TRANSAKT)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = paste(graftype, saeson), subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("B.8g Bruttoopsparing" , "B.8n Nettoopsparing")
prisenhed <- c("2010-priser, real værdi, (mia. kr.)")
saeson <- "Årlig vækst, kvt-kvt"
figur <- ggplot(NKN3.vaekst[which(NKN3.vaekst$TRANSAKT %in% graftype 
                                  & NKN3.vaekst$PRISENHED == prisenhed ), ]
                , aes(x = Kvartal, y = change_aa, color = TRANSAKT)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = paste(graftype, saeson), subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("B.8g Bruttoopsparing" , "B.8n Nettoopsparing")
prisenhed <- c("Pr. indbygger, løbende priser, (1000 kr.)")
saeson <- "Årlig vækst, kvt-kvt"
figur <- ggplot(NKN3.vaekst[which(NKN3.vaekst$TRANSAKT %in% graftype 
                                  & NKN3.vaekst$PRISENHED == prisenhed ), ]
                , aes(x = Kvartal, y = change_aa, color = TRANSAKT)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = paste(graftype, saeson), subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

