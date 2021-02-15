library(statsDK)
library(tidyverse)
library(ggplot2)
library(scales)
library(zoo)
dsttabel <- c("NKN1")
NKN1 <- as.data.frame(sdk_retrieve_data(dsttabel, lang = "dk"))
colnames(NKN1) <- gsub("Æ","A", colnames(NKN1)) 
NKN1$INDHOLD <- as.numeric(gsub(",",".",NKN1$INDHOLD))
# NKN1 <- NKN1[complete.cases(NKN1), ]
# tables <- statsDK::sdk_retrieve_tables()
# dplyr::glimpse(tables)

NKN1.vaekst <- NKN1 %>%
 group_by(TRANSAKT, PRISENHED, SASON) %>% 
 arrange(c(TID), .by_group = TRUE) %>%
 mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
        mutate(pct_change_aa = (INDHOLD/lag(INDHOLD, 4) - 1) * 100) %>%
 filter(TID > 1970) %>% na.omit# %>% filter(complete.cases(.)) #
NKN1.vaekst <- NKN1.vaekst %>% na.omit
# NKN1.vaekst %>% filter(complete.cases(.))

NKN1.vaekst <- NKN1.vaekst[which(complete.cases(NKN1.vaekst)), ]
# %>% filter(pct_change is.null == FALSE)
NKN1.vaekst$Kvartal <- as.Date(as.yearqtr(gsub("K", "Q",NKN1.vaekst$TID)))

graftype <- c("Samlet antal beskæftigede (1000 personer)" )
prisenhed <- c("Løbende priser, (mia. kr.)")
saeson <- "Sæsonkorrigeret"
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed & NKN1.vaekst$SASON == saeson), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("Samlet antal beskæftigede (1000 personer)" )
prisenhed <- c("Løbende priser, (mia. kr.)")
saeson <- "Ikke sæsonkorrigeret"
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                                  & NKN1.vaekst$PRISENHED == prisenhed & NKN1.vaekst$SASON == saeson), ]
                , aes(x = Kvartal, y = pct_change_aa)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = paste("Årlig vækst i",graftype, "pr. kvartal"), subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, "årlig", ".png"), height = 540, width = 720)
figur
dev.off()


graftype <- c("P.31 Privatforbrug")
prisenhed <- c("2010-priser, kædede værdier, (mia. kr.)")
saeson <- c("Ikke sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                                  & NKN1.vaekst$PRISENHED == prisenhed 
                                  & NKN1.vaekst$SASON == saeson) ,  ]
                , aes(x = Kvartal, y = pct_change_aa)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = paste("Årlig vækst i",graftype, "pr. kvartal"), subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, "årlig", ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("P.31 Privatforbrug")
graftype <- c("Samlede præsterede timer (mio. timer)" )
prisenhed <- c("Løbende priser, (mia. kr.)")
saeson <- c("Sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed & NKN1.vaekst$SASON == saeson), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("P.31 Privatforbrug")
prisenhed <- c("2010-priser, kædede værdier, (mia. kr.)")
saeson <- c("Sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed & NKN1.vaekst$SASON == saeson), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype, prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("P.31 Privatforbrug")
prisenhed <- c("Realvækst i forhold til foregående periode (pct.)")
saeson <- c("Sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
 & NKN1.vaekst$PRISENHED == prisenhed & NKN1.vaekst$SASON == saeson), ]
 , aes(x = Kvartal, y = INDHOLD)) + 
 geom_line() + theme(legend.position = "bottom" ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
 scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype, prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("B.1*g Bruttonationalprodukt, BNP")
prisenhed <- c("Løbende priser, (mia. kr.)")
saeson <- c("Sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype
                                  & NKN1.vaekst$PRISENHED == prisenhed& NKN1.vaekst$SASON == saeson), ]
                , aes(x = Kvartal, y = INDHOLD)) +
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) +
        scale_x_date(date_breaks = "12 month", date_labels = "%y") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed,".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("B.1*g Bruttonationalprodukt, BNP")
prisenhed <- c("2010-priser, kædede værdier, (mia. kr.)")
saeson <- c("Sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                                  & NKN1.vaekst$PRISENHED == prisenhed & NKN1.vaekst$SASON == saeson), ]
                , aes(x = Kvartal, y = INDHOLD)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype, prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("B.1*g Bruttonationalprodukt, BNP")
prisenhed <- c("Realvækst i forhold til foregående periode (pct.)")
saeson <- c("Sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                                  & NKN1.vaekst$PRISENHED == prisenhed & NKN1.vaekst$SASON == saeson), ]
                , aes(x = Kvartal, y = INDHOLD)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype, prisenhed, saeson, ".png"), height = 540, width = 720)
figur
dev.off()

graftype <- c("B.1*g Bruttonationalprodukt, BNP")
prisenhed <- c("2010-priser, kædede værdier, (mia. kr.)")
saeson <- c("Ikke sæsonkorrigeret")
figur <- ggplot(NKN1.vaekst[which(NKN1.vaekst$TRANSAKT == graftype 
                                  & NKN1.vaekst$PRISENHED == prisenhed 
                                  & NKN1.vaekst$SASON == saeson) ,  ]
                , aes(x = Kvartal, y = pct_change_aa)) + 
        geom_line() + theme(legend.position = "bottom" ) +
        labs( title = paste("Årlig vækst i",graftype, "pr. kvartal"), subtitle = paste(prisenhed, saeson ), caption = paste("DSt", dsttabel)) + 
        scale_x_date(date_breaks = "12 month", date_labels = "%y") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
png(filename = paste(graftype,prisenhed, saeson, "årlig", ".png"), height = 540, width = 720)
figur
dev.off()
