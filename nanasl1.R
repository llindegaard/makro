library(statsDK)
library(tidyverse)
dsttabel <- c("NASL1")
nanasl1 <- sdk_retrieve_data(dsttabel, lang = "dk")

nanasl1$INDHOLD <- as.numeric(nanasl1$INDHOLD)

nanasl1.vaekst <- nanasl1 %>%
  group_by(TRANSAKT, SEKTOR) %>% 
  arrange(c(TID), .by_group = TRUE) %>%
  mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
  filter(TID > 1989)

graftype <- c("D.1 Aflønning af ansatte" )
prisenhed <- c("Aflønning")
figur <- ggplot(nanasl1.vaekst[which(nanasl1.vaekst$TRANSAKT == graftype 
                       & (nanasl1.vaekst$SEKTOR == "S.1 Hele økonomien"  
                       | nanasl1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Selskaber, organisationer og husholdninger"
                 )), ]
       , aes(x = TID, y = pct_change, color = SEKTOR)) + 
  geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
  labs( title = graftype, subtitle = prisenhed  , caption = paste("DSt", dsttabel)  )
png(filename = paste(graftype,prisenhed,".png"), height = 800, width = 1200)
figur
dev.off()

graftype <- c("B.1GF Bruttofaktorindkomst, BFI" )
prisenhed <- c("Faktoromkostninger")
figur <- ggplot(nanasl1.vaekst[which(nanasl1.vaekst$TRANSAKT == graftype 
                       & (nanasl1.vaekst$SEKTOR == "S.1 Hele økonomien"  
                          | nanasl1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Selskaber, organisationer og husholdninger"
                       )), ]
       , aes(x = TID, y = pct_change, color = SEKTOR)) + 
  geom_line()  + theme(legend.position = "bottom", title = element_text(graftype) ) +
  labs( title = graftype, subtitle = prisenhed  , caption = "DSt") 
png(filename = paste(graftype,prisenhed,".png"), height = 800, width = 1200)
figur
dev.off()