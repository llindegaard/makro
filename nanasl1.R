library(statsDK)
library(tidyverse)
dsttabel <- c("NASL1")
nanasl1 <- sdk_retrieve_data(dsttabel, lang = "dk")
meta <- statsDK::sdk_retrieve_metadata("NASL1")
prisenhed <- meta$unit

nanasl1$INDHOLD <- as.numeric(nanasl1$INDHOLD)

nanasl1.vaekst <- nanasl1 %>%
 group_by(TRANSAKT, SEKTOR) %>% 
 arrange(c(TID), .by_group = TRUE) %>%
 mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
 filter(TID > 1979)

graftype <- c("D.1 Aflønning af ansatte" )
# prisenhed <- c("Aflønning")
figur <- ggplot(nanasl1.vaekst[which(nanasl1.vaekst$TRANSAKT == graftype 
      & (nanasl1.vaekst$SEKTOR == "S.1 Hele økonomien" 
      | nanasl1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Selskaber, organisationer og husholdninger"
     )), ]
  , aes(x = TID, y = pct_change, color = SEKTOR)) + 
 geom_line() + theme(legend.position = "bottom", title = element_text(graftype) ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt", dsttabel) )
png(filename = paste(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

graftype <- c("B.1GF Bruttofaktorindkomst, BFI" )
# prisenhed <- c("Faktoromkostninger")
figur <- ggplot(nanasl1.vaekst[which(nanasl1.vaekst$TRANSAKT == graftype 
      & (nanasl1.vaekst$SEKTOR == "S.1 Hele økonomien" 
       | nanasl1.vaekst$SEKTOR == "S.11+S.12+S.14+S.15 Selskaber, organisationer og husholdninger"
      )), ]
  , aes(x = TID, y = pct_change, color = SEKTOR)) + 
 geom_line() + theme(legend.position = "bottom", title = element_text(graftype) ) +
 labs( title = graftype, subtitle = prisenhed , caption = paste("DSt",dsttabel)) 
png(filename = paste(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()


graftype <- c("P.1 Produktion", "P.2 Forbrug i produktionen" ,"D.1 Aflønning af ansatte")
sektor <- c("S.1 Hele økonomien")

figur <- ggplot(nanasl1.vaekst[which(nanasl1.vaekst$TRANSAKT %in% graftype 
                                     & (nanasl1.vaekst$SEKTOR %in% sektor
                                     )), ]
                , aes(x = TID, y = pct_change, color = TRANSAKT)) + 
  geom_line() + theme(legend.position = "bottom") + 
  # geom_ribbon(aes(xmin = 1985.5, xmax = 1992.5), alpha = 0.05, show.legend = FALSE, outline.type = "both") +
  # geom_ribbon(aes(xmin = 2008.5, xmax = 2009.5), alpha = 0.05, show.legend = FALSE, outline.type = "both") +
    # geom_rect(aes(xmin = 1986, xmax = 1992,
  #               ymin = -Inf, ymax = Inf,
  #               alpha = .050)) +
  labs( title = graftype, subtitle = prisenhed , caption = paste("DSt",dsttabel))
png(filename = paste(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

