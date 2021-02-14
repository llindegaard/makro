library(statsDK)
library(tidyverse)
NAHB <- sdk_retrieve_data("NAHB", lang = "dk")


# NAHB %>% 
# mutate(INDHOLD = str_replace(INDHOLD, ".", "NA"))

NAHB$INDHOLD <- as.numeric(NAHB$INDHOLD)

NAHB.vaekst <- NAHB %>%
  group_by(SOCIO) %>% 
 arrange(c(TID), .by_group = TRUE) %>%
 mutate(pct_change = (INDHOLD/lag(INDHOLD) - 1) * 100) %>%
 # filter(TRANSAKT == "D.1 Compensation of employees" & SEKTOR == "S.11+S.12+S.14+S.15 Total economy, except General Government") %>%
 filter(TID > 1989)
 
# distinct(NAHB.vaekst $TRANSAKT)
NAHB.vaekst <- as.data.frame(NAHB.vaekst)

graftype <- c("Beskæftigede med bopæl i Danmark" )
prisenhed <- c("Årlig stigning")
figur <- ggplot(NAHB.vaekst[which(NAHB.vaekst$SOCIO == graftype), ]
 , aes(x = TID, y = pct_change)) +
   geom_line() + theme(legend.position = "bottom", title = element_text(graftype) ) +
   labs(title = graftype, subtitle = prisenhed , caption = "DSt") 
png(filename = paste0(graftype,prisenhed,".png"), height = 450, width = 600)
figur
dev.off()

