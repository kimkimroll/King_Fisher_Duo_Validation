setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/kfduo")

library(ggplot2)
library(tidyverse)

dat_zq <-read.csv("zymo_duo_comparison111020.csv", header = TRUE)

dat_zq$DASH <-as.character(dat_zq$DASH)
dat_zq$Cq[dat_zq$Cq == "N/A"] <- 'NA'
dat_zq$Cq <-as.numeric(dat_zq$Cq)

methodcolors<-c('duo_5ul' = '#274c77',
                'duo_7ul' = '#6096ba',
                'zymo' = '#e87421')

dat<-dat_zq %>% filter (discordant %in% "no")

zymo_qb_graph_nodis <-ggplot(dat, aes(x = Method, y = Cq), na.rm = TRUE)+
  geom_boxplot(aes(x = Method, group = Method, color = Method), outlier.shape = NA, na.rm = TRUE, show.legend = FALSE)+
  geom_jitter(shape = 1, alpha = .75, size = 3, color = "black", position = position_jitter(width = 0.2), na.rm = TRUE)+
  scale_x_discrete(labels = c('duo_5ul' = 'Duo(5ul)',
                              'duo_7ul' = 'Duo(7ul)',
                              'zymo' = 'Zymo'))+
  scale_color_manual(values = methodcolors)+
  theme_minimal()+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 12, angle = 0),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.title = element_text( size = 14),
    strip.text.x = element_text( size = 12),
    strip.text.y = element_text( size = 12, angle =0, lineheight = 20),
    #strip.background.x = element_rect(size=.2),
    legend.text = element_text( size = 14, angle =0),
    legend.title = element_text( size = 14, angle =0)
  )+
  labs(x = "Method",
       y = "Ct",
       title = "Zymo and QBeta Comparison (Discordants Removed)",
       subtitle = "Duo (5ul) method = 75x\nDuo (7ul) method = 105x")+
  facet_wrap(.~Target, nrow = 1)

zymo_qb_graph_nodis

ggsave("zymo_qb_comp_nodiscordants.png", dpi = 500, height = 6, width = 12, units = "in")

