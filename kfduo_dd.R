setwd("//cdc.gov/project/CCID_NCIRD_DVD_PPLB/_PMDDL/Kim/R/kfduo")

library(ggplot2)
library(ggalt)
library(dplyr)


#duo_dat<-read.csv("duodd.csv", header = TRUE)
duo_dat1<-read.csv("duodd1.csv", header = TRUE)

duo_dat1<- duo_dat1 %>% filter(assay == "EV" | assay == "PV" | assay == "Sabin1" | assay == "Sabin3" | assay == "WPV1")



#dumbbell
graph <- ggplot()+
  geom_dumbbell(data=duo_dat1, aes(y=as.numeric(elution5), x=factor(sample), xend=as.numeric(elution9), group = method),
                       size=1.5, color="#7B7875", size_x=3, size_xend = 3, colour_x = "red", colour_xend = "blue",
                dot_guide=TRUE, dot_guide_size=0.25, show.legend = TRUE, na.rm =TRUE) +
  scale_x_continuous("Ct", breaks = c(10,20,30,40)) +
  scale_y_discrete(#"Sample", 
    labels = c("996" = "996 (SL3)",
               "942" = "942 (SL3)",
               "729" = "729 (SL3)",
               "508" = "508 (WPV1)",
               "343" = "343 (WPV1)", 
               "792" = "792 (WPV1)"),
    limits = c("792", "343", "508", "729", "942", "996")) +
  scale_color_manual(name = "Elution", values = c("red", "blue")) +
  facet_wrap(assay~method,ncol=2, nrow=6)+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(size = 10),
        panel.grid.major=element_line(color = "grey"))+
  labs(y="Sample",
       title="KF Duo DD ITD Results", 
       subtitle="ITD results for KF Duo RNA extracted samples" 
       #caption="Source: PMDDL"
       ) 

graph

ggsave("kf_duo_kim.png", dpi = 500, height = 8, width = 13, units = "in")




#dumbbell

duo_dat1<-read.csv("duodd2.csv", header = TRUE)

duo_dat1<- duo_dat1 %>% filter(assay == "EV" | assay == "PV" | assay == "Sabin1" | assay == "Sabin3" | assay == "WPV1")

graph <- ggplot()+
  geom_dotplot(data=duo_dat1, aes(x = method, y = as.numeric(ct)), binaxis='y', stackdir='center', dotsize=1, show.legend = TRUE, na.rm =TRUE) +
  theme_minimal()+
  facet_wrap(assay~sample)+
  labs(y="Sample",
       title="KF Duo DD ITD Results", 
       subtitle="ITD results for KF Duo RNA extracted samples" 
       #caption="Source: PMDDL"
  ) 

graph

ggsave("kf_duo_kim.png", dpi = 500, height = 8, width = 13, units = "in")





#dots
duo_dat1<-read.csv("duodd3.csv", header = TRUE)

duo_dat1<- duo_dat1 %>% filter(assay == "EV" | assay == "PV" | assay == "Sabin1" | assay == "Sabin3" | assay == "WPV1")
duo_dat1$method <- factor(duo_dat1$method,      # Reordering group factor levels
                         levels = c("250ul", "500ul", "750ul", "1000ul"))
methodcol<-c("1000ul" = "#ee6c4d",
             "750ul" = "#3d5a80", 
             "500ul" = "#98c1d9", 
             "250ul" = "#7B7875")
elutionsize<-c("5ul" = "3",
               "9ul" = "5")
elutionsize<-as.numeric(elutionsize)
samplelab = c("996" = "996 (SL3)",
           "942" = "942 (SL3)",
           "729" = "729 (SL3)",
           "508" = "508 (WPV1)",
           "343" = "343 (WPV1)", 
           "792" = "792 (WPV1)")

graph <- ggplot(data=duo_dat1, aes(x = assay, y = as.numeric(ct), color = method))+
  geom_jitter(aes(size = elution),
              stroke = 1, alpha = 0.75, show.legend = TRUE, na.rm =TRUE) +
 # geom_line(aes(color = factor(sample)))+
  theme_minimal()+
  scale_color_manual(values = methodcol) +
  scale_size_manual(values = elutionsize) +
  #facet_wrap(assay~., ncol=5)+
  facet_wrap(~method, ncol = 6)+
  labs(y="Ct",
       title="KF Duo DD ITD Results", 
       subtitle="ITD results for KF Duo RNA extracted samples" 
       #caption="Source: PMDDL"
  ) 

graph

ggsave("duo_fullcomp.png", dpi = 500, height = 8, width = 13, units = "in")




#box and whiskers 9ul (in red boxplot) removed 250ul method
duo_dat1<-read.csv("duodd3_nodis.csv", header = TRUE)

duo_dat1<- duo_dat1 %>% filter(assay == "PV" |assay == "Sabin3" | assay == "QB" | assay == "WPV1")
duo_dat1<- duo_dat1 %>% filter(method == "100X" |method == "75X" | method == "25X")
duo_dat1<- duo_dat1 %>% filter(sample == "996" | sample == "942" | sample == "729" | sample == "508" | sample == "343")
duo_dat1a<- duo_dat1 %>% filter(elution == "5ul" & method == "100X" | method == "25X")
duo_dat1b<- duo_dat1 %>% filter(elution == "5ul" & method == "75X")
#uo_dat1a<- duo_dat1 %>% filter(method == "1000ul")

duo_dat1c<- duo_dat1 %>% filter(elution == "9ul" & method == "100X")
#duo_dat2<- duo_dat1 %>% filter(method == "1000ul"
                               #| method == "250ul"
#                               )
duo_dat3<- full_join(duo_dat1a, duo_dat1b)
duo_dat4<- full_join(duo_dat3, duo_dat1c)
#duo_dat1<- duo_dat1 %>% filter(elution != "5ul" & assay != "PV")
duo_dat4$method <- factor(duo_dat4$method,      # Reordering group factor levels
                          levels = c("100X", "75X", "25X"))
duo_dat4$elution <- factor(duo_dat4$elution,      # Reordering group factor levels
                          levels = c("5ul", "9ul"))
methodcol<-c("1000ul" = "#ee6c4d",
             "750ul" = "#3d5a80", 
             "500ul" = "#98c1d9", 
             "250ul" = "#7B7875")
methodlab<-c("100X" = "100ul",
             "75X" = "75ul", 
             "25X" = "25ul")
elutioncol<-c("5ul" = "#48839A",
               "9ul" = "#cc444b",
              "NA" = "#000000")
elutionfill<-c("5ul" = "#48839A",
               "9ul" = "#cc444b", 
               "NA" = "#000000")
elutionsize<-as.numeric(elutionsize)
samplelab = c("996" = "996 (SL3)",
              "942" = "942 (SL3)",
              "729" = "729 (SL3)",
              "508" = "508 (WPV1)",
              "343" = "343 (WPV1)", 
              "792" = "792 (WPV1)")

graph <- ggplot(data=duo_dat4, aes(x = factor(method), y = as.numeric(ct)))+
  geom_boxplot(aes(x = factor(method), color = factor(elution)), 
               #fill = "lightgrey", 
               size = 1, alpha = 0.75, show.legend = FALSE, na.rm =TRUE, outlier.shape = NA) +
  geom_point(aes(x = method, fill = factor(elution)), position = position_jitterdodge(), alpha = .6, shape = 21, size = 4, na.rm = TRUE, show.legend = FALSE)+
  scale_color_manual(values = elutioncol) +
  #scale_size_manual(values = elutionsize) +
  #facet_wrap(assay~., ncol=5)+
  scale_fill_manual(values = elutionfill) +
  scale_x_discrete(labels = c("100X" = "100[5ul]   100[9ul]",
                              "75X" = "75[5ul]", 
                              "25X" = "25[5ul]")) +
  facet_wrap(~assay, ncol = 4)+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 12, angle = 0),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.title = element_text( size = 14),
    strip.text.x = element_text( size = 12),
    strip.text.y = element_text( size = 12, angle =0, lineheight = 20),
    #strip.background.x = element_rect(size=.2),
    legend.text = element_text( size = 14, angle =0),
    legend.title = element_text( size = 14, angle =0),
    legend.position="bottom",
    panel.background = element_rect(fill = "white"),
    #panel.border = element_rect(colour="black"),
    panel.border=element_rect(fill=NA, color = "black"),
    panel.spacing = unit(0, "mm"),
    strip.background = element_rect(fill="white", colour="black")
    )  +
  labs(y="Ct",
       x= "Sample Volume Extracted",
       color = "Elution", 
       title="KF Duo Direct Detection", 
       subtitle="ITD Results for five RNA extracted samples" 
       #caption="Source: PMDDL"
  ) 

graph

ggsave("duo_9ul_kconc1.png", dpi = 500, height = 7, width = 15, units = "in")




#box and whiskers 5ul
duo_dat1<-read.csv("duodd3_nodis.csv", header = TRUE)

duo_dat1<- duo_dat1 %>% filter(assay == "PV" |assay == "Sabin3" | assay == "QB" | assay == "WPV1")
duo_dat1<- duo_dat1 %>% filter(sample == "996" | sample == "942" | sample == "729" | sample == "508" | sample == "343")
duo_dat1<- duo_dat1 %>% filter(elution == "5ul")
duo_dat1$method <- factor(duo_dat1$method,      # Reordering group factor levels
                          levels = c("1000ul", "750ul", "500ul", "250ul"))
methodcol<-c("1000ul" = "#ee6c4d",
             "750ul" = "#3d5a80", 
             "500ul" = "#98c1d9", 
             "250ul" = "#7B7875")
elutioncol<-c("5ul" = "#3d5a80",
              "9ul" = "#ee6c4d")
elutionsize<-as.numeric(elutionsize)
samplelab = c("996" = "996 (SL3)",
              "942" = "942 (SL3)",
              "729" = "729 (SL3)",
              "508" = "508 (WPV1)",
              "343" = "343 (WPV1)", 
              "792" = "792 (WPV1)")

graph <- ggplot(data=duo_dat1, aes(x = method, y = as.numeric(ct), color = "black"))+
  geom_boxplot(aes(color = elution), 
               #fill = "lightgrey", 
               size = 1, alpha = 0.75, color = "steelblue", na.rm =TRUE, outlier.shape = NA, show.legend. = FALSE) +
  geom_jitter(color = "black", shape = 1, size = 2, show.legend = FALSE)+
  #scale_color_manual(values = elutioncol) +
  #scale_size_manual(values = elutionsize) +
  #facet_wrap(assay~., ncol=5)+
  facet_wrap(~assay, ncol = 5)+
  theme( 
    axis.text = element_text( size = 20 ),
    axis.text.x = element_text(size = 12, angle = 0),
    axis.text.y = element_text(size = 12, angle = 0),
    axis.title = element_text( size = 14),
    strip.text.x = element_text( size = 12),
    strip.text.y = element_text( size = 12, angle =0, lineheight = 20),
    #strip.background.x = element_rect(size=.2),
    legend.text = element_text( size = 14, angle =0),
    legend.title = element_text( size = 14, angle =0),
    legend.position="bottom",
    panel.background = element_rect(fill = "white"),
    #panel.border = element_rect(colour="black"),
    panel.border=element_rect(fill=NA, color = "black"),
    panel.spacing = unit(0, "mm"),
    strip.background = element_rect(fill="white", colour="black")
  )  +
  labs(y="Ct",
       x= "Extraction Method",
       color = "Elution", 
       title="KF Duo Direct Detection", 
       subtitle="ITD Results for six RNA extracted samples - 5ul Elution" 
       #caption="Source: PMDDL"
  ) 

graph

ggsave("duo_5ul.png", dpi = 500, height = 6, width = 12, units = "in")

write.csv(duo_dat1, "kim.csv")
