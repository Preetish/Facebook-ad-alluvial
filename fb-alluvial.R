library("ggalluvial")
library("magrittr")
library("dplyr")

A_col <- "firebrick3"
B_col <- "darkorange"
C_col <- "deepskyblue3"
alpha <- 0.7

fb_ad <- read.csv(file.choose())

fb_ad <- fb_ad[c(1,3)]

fb_ad %>% group_by(Category, Response) %>%
  summarise(freq = n()) -> fb_ad_3d

ggplot(fb_ad_3d,
       aes(weight = freq, axis1 = Category, axis2 = Response)) +
       geom_alluvium(aes(fill = Response, color = Response), 
                     width = 1/12, alpha = alpha, knot.pos = 0.4) +
       geom_stratum(width = 1/6, color = "grey") +
       geom_label(stat = "stratum", label.strata = TRUE) +
       scale_x_continuous(breaks = 1:2, labels = c("Category", "Response")) +
       scale_fill_manual(values  = c(A_col, B_col, C_col)) +
       scale_color_manual(values = c(A_col, B_col, C_col)) +
       ggtitle("Relevance of Facebook Custom List Advertising") +
       theme_minimal() +
       theme(
             legend.position = "none",
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             axis.text.y = element_blank(),  
             axis.text.x = element_text(size = 12, face = "bold")
      )
