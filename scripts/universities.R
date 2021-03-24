# top 15 universities appeared in author's affiliation list of OoSRs

# load the packages --------------------------------------------------------------------------
library(here)
library(tidyverse)
library(ggpubr)
library(ggimage)
library(ggthemes)


# load the data ------------------------------------------------------------------------------
library(readxl)
institutes <- read_excel(here("data", "universities.xlsx"))



# set new color palette -------------------------------------------------------------
library(paletteer)

scale_tableu <- function(){
  paletteer::scale_colour_paletteer_d("ggthemes::Classic_10_Medium", direction = 1)
  
}

options(ggplot2.discrete.colour = scale_tableu)





# data frames with images --------------------------------------------------------------------

### for the two first universities as a vector
posit<- c("KING'S COLLEGE LONDON", 
          "UNIVERSITY OF OXFORD")
img <- here("logos", c("kings.png", "oxford.png"))
imgs <- data.frame(x = posit, y = -3.5, image = img)



### for the remaning universities
img3 <- data.frame(x = "IMPERIAL COLLEGE LONDON", y = -3.5, image = here("logos", "imperial.jpg"))
img4 <- data.frame(x = "UNIVERSITY OF TORONTO", y = -3.5, image = here("logos", "toronto.jpg"))
img5 <- data.frame(x = "UNIVERSITY OF ALBERTA", y = -3.5, image = here("logos", "alberta.png"))
img6 <- data.frame(x = "UNIVERSITY OF EXETER", y = -3.5, image = here("logos", "exeter.jpg"))
img7 <- data.frame(x = "UNIVERSITY OF OTTAWA", y = -3.5, image = here("logos", "ottawa.png"))
img8 <- data.frame(x = "MONASH UNIVERSITY", y = -3.5, image = here("logos", "monash.png"))
img9 <- data.frame(x = "UNIVERSITY OF MANCHESTER", y = -3.5, image = here("logos", "manchester.jpg"))
img10 <- data.frame(x = "UNIVERSITY OF IOANNINA", y = -3.5, image = here("logos", "ioannina.png"))
img11 <- data.frame(x = "UNIVERSITY OF SYDNEY", y = -3.5, image = here("logos", "sydney.png"))
img12 <- data.frame(x = "UNIVERSITY OF MELBOURNE", y = -3.5, image = here("logos", "melbourne.jpg"))
img13 <- data.frame(x = "MCMASTER UNIVERSITY", y = -3.5, image = here("logos", "mcmaster.png"))
img14 <- data.frame(x = "STANFORD UNIVERSITY", y = -3.5, image = here("logos", "stanford.png"))
img15 <- data.frame(x = "UNIVERSITY OF PADOVA", y = -3.5, image = here("logos", "padova.jpeg"))





# plot universities ---------------------------------------------------------------------------

fig8 <- ggplot(institutes , aes(x = reorder(Institute, Freq), y = Freq)) + 
  geom_point(aes(color = Country), size=13) + 
  geom_segment(aes(x=Institute, 
                   xend=Institute, 
                   y=0, 
                   yend=Freq), color = "grey10", size=3.3) + 
  geom_point(color = "grey10", size=5) + 
  geom_text(aes(label = Freq), size= 7, vjust = -1.3, hjust = 0.55) + 
  geom_text(aes(y = 1.5, label = Institute), size= 6.0, vjust = -0.9, hjust = 0.01) + 
  labs(x="University", y="Number of publications", color = "Country", size=17) +
  labs_pubr(base_size = 16, base_family = "") +
  theme_pubclean(base_size = 20) +
  theme(panel.grid.major.x = element_line(linetype = "dotted", size = 1, color = "grey"),
        #panel.grid.minor.x = element_line(linetype = "dotted", size = 1, color = "grey"),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(hjust=1, face = "bold", margin = margin(t = 10, r = 0, b = 10, l = 0), size = 24),
        axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0), size = 24),
        axis.text.x = element_text(color = "#464a62"),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.title =  element_text(size = 18),
        legend.text =  element_text(size = 15),
        legend.key = element_rect(size = 16, colour = NA, fill = NA),
        legend.background=element_rect(fill='transparent'),
        #legend.margin=margin(t = 0, unit='cm'),
        #legend.spacing.x = unit(1.0, 'cm'),
        #legend.justification = "left",
        #legend.position = c(0.9, 0.3)
        legend.position="right"
  ) +
  scale_y_continuous(limits=c(-3.5, 70),breaks = seq(0, 70, 5)) +
  coord_flip() +
  geom_image(data = imgs, aes(x, y, image = image), size = 0.065) +
  geom_image(data = img3, aes(x, y, image = image), size = 0.075) +
  geom_image(data = img4, aes(x, y, image = image), size = 0.075) +
  geom_image(data = img5, aes(x, y, image = image), size = 0.085) +
  geom_image(data = img6, aes(x, y, image = image), size = 0.065) +
  geom_image(data = img7, aes(x, y, image = image), size = 0.065) +
  geom_image(data = img8, aes(x, y, image = image), size = 0.085) +
  geom_image(data = img9, aes(x, y, image = image), size = 0.085) +
  geom_image(data = img10, aes(x, y, image = image), size = 0.045) +
  geom_image(data = img11, aes(x, y, image = image), size = 0.075) +
  geom_image(data = img12, aes(x, y, image = image), size = 0.065) +
  geom_image(data = img13, aes(x, y, image = image), size = 0.075) +
  geom_image(data = img14, aes(x, y, image = image), size = 0.045) +
  geom_image(data = img15, aes(x, y, image = image), size = 0.060)


fig8

#ggsave(here::here("figures", paste0("fig8", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
       #type = "cairo", width = 16, height = 15, dpi = 320, compression = "lzw")


