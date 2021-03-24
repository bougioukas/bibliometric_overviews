# load the packages -------------------------------------------------------------------------
library(countrycode)
library(colorspace)
library(ggpubr)

library(here)
library(tidyverse)



# load the dataset ---------------------------------------------------------------------------

library(readxl)
bmx <- read_excel(here("data", "bmx.xlsx"))



# prepare the data ---------------------------------------------------------------------------


countries <- bmx %>% 
  mutate(code = countrycode(CO, "country.name.en", "iso3c"))


cl <- codelist %>%
  select(country.name.en, continent, region23, iso3c) %>%
  rename(country = country.name.en, code = iso3c, region = region23) %>% 
  filter_at(vars(continent, code), all_vars(!is.na(.)))


df <- countries %>%
  left_join(cl, by = "code")



# change the name of subject categories and regions

df$region[df$region=="Eastern Africa"] <- "1a.Eastern Africa"
df$region[df$region=="Northern Africa"] <- "1b.Northern Africa"
df$region[df$region=="Southern Africa"] <- "1c.Southern Africa"


df$region[df$region=="Australia and New Zealand"] <- "2a.Australia & N.Zealand"


df$region[df$region=="Central Asia"] <- "3a.Central Asia"
df$region[df$region=="Southern Asia"] <- "3b.Southern Asia"
df$region[df$region=="Western Asia"] <- "3c.Western Asia"
df$region[df$region=="South-Eastern Asia"] <- "3d.South-Eastern Asia"
df$region[df$region=="Eastern Asia"] <- "3e.Eastern Asia"


df$region[df$region=="Caribbean"] <- "4a.Caribbean"
df$region[df$region=="Central America"] <- "4b.Central America"
df$region[df$region=="South America"] <- "4c.South America"
df$region[df$region=="Northern America"] <- "4d.Northern America"


df$region[df$region=="Eastern Europe"] <- "5a.Eastern Europe"
df$region[df$region=="Western Europe"] <- "5b.Western Europe"
df$region[df$region=="Southern Europe"] <- "5c.Southern Europe"
df$region[df$region=="Northern Europe"] <- "5d.Northern Europe"



df$SC[df$SC=="Dentistry, Oral Surgery & Medicine"] <- "Dentistry & Oral Surgery"
df$SC[df$SC=="Health Informatics, eHealth & Technology"] <- "eHealth & Technology"
df$SC[df$SC=="Occupational & Environmental Health"] <- "Occupational & Environ. Health"
df$SC[df$SC=="Diseases of the circulatory system"] <- "Circulatory system diseases"
df$SC[df$SC=="Physical Activity & Sports Medicine"] <- "Phys. Activity & Sports Medicine"
df$SC[df$SC=="Rehabilitation & Physical Therapy"] <- "Rehabilit. & Physical Therapy"




# Figure 4A --------------------------------------------------------------------------

# top subject categories (SC)
top_SC <- df %>% 
  count(SC, name = "COUNTS", sort = T) %>% 
  mutate(percent = round(COUNTS / sum(COUNTS) * 100, digits = 1)) %>%
  #mutate(text = paste0(COUNTS, " (", percent, "%)"))
  slice(1:20)



# barplot subregions
fig4A <- ggplot(top_SC, aes(x = COUNTS, y = reorder(SC, COUNTS))) +
  geom_col(fill = "#6AE1D0", width = 0.6) +
  #geom_text(aes(label = text), size= 4.5, hjust = -0.1) +
  geom_text(aes(label = COUNTS), size= 5.5, hjust = +1.2) +
  scale_x_continuous(expand = c(0.008, 0), limits=c(0,160), breaks = seq(0, 160, by = 10)) +
  labs(x="Number of publications", y="Subject category") +
  theme_pubclean(base_size = 19) +
  #my_theme
  theme(panel.grid.major.x = element_line(linetype = "dotted", size = 1, color = "grey"),
        panel.grid.major.y = element_blank(),
        panel.grid.major = element_blank(),
        axis.title.x = element_text(hjust=1, face = "bold", margin = margin(t = 10, r = 0, b = 10, l = 0), size = 20),
        axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0), size = 20),
        axis.text = element_text(color = "#464a62"))

fig4A

# save image
#ggsave(here::here("figures", paste0("fig4A", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
#type = "cairo", width = 18, height = 9, dpi = 360, compression = "lzw")  



# Figure 4B --------------------------------------------------------------------------

# counts for each region
regions <- df %>% 
  count(region, name = "COUNTS", sort = T)


# counts for subject category (SC)
data_cntSC <- df %>% 
  count(SC, name = "COUNTS", sort = T) 

#  mutate(percent = round(COUNTS / sum(COUNTS) * 100, digits = 1))




# counts for subject category (SC) by region
dat <- df %>% 
  count(region, SC, continent, name = "COUNTS") %>% 
  complete(region, SC, fill = list(COUNTS = 0))




# create the heatmap for subject categories by regions

fig4B <- ggplot(data = dat, aes(x = SC, y = region)) +
  theme_classic(base_size = 18) +
  geom_tile(aes(fill = COUNTS), color='grey70') +
  geom_text(aes(color = COUNTS > 30, label = COUNTS), size = 4.65) +
  theme(axis.title=element_text(size = 16, face = "bold",),
        axis.text.x = element_text(angle = 90, vjust= + 0.08, hjust=0.98, size = 16),
        axis.text.y=element_text(size = 16),
        legend.position = "top",
        legend.justification = "right",
        legend.title = element_text(size = 14, face = "bold", hjust = 1.5, vjust=0.75), 
        legend.text = element_text(size = 14, vjust= 2.75),
        legend.key.size = unit(0.8, "cm"),
        legend.key.width = unit(2.8,"cm")) +
  scale_fill_gradient(low="white", limits = c(0, 80), 
                      breaks=c(0, 20, 40, 60, 80), high="darkmagenta", 
                      name = "Number of publications") +
  
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  labs(x="Subject category", y="World region"
  ) 


fig4B


# add marginal totals
h_total <- regions %>% 
  mutate(SC = 'zTotal')

v_total <- data_cntSC %>% 
  mutate(region = '6.Total')



# add totals and horizontal lines

fig4B + 
  geom_text(aes(label = COUNTS), data = h_total, size = 4.8) +
  geom_text(aes(label = COUNTS), data = v_total, size = 4.8) +
  geom_segment(aes(x = 0.5, y = 17.5, xend = 47.5, yend = 17.5), size = 0.75, color = "grey10") +
  geom_segment(aes(x = 47.5, y = 0.5, xend = 47.5, yend = 17.5), size = 0.75, color = "grey10") +
  geom_segment(aes(x = 0.5, y = 3.5, xend = 47.5, yend = 3.5), size = 0.75, color = "grey10") +
  geom_segment(aes(x = 0.5, y = 4.5, xend = 47.5, yend = 4.5), size = 0.75, color = "grey10") +
  geom_segment(aes(x = 0.5, y = 9.5, xend = 47.5, yend = 9.5), size = 0.75, color = "grey10") +
  geom_segment(aes(x = 0.5, y = 13.5, xend = 47.5, yend = 13.5), size = 0.75, color = "grey10")

# save image
#ggsave(here::here("figures", paste0("fig4B", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
#type = "cairo", width = 18, height = 9, dpi = 360, compression = "lzw")  
