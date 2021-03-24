# load the packages -------------------------------------------------------------------

if (!require("ggflags")) {
  devtools::install_github("rensa/ggflags")
  library(ggflags)
}

library(ggpubr) # 'ggplot2' Based Publication Ready Plots
library(countrycode) # Convert Country Names and Country Codes


library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'



# load the dataset --------------------------------------------------------------------

library(readxl) # Read Excel Files
bmx <- read_excel(here("data", "bmx.xlsx"))




# word map -----------------------------------------------------------------------------


# country counts
countries <- bmx %>% 
  count(CO, name = "COUNTS") %>% 
  arrange(desc(COUNTS)) %>% 
  mutate(code = countrycode(CO, "country.name.en", "iso3c"))

# country codes
cl <- codelist %>%
  select(country.name.en, continent, iso3c) %>%
  rename(country = country.name.en, code = iso3c) %>% 
  filter_at(vars(continent, code), all_vars(!is.na(.)))


# join the countries and counts by code
df <- cl %>%
  left_join(countries, by = "code") %>% 
  arrange(desc(COUNTS))


# rename United States and United Kingdom
df$country[df$country=="United States"] <- "USA"
df$country[df$country=="United Kingdom"] <- "UK"




require(maps) # Draw Geographical Maps

# map long and lat
world_map <- map_data("world") %>% 
  select(-subregion) %>% 
  rename(country = region) %>% 
  filter(country !="Antarctica")

map <- left_join(world_map, df, by = "country")

map$COUNTS[is.na(map$COUNTS)] <- 0

colors <- c("white", "#E5F1FD", "#89B1D5", "#1F8FB7", "#006298", "#013E7F", "#002157")

map$COUNTS2 <- cut(map$COUNTS, breaks=c(-Inf, 0.99, 5, 10, 20, 100, 200, Inf), 
                   labels=c("0", "1 - 5", "6 - 10", "11 - 20", "21 - 100", "101 - 200", ">200"))


# wordmap
fig3 <- ggplot(map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = COUNTS2), color = "grey60") +
  scale_fill_manual(values = colors, drop = FALSE, name = "Number of \npublications") +
  theme_void() +
  theme(plot.caption = element_text(size = 14, face = "italic"),
        legend.position=c(0.15, 0.25),
        legend.key.size = unit(1.3, "cm"),
        legend.key.width = unit(1.3,"cm"), 
        legend.title = element_text(size = 23, face = "bold"),
        legend.text = element_text(size = 20, face = "bold"))
  
fig3

# save image
#ggsave(here::here("figures", paste0( "fig3", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
       #type = "cairo", width = 24, height = 13, dpi = 300, compression = "lzw")






# bars with flags ---------------------------------------------------------------------------

# top 15 country counts
country_top15 <- bmx %>% 
  count(CO, name = "COUNTS") %>% 
  arrange(desc(COUNTS)) %>% 
  slice(1:15)


dat <- country_top15 %>%
  mutate(code = countrycode(CO, "country.name", "iso2c")) %>%
  mutate(code = tolower(code)) %>%
  rename(COUNTRY = CO)


dat <- dat %>% 
  mutate(COUNTS2 = COUNTS-5.2)


fig5 <- ggplot(dat, aes(x = COUNTS, y = reorder(COUNTRY, COUNTS))) +
  geom_col(fill = "#A9B0DE", width = 0.70) +
  geom_flag(aes(x = COUNTS2, country = code), size = 13.0) +
  geom_text(aes(label = COUNTS), size= 8, hjust = - 0.2) +
  scale_x_continuous(expand = c(0.008, 0), limits=c(0,350), breaks = seq(0, 350, by = 20)) +
  labs(x="Number of publications", y=" Country of corresponding author") +
  theme_pubclean(base_size = 26) +
  theme(panel.grid.major.x = element_line(linetype = "dotted", size = 1, color = "grey"),
        #panel.grid.minor.x = element_line(linetype = "dotted", size = 1, color = "grey"),
        panel.grid.major.y = element_blank(),
        axis.title.x = element_text(hjust = 1, face = "bold", margin = margin(t = 10, r = 0, b = 10, l = 0), size = 24),
        axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0), size = 24),
        axis.text = element_text(color = "#464a62"))

fig5

# save image
ggsave(here::here("figures", paste0("fig5", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
       type = "cairo", width = 24, height = 12, dpi = 320, compression = "lzw")

