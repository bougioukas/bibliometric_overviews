# load the packages ---------------------------------------
library(here) # A Simpler Way to Find Your Files
library(tidyverse) # Easily Install and Load the 'Tidyverse'

library(countrycode) # Convert Country Names and Country Codes
library(ggtext) # Improved Text Rendering Support for 'ggplot2'



# load the dataset --------------------------------------------------------------------------
library(readxl) # Read Excel Files
bmx <- read_excel(here("data", "bmx.xlsx"))



# prepare the data --------------------------------------------------------------------------

# add country codes to the database
bmx_codes <- bmx %>% 
  mutate(code = countrycode(CO, "country.name.en", "iso3c"))

# dataframe with continents
clist <- codelist %>%
  select(country.name.en, continent, iso3c) %>%
  rename(Country = country.name.en, Continent = continent, code = iso3c) %>% 
  filter_at(vars(Continent, code), all_vars(!is.na(.)))

# join the two dataframes by code
dframe <- bmx_codes %>%
  left_join(clist, by = "code")

# counts by publication year and continent
df1 <- dframe %>% 
  count(PY, Continent, name = "COUNTS") %>% 
  complete(PY, Continent, fill = list(COUNTS = 0))


# counts by publication year
df2 <- bmx %>% 
  count(PY, name = "COUNTS2")




# theme and colors ------------------------------------------------------------------------------

my_theme <- theme(text = element_text(size = 18),
                plot.margin = margin(35, 25, 15, 35),
                axis.text.x = element_text(color = "grey60", margin = margin(t = 4)),
                axis.ticks.x = element_line(color = "grey60"),
                axis.ticks.length.x = unit(.4, "lines"),
                axis.text.y = element_text(color = "grey60", margin = margin(t = 4)),
                axis.title.x = element_text(face = "bold", margin = margin(t = 10, r = 0, b = 10, l = 0), size = 20),
                axis.title.y = element_text(face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0), size = 20),
                axis.text = element_text(size = 16),
                legend.title = element_text(size = 20, face="bold"),
                legend.text=element_text(size= 20),
                legend.key.size = unit(1.6,"line"),
                panel.grid.major.y= element_line(color = "grey70", linetype = "dotted"),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank()
                )


my_colors <- c("#AA5656", "#B7B7B7", "#FDC266", "#9FA368", "#4E86A1")





# create the figure------------------------------------------------------------------------

fig2 <- ggplot(df1, aes(x = PY, y = COUNTS, fill = Continent)) +
  geom_col(size = 0.1, color = "grey50", width = 0.70, alpha=0.9) +
  geom_text(aes(x = PY, y = COUNTS2, label = COUNTS2), data = df2,
            hjust = 0.5, vjust = -0.4, 
            color = "black", fontface = "bold", size = 6.0, inherit.aes = F) +
  theme_minimal(base_size = 18) +
  scale_x_continuous(expand = c(0.008, 0), breaks = seq(2000, 2020, by = 1)) +
  scale_y_continuous(expand = c(0.008, 0), limits=c(0,320), breaks=seq(0, 320, 50)) +
  scale_fill_manual(values = my_colors) +
  my_theme +
  labs(x="Year of publication", y="Number of publications") 

fig2

# save image tiff
#ggsave(here::here("figures", paste0("fig2", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
  #device = "tiff", type = "cairo", width = 18, height = 10, dpi = 380, compression = "lzw")

