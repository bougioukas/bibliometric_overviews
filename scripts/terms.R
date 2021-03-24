# load the packages ---------------------------------------------
library(here)
library(tidyverse)
library(ggtext)



# load the dataset 'bmx' ---------------------------------------------

library(readxl)
bmx <- read_excel(here("data", "bmx.xlsx"))



# count terms with id for the circular graph
data_cnt <- bmx %>% 
  count(TT, name = "value", sort = T) %>% 
  filter(value >5, TT != "NA") %>% 
  mutate(id = seq(1,34))



# the graph ---------------------------------------------

#setting up text to go in the middle of the circle
center_text <- tibble(
  labelz = "**<p style='color:black;font-size:18pt'>Terminology in title
  </span><br><span style='font-size:16pt'>*<span style='color:#6C8036'>Height of the bars reprensets counts.</span> </span><br><span style='font-size:14pt'><span style='color:gray40'>(Terms with counts <6 are not shown in the graph)</span>*</p>**",
  x = 0,
  y2 = -170
  )


# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- data_cnt

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar #substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle <- ifelse(angle < -90, angle + 180, angle - 5)
# ----- ------------------------------------------- ---- #
line_color <- "grey50"

# Start the plot
fig1 <- ggplot(data_cnt, aes(x = as.factor(id), y = value)) +   # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", fill = alpha("#a2b66d", 0.7), width = 0.8) +    # This add the bars with a blue color
  # Add text showing the value of each 0/100/200/300 line
  annotate("text", x = c(34.2, 33.7,33.9,34), y = c(0, 100, 200, 300), label = c("0","100", "200", "300") ,
           color="#3D9970", size=4 , angle = 0, fontface="bold", hjust= -1.0, vjust= -0.7) +
  geom_hline(aes(yintercept = (100)), linetype = 3, size = 0.8, color = line_color, alpha = 1.0) +
  geom_hline(aes(yintercept = (200)),linetype = 3,  size = 0.8, color = line_color, alpha = 1.0) +
  geom_hline(aes(yintercept = (300)), linetype = 3, size = 0.8, color = line_color, alpha = 1.0) +
  ylim(-180, 320) +  # The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  geom_textbox(aes(x = x, y = y2, label = labelz),
               data = center_text,
               fill = NA, color = NA,
               width = unit(75, "mm"),
               size = 6,
               hjust = .45) +
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 4), "cm") # Adjust the margin to make in sort labels are not truncated
  ) +
  coord_polar() +  # This makes the coordinate polar instead of cartesian
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(aes(x=id, y=0, label=TT, hjust=hjust), data=label_data, 
            color="black", alpha=0.8, size=5.5, angle= label_data$angle, inherit.aes = FALSE ) 

fig1


# save image
#ggsave(here::here("figures", paste0("fig1", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tiff")), 
  #device = "tiff", type = "cairo", width = 13, height = 10.5, dpi = 460, compression = "lzw")

