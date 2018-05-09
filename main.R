library(tidyverse)
library(plotly)
source("breakdown_colors.R")

get_data <- function(folder) {
  move_queueing <- read_csv(paste(folder, "/move_queueing", sep=""), col_names=FALSE) %>% mutate(type="touchmove queueing")
  move_handling <- read_csv(paste(folder, "/move_handling", sep=""), col_names=FALSE) %>% mutate(type="touchmove handling")
  start_handling <- read_csv(paste(folder, "/start_handling", sep=""), col_names=FALSE) %>% mutate(type="touchstart handling")
  
  df <- bind_rows(move_queueing, move_handling, start_handling) %>% 
    select(method=X1, duration=X2, type=type)
  
  return(df)
}

all_data <- get_data("all_data") %>% 
  group_by(type) %>%
  arrange(desc(duration))

non_other_count <- 20

top_data <- all_data %>%
  slice(1:min(n(), non_other_count - 1))

other_data <- all_data %>% 
  slice(non_other_count:n()) %>%
  summarize(duration=sum(duration)) %>%
  mutate(method="other")

all_data <- bind_rows(top_data, other_data)

all_data$type <- as.factor(all_data$type)

# TODO - summarize better
methods_by_volume <- all_data %>% 
  filter(type == "touchmove queueing")

all_data %>% filter(type == "touchmove queueing") %>% select(method)
all_data$method <- factor(all_data$method, levels=rev(methods_by_volume$method))

ggplot(all_data, aes(fill=method, y=duration, x=type)) +
  geom_col() +
  scale_fill_manual(values=breakdown_colors)
  