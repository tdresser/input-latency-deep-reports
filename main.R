library(tidyverse)
library(plotly)
source("breakdown_colors.R")

myplotly <- function(plot, ...) {
  xlab <- paste("&nbsp;<br>", plot$labels$x ,sep="")
  ylab <- paste(plot$labels$y, "<br>&nbsp;", sep="")
  return(ggplotly(plot + labs(x = xlab, y = ylab))) %>% 
    layout(margin=list(l=180, b=-30)) 
}   

all_data <- bind_rows(
  read_csv("all_data/move_queueing", col_names=FALSE) %>% mutate(type="touchmove queueing all"),
  read_csv("all_data/move_handling", col_names=FALSE) %>% mutate(type="touchmove handling all"),
  read_csv("all_data/start_handling", col_names=FALSE) %>% mutate(type="touchstart handling all"),
  read_csv("worst_10_percent/move_queueing", col_names=FALSE) %>% mutate(type="touchmove queueing worst"),
  read_csv("worst_10_percent/move_handling", col_names=FALSE) %>% mutate(type="touchmove handling worst"),
  read_csv("worst_10_percent/start_handling", col_names=FALSE) %>% mutate(type="touchstart handling worst")
) %>% 
  select(method=X1, duration=X2, type=type) %>%
  group_by(type) %>%
  arrange(desc(duration))

top_methods <- all_data %>% 
  group_by(method) %>% 
  summarize(duration=sum(duration)) %>% 
  arrange(desc(duration)) %>%
  slice(1:19)

top_data <- all_data %>% semi_join(top_methods, by=c("method"))
other_data <- all_data %>% anti_join(top_methods, by=c("method")) %>%
  summarize(duration=sum(duration)) %>%
  mutate(method="other")

all_data <- bind_rows(top_data, other_data)

all_data$type <- factor(all_data$type, levels=c(
  "touchmove queueing all",
  "touchmove queueing worst",
  "touchmove handling all",
  "touchmove handling worst",
  "touchstart handling all",
  "touchstart handling worst"
))

methods_by_volume <- all_data %>% 
  group_by(method) %>%
  summarize(duration = sum(duration)) %>% 
  mutate(other_override = method=="other") %>%
  arrange(other_override, desc(duration)) 

all_data$method <- factor(all_data$method, levels=methods_by_volume$method)

all_data_plot <- ggplot(all_data, aes(fill=method, y=duration, x=type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=breakdown_colors) +
  coord_flip() +
  ylab("Duration (ms)") + xlab("") +
  ggtitle("Average contributions of methods to touch latency")

myplotly(all_data_plot)

normalized_data <- all_data %>% 
  group_by(type) %>% mutate(duration = duration / sum(duration))

normalized_data_plot <- (all_data_plot + ylab("Duration (%)")) %+% normalized_data
myplotly(normalized_data_plot)
  