library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmosaic)
library(ggthemes)
library(gridExtra)

Dt <- read.csv('Mosaic Plot.csv')
head(Dt)

# Set variables as factors
Dt$Income.Level <- factor(Dt$Income.Level, levels = c('Unknown', '$50,354 - $63,332', '$40,227 - $50,353', '< $40,227', '>= $63,333'))

Dt$Facility.Type <- ifelse(Dt$Facility.Type == 'Integrated Network Cancer Program', 'Integrated Network \n Cancer Program', 
                           ifelse(Dt$Facility.Type == 'Community Cancer Program', 'Community Cancer \n Program', 
                                  ifelse(Dt$Facility.Type == 'Academic/Research Program', 'Academic/Research \n Program', Dt$Facility.Type)))
Dt$Facility.Type <- factor(Dt$Facility.Type, levels = c('Integrated Network \n Cancer Program', 'Community Cancer \n Program', 'Academic/Research \n Program'))

# Version 1.1: without legend
p1.1 <-
  ggplot(data = Dt) +
  geom_mosaic(aes(x = product(Income.Level,     # y-axis: Income level
                              Facility.Type),   # x-axis: Facility type
                  fill = Income.Level),         # Color of "box" specified by income level
              show.legend = FALSE) +            # Text in y-axis has already showed label so don't need legend
  xlab('Facility Type') + ylab('Neighborhood \n Income Level') + 
  scale_fill_tableau() +                        # Use better color palette
  theme_mosaic() +                              # Specific theme for mosaic plot
  theme(axis.ticks.x = element_blank(),         # Remove ticks in x and y axes               
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(color = 'black', face = 'bold'), 
        axis.title.y = element_text(color = 'black', vjust = 3, face = 'bold'),
        axis.text.x = element_text(color = 'black', angle = 45, hjust = 1), 
        axis.text.y = element_text(color = 'black'))

# Version 1.2: with legend
p1.2 <-
  ggplot(data = Dt) +
  geom_mosaic(aes(x = product(Income.Level,     # y-axis: Income level
                              Facility.Type),   # x-axis: Facility type
                  fill = Income.Level)) +         # Color of "box" specified by income level
  xlab('Facility Type') + ylab('Neighborhood \n Income Level') + 
  guides(fill = guide_legend(title = "Neighborhood \n Income Level")) +  # Modify legend title
  scale_fill_tableau() +                        # Use better color palette
  theme_mosaic() +                              # Specific theme for mosaic plot
  theme(axis.ticks.x = element_blank(),         # Remove ticks in x and y axes               
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(color = 'black', face = 'bold'), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = 'black', angle = 45, hjust = 1), 
        axis.text.y = element_blank(),
        legend.title = element_text(color = 'black', face = 'bold'))

grid.arrange(p1.1, p1.2, nrow = 1)

# Factor for Status variable
Dt$Status <- factor(Dt$Status, levels = c('Death', 'Alive'))

# Version 2
p2 <-
  ggplot(data = Dt) +
  geom_mosaic(aes(x = product(Income.Level,      # y-axis: Income level
                              Facility.Type),    # x-axis: Facility type
                  fill = Income.Level),          # Color of "box" specified by income level
              offset = 0.02) +                   # More space between boxes
  facet_grid( ~ Status) +                        # Facet by survival status
  xlab('Facility Type')  + 
  guides(fill = guide_legend(title = "Neighborhood \n Income Level")) +  # Modify title of legend
  scale_fill_tableau() +                        # Use better color palette
  theme_mosaic() +                              # Specific theme for mosaic plot
  theme(axis.ticks.x = element_blank(),         # Remove ticks in x and y axes               
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(color = 'black', face = 'bold'), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = 'black', angle = 45, hjust = 1), 
        axis.text.y = element_blank(),
        legend.title = element_text(color = 'black', face = 'bold'), 
        strip.text = element_text(size = 10, color = 'black', face = 'bold'),
        strip.background = element_blank())
p2

# Combined version 
Title <- c(`Death` = 'Death', `Alive` = 'Alive', `(all)` = 'Total')

p3 <- 
  ggplot(data = Dt) +
  geom_mosaic(aes(x = product(Income.Level,     # y-axis: Income level
                              Facility.Type),     # x-axis: Facility type
                  fill = Income.Level),          # Color of "box" specified by income level
              offset = 0.02) +                   # More space between boxes
  facet_grid( ~ Status, margins = T, labeller = as_labeller(Title)) +
  xlab('Facility Type')  + 
  guides(fill = guide_legend(title = "Neighborhood \n Income Level")) + 
  scale_fill_tableau() +                        # Use better color palette
  theme_mosaic() +                              # Specific theme for mosaic plot
  theme(axis.ticks.x = element_blank(),         # Remove ticks in x and y axes               
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(color = 'black', face = 'bold'), 
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = 'black', angle = 45, hjust = 1), 
        axis.text.y = element_blank(),
        legend.title = element_text(color = 'black', face = 'bold'), 
        strip.text = element_text(size = 10, color = 'black', face = 'bold'),
        strip.background = element_blank())

p3

# Version 3
p3 <-
  ggplot(data = Dt) +
  geom_mosaic(aes(x = product(Income.Level,      # y-axis: Income level
                              Facility.Type),    # x-axis: Facility type
                  fill = Income.Level, 
                  conds = product(Status)),       # Color of "box" specified by income level
              offset = 0.025) +                   # More space between boxes
  annotate(geom = "text", x = 0.25, y = -0.1, label = "Death") +  # Change text for x axis
  annotate(geom = "text", x = 0.75, y = -0.1, label = "Alive") + 
  annotate(geom = "text", x = 0.50, y = -0.2, label = "Survival Status", fontface = "bold") + # x-axis label
  ylab('Facility Type')  + 
  guides(fill = guide_legend(title = "Neighborhood \n Income Level")) +  # Modify title of legend
  scale_fill_tableau() +                        # Use better color palette
  theme_mosaic() +                              # Specific theme for mosaic plot
  theme(axis.ticks.x = element_blank(),         # Remove ticks in x and y axes               
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(color = 'black', face = 'bold', vjust = 5),
        axis.text.x = element_blank(), 
        axis.text.y = element_text(color = 'black'),
        legend.title = element_text(color = 'black', face = 'bold'))
p3









