library(ggplot2)
library(tidyverse)
library(tidytuesdayR)
library(gganimate)
library(gapminder)
library(santoku)
library(ggbump)
tuesdata <- tidytuesdayR::tt_load('2022-05-31')
poll <- tuesdata$poll
reputation <- tuesdata$reputation
attach(poll)
attach(reputation)
mydata <- poll |> 
  mutate(Tech.Company =case_when(
    company=="Samsung"~ company, 
    company==" Sony" ~ company,
    company=="IBM" ~ company,
    company=="Microsoft" ~ company,
    company=="Apple" ~ company,
    company=="Netflix" ~ company,
    company=="LG Corporation" ~ company,
    company=="Google" ~ company,
    company=="Dell" ~ company,
    company=="Electronic Arts" ~ company,
    company=="Robinhood" ~ company,
    company=="Twitter" ~ company,
    company=="Facebook" ~ company, 
    company=="TikTok" ~ company,
    
  )
  )
View(mydata)


mydata <- drop_na(mydata)

View(mydata)

my.color <- c("lightseagreen","navajowhite1","darkseagreen4","darkmagenta","lightblue","palegreen","goldenrod1","mediumvioletred","peachpuff2","firebrick1","khaki","turquoise1","black")
ggplot(mydata, aes(year,rank, color = Tech.Company)) +
  geom_point(size = 1) +
  geom_bump(size=2)+
  scale_colour_manual(values = my.color)+
  theme(legend.position = "right",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = .5, color = "white"),
        plot.caption = element_text(hjust = 1, color = "white", size = 8),
        plot.subtitle = element_text(hjust = .5, color = "white", size = 10),
        axis.line = element_line(size = 0.5, colour = "white", linetype=1),
        axis.ticks = element_blank(),
        axis.text.y = element_text(face = 2, color = "white"),
        axis.title.y = element_text(face = 2, color = "white"),
        axis.title.x = element_text(face = 2, color = "white"),
        axis.text.x = element_text(face = 2, color = "white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"))+
  labs(title = "Technology Reputation Rankings",
       subtitle = "Reputation ranking is based on the Axios Harris Poll 100.",
       caption = "#TidyTuesday week 22\n Data source: Axios Harris Poll 100 |
@JavanmardiSahar")


ggsave("tidytuesday_week_22.png",
       height = 7,
       width = 9,
       dpi=320)


