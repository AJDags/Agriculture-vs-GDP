### Importing Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(paletteer)
library(scales)
library(showtext)
# library(gganimate)

### Importing Datasets
Agriculture <- Agriculture_Contribution
GDP_Growth <- GDP

### Data Cleaning
# Agriculture Data
Phil_Agri <- Agriculture %>% 
  # select(-Indicator Code) %>% 
  pivot_longer(
    c(-`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`),
    names_to = "Year",
    values_to = "Contribution"
  ) %>% 
  drop_na('Contribution') %>% 
  filter(`Country Name`== "Philippines") %>% 
  select(
    !starts_with("Indicator"),
    -`Country Code`
  ) %>% 
  mutate(Contribution = Contribution * 100000000)
  
# GDP Growth Data
Phil_GDP <- GDP %>% 
  # select(-Indicator Code) %>% 
  pivot_longer(
    c(-`Country Name`, -`Country Code`, -`Indicator Name`, -`Indicator Code`),
    names_to = "Year",
    values_to = "GDP"
  ) %>% 
  drop_na('GDP') %>% 
  filter(`Country Name`== "Philippines") %>% 
  select(
    !starts_with("Indicator"),
    -`Country Code`
  )

### Joining Tables for final DF

Phil_Agri_Phil <- Phil_GDP %>% 
  full_join(Phil_Agri, by = c("Country Name", "Year")) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  pivot_longer(
    c(GDP, Contribution),
    names_to = "Indicators",
    values_to = "Dollars"
  )

### Visualizing Data

font <- "Copse"
font_add_google(family=font, font)
theme_set(theme_minimal(base_family = font))
bg <- "#EEE8CD"
txt_col <- "#333333"
showtext_auto(enable = TRUE)
col <- c("#E5803B", "#53868B")


Phil_Agri_Phil %>% 
    ggplot() +
    geom_line(aes(x = Year, y = Dollars, color = Indicators), size = 1)+
    geom_text(data=Phil_Agri_Phil %>% filter(Year =="2023"),
            aes(x = max(Year) + .5, y = Dollars, label=Dollars/1000000000, color=Indicators),
            hjust = 0,
            vjust = 0.5,
            size=3,
            family=font) +
    scale_y_log10(labels = label_comma()) +
    scale_color_manual(values = col) +
    labs(
      x = "Year",
      y = "Amount in Dollars ($)",
      title = "GDP vs Agriculture Sector",
      subtitle = "A growing gap between GDP Growth and Agriculture Sector's Contribution to GDP\nfrom 1961 to 2023 in Hundred Billion Dollars ($)",
      caption = "AdrianJamesDagatan | Source: Worldbank Indicators Datasets"
      ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(color=txt_col, size=9),
    axis.title.y  = element_text(color=txt_col, size=10, hjust=.5),
    axis.title.x  = element_text(color=txt_col, size=10, hjust=.5),
    axis.line = element_line(),
    plot.title = element_text(hjust=0.6,size=22, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,10,0)),
    plot.subtitle = element_text(hjust=0.6,size=10, color=txt_col, margin=margin(0,0,20,0)),
    plot.caption = element_text(hjust=-.5, margin=margin(20,0,0,0), size=8, color=txt_col, face="italic"),
    plot.tag = element_text(face = "italic"),
    plot.title.position = "plot",
    plot.background = element_rect(color=bg, fill=bg),
    plot.margin = margin(10,60,20,20),
    legend.position = "bottom",
    legend.title = element_blank()
  ) 

# Save
showtext_opts(dpi = 320) 

ggsave("Agriculture vs GDP.png",
       height = 8.8,
       width = 8,
       dpi=320,
       
)

showtext_auto(FALSE)    
