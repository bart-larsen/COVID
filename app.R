# Impact Factors
# This app displays impact factor information for
# commonly used neuroscience journals

## Libraries
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(cowplot)
library(scales)
library(lubridate)
library(gganimate)
library(gifski)
library(png)
theme_set(theme_classic(base_size = 10))
setwd('/Users/larsenb/Documents/BenProjects/covid19/')

## Read in data
covid_data <- read.csv('/Users/larsenb/Documents/BenProjects/covid19/covid-19-data/us-counties.csv')
covid_data$date<-as.Date(covid_data$date)
population_data <- read.csv('co-est2019-alldata.csv')
population_data$CTYNAME <- str_replace(population_data$CTYNAME," County","")
covid_data <- covid_data %>% 
  filter(date > as.Date("2020-02-29")) %>%
  left_join(population_data %>% select(STNAME,CTYNAME,POPESTIMATE2019),by = c("state"="STNAME","county" = "CTYNAME")) %>%
  unite(state_county,c(county,state),sep = ", ",remove = FALSE)

covid_data$population <- covid_data$POPESTIMATE2019

covid_data <- covid_data %>%
  mutate(cases_per_capita = cases/population) %>%
  mutate(deaths_per_capita = deaths/population)

drv <- function(x) c(NA, diff(x))
first_date <- function(x,y) min(x[y==min(y)],na.rm=T)

covid_data <- covid_data %>%
  group_by(state_county) %>%
  mutate(daily_change = drv(cases_per_capita)) %>%
  mutate(daily_change_deaths = drv(deaths_per_capita)) %>%
  mutate(days_since_first_case = as.numeric(date - first_date(date,cases)))

recent_date = max(covid_data$date)
cat(sprintf("\nMost recent data: %s\n",recent_date))

## select the counties

## App layout and functions
ui <- fluidPage(
  # Application title
  titlePanel("COVID-19: Daily progression"),
  
  inputPanel(
    h6('Select the counties you would like information for.'),
    pickerInput("state_county","Select County",sort(unique(covid_data$state_county)),multiple = T,
                selected = c("Illinois Cook","Pennsylvania Philadelphia"),
                options = pickerOptions(actionsBox = TRUE,
                                        liveSearch = TRUE)),
    checkboxInput("anim_plot","Animated plots (be patient if yes)")
  ),
  
  mainPanel(
    fluidRow(
      titlePanel("Populations of selected counties"),
      plotOutput("pop_plot"),
      titlePanel("Progression in Cases and Deaths per Capita by Date"),
      plotOutput("date_plot"),
      titlePanel("Progression in Cases and Deaths per Capita by Day of First Case"),
      plotOutput("elapsed_plot")
      ),
    imageOutput("case_anim")
    
    )
)

server <- function(input, output,session) {
  
  output$pop_plot <- renderPlot({
    covid_data <- covid_data %>% 
      filter(state_county %in% input$state_county)
    
    ggplot(data=covid_data,aes(x = reorder(county,-population), y = population,fill = county)) +
      geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +
      scale_fill_brewer(type = "qual", palette = "Dark2") +
      ylab("Population") + xlab("County")
  })
  
  output$date_plot <- renderPlot({
    covid_data <- covid_data %>% 
      filter(state_county %in% input$state_county)

    cases <- ggplot(data = covid_data,aes(x=date,y=cases_per_capita,fill=county,color = county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Cases per capita",x="Date",title = "COVID-19 cases")
    
    
    dtCases <- ggplot(data = covid_data,aes(x=date,y=daily_change,color = county)) +
      geom_point(alpha = .75) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Daily change (per capita)",x="Date",title = "Change in COVID-19 cases")
    
    deaths <- ggplot(data = covid_data,aes(x=date,y=deaths_per_capita,fill=county,color = county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Deaths per capita",x="Date",title = "COVID-19 deaths")
    
    dtDeaths <- ggplot(data = covid_data,aes(x=date,y=daily_change_deaths,color = county)) +
      geom_point(alpha = .5) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Daily change (per capita)",x="Date",title = " Change in COVID-19 deaths")
    
    
    # compile into one figure
    combo_plot <- cowplot::plot_grid(
      cases +  theme(legend.position="none"), 
      dtCases + theme(legend.position="none"), 
      deaths + theme(legend.position="none"), 
      dtDeaths + theme(legend.position="none"), 
      labels = "AUTO",nrow = 2)
    
    county_legend <- cowplot::get_legend(
      cases + 
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
    )
    
    combo_plot <- plot_grid(combo_plot, county_legend, ncol = 1, rel_heights = c(1, .1))
    combo_plot
    # gt <- ggplotGrob(g)
    # gt$layout$clip[gt$layout$name == "panel"] <- "off"
    # grid.draw(combo_plot)

  })
  
  output$elapsed_plot <- renderPlot({
    covid_data <- covid_data %>% 
      filter(state_county %in% input$state_county)
    
    cases <- ggplot(data = covid_data,aes(x=days_since_first_case,y=cases_per_capita,fill=county,color = county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Cases per capita",x="Days since first case",title = "COVID-19 cases")
    
    
    dtCases <- ggplot(data = covid_data,aes(x=days_since_first_case,y=daily_change,color = county)) +
      geom_point(alpha = .75) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Daily change (per capita)",x="Days since first case",title = "Change in COVID-19 cases")
    
    deaths <- ggplot(data = covid_data,aes(x=days_since_first_case,y=deaths_per_capita,fill=county,color = county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Deaths per capita",x="Days since first case",title = "COVID-19 deaths")
    
    dtDeaths <- ggplot(data = covid_data,aes(x=days_since_first_case,y=daily_change_deaths,color = county)) +
      geom_point(alpha = .5) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = "Daily change (per capita)",x="Days since first case",title = " Change in COVID-19 deaths")
    
    
    # compile into one figure
    combo_plot <- cowplot::plot_grid(
      cases +  theme(legend.position="none"), 
      dtCases + theme(legend.position="none"), 
      deaths + theme(legend.position="none"), 
      dtDeaths + theme(legend.position="none"), 
      labels = "AUTO",nrow = 2)
    
    county_legend <- cowplot::get_legend(
      cases + 
        guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
    )
    
    combo_plot <- plot_grid(combo_plot, county_legend, ncol = 1, rel_heights = c(1, .1))
    combo_plot
    # gt <- ggplotGrob(g)
    # gt$layout$clip[gt$layout$name == "panel"] <- "off"
    # grid.draw(combo_plot)
    
  })
  
  
  output$case_anim <- renderImage({
    if (input$anim_plot ==TRUE) {
      covid_data <- covid_data %>% 
        filter(state_county %in% input$state_county)
      cases <- ggplot(data = covid_data,aes(x=date,y=cases_per_capita,fill=county,color = county)) +
        geom_point() + geom_line() +
        scale_color_brewer(type = "qual", palette = "Dark2") +
        labs(y = "Cases per capita",x="Days since first case",title = "COVID-19 cases")
      anim <- cases + transition_reveal(date)
      anim_save("cases_animation.gif",animation = anim,renderer = gifski_renderer())
      list(src = "cases_animation.gif",
           contentType = 'image/gif'
           # width = 400,
           # height = 300,
           # alt = "This is alternate text"
      )
    }
  })
  

  # output$yearplot <- renderPlot({
  #   yearTable <- ImpactTable %>%
  #     filter(Year == input$Year)%>%
  #     filter(Journal %in% input$Journal)
  #   
  #   ggplot(data = yearTable, aes(x = reorder(Journal,Journal.Impact.Factor), y = Journal.Impact.Factor,Group = Journal)) + 
  #     geom_bar(stat = "identity",aes(fill = Journal)) + coord_flip() + 
  #     geom_text(aes(label=Journal.Impact.Factor),hjust=1) +
  #     theme_classic() + theme(legend.position = "none")+
  #     theme(axis.title.y=element_blank(),axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12),axis.title.x = element_text(size = 12)) +
  #     ylab("Impact Factor") + ggtitle(sprintf("Ranking for year %s",input$Year))
  # })
  
}

shinyApp(ui = ui, server = server)

