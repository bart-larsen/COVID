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
# setwd('/Users/larsenb/Documents/BenProjects/covid19/')

## Read in data
covid_data <- read.csv('us-counties.csv')
covid_data$date<-as.Date(covid_data$date)
population_data <- read.csv('co-est2019-alldata.csv')
population_data$CTYNAME <- str_replace(population_data$CTYNAME," County","")
population_data$CTYNAME[population_data$CTYNAME=="New York"&population_data$COUNTY==61]= "New York City"
covid_data <- covid_data %>% 
  filter(date > as.Date("2020-02-29")) %>%
  left_join(population_data %>% select(STNAME,CTYNAME,POPESTIMATE2019),by = c("state"="STNAME","county" = "CTYNAME")) %>%
  unite(state_county,c(county,state),sep = ", ",remove = FALSE)

covid_data$population <- covid_data$POPESTIMATE2019

covid_data <- covid_data %>%
  mutate(cases_per_capita = cases/population) %>%
  mutate(deaths_per_capita = deaths/population) %>%
  mutate(death_rate = deaths/cases)

drv <- function(x) c(NA, diff(x))
# first_date <- function(x,y) min(x[y==min(y)],na.rm=T)
case_threshold <- 10 #Controls when to start counting days
first_date <- function(x,y) min(x[y>=case_threshold],na.rm=T)

covid_data <- covid_data %>%
  group_by(state_county) %>%
  mutate(daily_change_per_capita = drv(cases_per_capita)) %>%
  mutate(daily_change = drv(cases)) %>%
  mutate(daily_change_deaths_per_capita = drv(deaths_per_capita)) %>%
  mutate(daily_change_deaths = drv(deaths)) %>%
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
    checkboxInput("per_cap_plots","Plot as per capita?",value = T)
  ),
  
  mainPanel(
    fluidRow(
      titlePanel("Populations of selected counties"),
      plotOutput("pop_plot"),
      titlePanel("Progression in Cases and Deaths by Date"),
      plotOutput("date_plot"),
      titlePanel(sprintf("Progression in Cases and Deaths by Days since %s Cases",case_threshold)),
      plotOutput("elapsed_plot"),
      titlePanel("Deaths per Case"),
      plotOutput("death_rate_plot")
      )
    
    )
)

server <- function(input, output,session) {
  
  output$pop_plot <- renderPlot({
    covid_data <- covid_data %>% 
      filter(state_county %in% input$state_county)
    
    ggplot(data=covid_data,aes(x = reorder(state_county,-population), y = population,fill = state_county)) +
      geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +
      scale_fill_brewer(type = "qual", palette = "Dark2") +
      ylab("Population") + xlab("County")
  })
  
  output$date_plot <- renderPlot({
    covid_data <- covid_data %>% 
      filter(state_county %in% input$state_county)
    if (input$per_cap_plots==T) {
      covid_data <- covid_data %>%
        mutate(case_var = cases_per_capita, daily_change_var = daily_change_per_capita,
               death_var = deaths_per_capita, daily_change_death_var = daily_change_deaths_per_capita)
      label_suffix <- "(per capita)"
    } else {
      covid_data <- covid_data %>%
        mutate(case_var = cases, daily_change_var = daily_change,
               death_var = deaths, daily_change_death_var = daily_change_deaths)
      label_suffix <- ""
    }

    cases <- ggplot(data = covid_data,aes(x=date,y=case_var,fill=state_county,color = state_county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Cases %s",label_suffix),x="Date",title = "COVID-19 cases")
    
    
    dtCases <- ggplot(data = covid_data,aes(x=date,y=daily_change_var,color = state_county)) +
      geom_point(alpha = .75) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Daily Change %s",label_suffix),x="Date",title = "Change in COVID-19 cases")
    
    deaths <- ggplot(data = covid_data,aes(x=date,y=death_var,fill=state_county,color = state_county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Deaths %s",label_suffix),x="Date",title = "COVID-19 deaths")
    
    dtDeaths <- ggplot(data = covid_data,aes(x=date,y=daily_change_death_var,color = state_county)) +
      geom_point(alpha = .5) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Daily Change %s",label_suffix),x="Date",title = " Change in COVID-19 deaths")
    
    
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
    
    if (input$per_cap_plots==T) {
      covid_data <- covid_data %>%
        mutate(case_var = cases_per_capita, daily_change_var = daily_change_per_capita,
               death_var = deaths_per_capita, daily_change_death_var = daily_change_deaths_per_capita)
      label_suffix <- "(per capita)"
    } else {
      covid_data <- covid_data %>%
        mutate(case_var = cases, daily_change_var = daily_change,
               death_var = deaths, daily_change_death_var = daily_change_deaths)
      label_suffix <- ""
    }
    
    cases <- ggplot(data = covid_data,aes(x=days_since_first_case,y=case_var,fill=state_county,color = state_county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Cases %s",label_suffix),x=sprintf("Days since %s cases",case_threshold),title = "COVID-19 cases")+
      xlim(1,NA)
    
    
    dtCases <- ggplot(data = covid_data,aes(x=days_since_first_case,y=daily_change_var,color = state_county)) +
      geom_point(alpha = .75) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Daily Change %s",label_suffix),x=sprintf("Days since %s cases",case_threshold),title = "Change in COVID-19 cases")+
      xlim(1,NA)
    
    deaths <- ggplot(data = covid_data,aes(x=days_since_first_case,y=death_var,fill=state_county,color = state_county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Deaths %s",label_suffix),x=sprintf("Days since %s cases",case_threshold),title = "COVID-19 deaths")+
      xlim(1,NA)
    
    dtDeaths <- ggplot(data = covid_data,aes(x=days_since_first_case,y=daily_change_death_var,color = state_county)) +
      geom_point(alpha = .5) + geom_line(alpha = .5) + geom_smooth(method = "loess",formula = "y ~x",se = F) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y =sprintf("Daily Change %s",label_suffix),x=sprintf("Days since %s cases",case_threshold),title = " Change in COVID-19 deaths")+
      xlim(1,NA)
    
    
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
    
    
    death_rate <- ggplot(data = covid_data,aes(x=days_since_first_case,y=death_rate,fill=state_county,color = state_county)) +
      geom_point() + geom_line() +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(y = sprintf("Deaths per Case"),x=sprintf("Days since %s cases",case_threshold),title = "COVID-19 cases")+
      xlim(1,NA)
    death_rate
    
  })
  
}

shinyApp(ui = ui, server = server)

