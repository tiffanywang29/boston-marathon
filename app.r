library(shiny)
library(readr)
library(ggmap)
library(maps)
library(rvest)
library(tidyr)
library(stringr)
library(ggplot2)
library(shinythemes)
library(dplyr)

results_2001 <- read_csv("./results_2001.csv")
results_2002 <- read_csv("./results_2002.csv")
results_2003 <- read_csv("./results_2003.csv")
results_2004 <- read_csv("./results_2004.csv")
results_2005 <- read_csv("./results_2005.csv")
results_2006 <- read_csv("./results_2006.csv")
results_2007 <- read_csv("./results_2007.csv")
results_2008 <- read_csv("./results_2008.csv")
results_2009 <- read_csv("./results_2009.csv")
results_2010 <- read_csv("./results_2010.csv")
results_2011 <- read_csv("./results_2011.csv")
results_2012 <- read_csv("./results_2012.csv")
results_2013 <- read_csv("./results_2013.csv")
results_2014 <- read_csv("./results_2014.csv")
results_2015 <- read_csv("./marathon_results_2015.csv")
results_2016 <- read_csv("./marathon_results_2016.csv")
results_2017 <- read_csv("./marathon_results_2017.csv")
all <- bind_rows(mutate(results_2001, year = "2001"), 
                 mutate(results_2002, year = "2002"),
                 mutate(results_2003, year = "2003"),
                 mutate(results_2004, year = "2004"),
                 mutate(results_2005, year = "2005"),
                 mutate(results_2006, year = "2006"),
                 mutate(results_2007, year = "2007"),
                 mutate(results_2008, year = "2008"),
                 mutate(results_2009, year = "2009"),
                 mutate(results_2010, year = "2010"),
                 mutate(results_2011, year = "2011"),
                 mutate(results_2012, year = "2012"))
all <- all %>%
  select(age, gender, city, state, country, year)

new <-bind_rows(
  (results_2013 %>%
     mutate(year = "2013") %>%
     select(age, gender, city, state, country, year)),
  (results_2014 %>%
     mutate(year = "2014") %>%
     select(age, gender, city, state, country, year)),
  (results_2015 %>%
     mutate(year = "2015") %>%
     select(age = Age, gender = "M/F", city = City, state = State, country = Country, year)),
  (results_2016 %>%
     mutate(year = "2016") %>%
     select(age = Age, gender = "M/F", city = City, state = State, country = Country, year)),
  (results_2017 %>%
     mutate(year = "2017") %>%
     select(age = Age, gender = "M/F", city = City, state = State, country = Country, year)))

all <- bind_rows(all, new)
colnames(all) <- c("Age", "Gender", "City", "State", "Country", "Year")


states <- map_data("state")
state_abb <- read_csv("Data/states.csv")
state_abb <- state_abb %>%
  mutate(State = tolower(State))
states <- left_join(states, state_abb, by = c("region" = "State"))
all_counts <- all %>%
  mutate(age.range = ifelse(Age %in% 18:34, "18-34", 
                            ifelse(Age %in% 34:39, "34-39",
                                   ifelse(Age %in% 40:44, "40-44",
                                          ifelse(Age %in% 45:49, "45-49",
                                                 ifelse(Age %in% 50:54, "50-54",
                                                        ifelse(Age %in% 55:59, "55-59",
                                                               ifelse(Age %in% 60:64, "60-64",
                                                                      ifelse(Age %in% 65:69, "65-69",
                                                                             ifelse(Age %in% 70:74, "70-74",
                                                                                    ifelse(Age %in% 75:79, "75-79", "80+"))))))))))) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Year, State, Gender, age.range) %>%
  summarise(counts = n())
states <- inner_join(states, all_counts, by = c("Abbreviation" = "State"))

summary_1 <- all %>%
  group_by(Year) %>%
  summarise(distinct_countries = n_distinct(Country))
summary_2 <- all %>%
  group_by(Year, Gender) %>%
  summarise(count = n())
summary_2 <- spread(summary_2, 
                    key = Gender, 
                    value = count)
summary <- left_join(summary_2, summary_1, by = c("Year" = "Year"))
summary <- mutate(summary, total = M + F)

country_counts <- all %>%
  group_by(Year, Country) %>%
  summarise(counts = n())

url <- "http://www.baa.org/races/boston-marathon/boston-marathon-history/participation.aspx"
tables <- url %>%
  read_html() %>%
  html_nodes(css = "table") 
participation <- html_table(tables[[2]])
colnames(participation) <- c("year", 
                             "total", 
                             "men", 
                             "women", 
                             "(F)_total", 
                             "(F)_men", 
                             "(F)_women")
participation <- participation %>%
  mutate(total = str_replace_all(total,",",""),
         men = str_replace_all(men,",",""),
         women = str_replace_all(women,",",""),
         year = as.numeric(year),
         total = as.numeric(total),
         men = as.numeric(men),
         women = as.numeric(women)) %>%
  filter(!(is.na(year)))


server <- function(input, output) {
  output$line <- renderPlot({
    req(input$genderline)
    data_react <- select(participation, 
                         input$genderline,
                         year) %>%
      filter(year <= input$animation)
    ymin <- min(as.vector(participation[input$genderline])) - 10
    ymax <- max(as.vector(participation[input$genderline])) + 100
    ggplot(data = data_react) +
      geom_line(aes_string(x = 'year', 
                           y = input$genderline,
                           group = 1)) +
      theme_bw() +
      xlim(1972, 2017) +
      ylim(ymin, ymax) 
  })
  
  
  output$map <- renderPlot({
    states_reactive <- eventReactive(input$action,
                                     {filter(states, 
                                             Gender %in% input$gender, 
                                             age.range %in% input$age, 
                                             Year >= input$year[1], 
                                             Year <= input$year[2])
                                     })
    if (dim(states_reactive())[1] > 0){
      ggplot(data = states_reactive()) + 
        geom_polygon(aes(x = long, 
                         y = lat, 
                         group = group, 
                         fill = log(counts)), 
                     color = "white") + 
        scale_fill_gradient(low = "#FFDD00", high = "#0004FF") +
        labs(fill = "Log of Runners") +
        coord_map() + 
        theme_void()
    }
  })
  
  output$ustable <- DT::renderDataTable({
    states_react <- eventReactive(input$action,
                                  {filter(states, 
                                          Gender %in% input$gender, 
                                          age.range %in% input$age, 
                                          Year >= input$year[1], 
                                          Year <= input$year[2]) %>%
                                      select("State" = region, 
                                             "Year" = Year, 
                                             "Gender" = Gender, 
                                             "Age Range" = age.range, 
                                             "Number of Runners" = counts)
                                  })
    DT::datatable(data = states_react(), rownames = FALSE)
  })
  
  output$table <- DT::renderDataTable({
    req(input$vars)
    data_react <- select(all, input$vars)
    DT::datatable(data = data_react, rownames = FALSE)
  })
  
  output$summary <- DT::renderDataTable({
    req(input$sumyear)
    summ_react <- filter(summary, Year %in% input$sumyear) %>%
      select("Year" = Year, 
             "Female" = "F", 
             "Male" = "M", 
             "Total Countries" = distinct_countries, 
             "Number of Runners" = total)
    DT::datatable(data = summ_react, rownames = FALSE)
  })
}

ui <- fluidPage(
  navbarPage("Boston Marathon", tags$h5("Kevin Choi and Tiffany Wang"), theme = shinythemes::shinytheme("united"),
             p("The", 
               tags$a(href="http://www.baa.org/", "Boston Marathon"), 
               "has been around for 122 years. Each Patriots' Day, celebrated on the third Monday of April, thousands of runners from around the world come together and run 26.2 miles. Here we have data on the runners who have participated in the more recent Boston Marathons. Let's explore how the race has changed!"),
             tabsetPanel(type = "tabs",
                         
                         tabPanel(title = "Participation",
                                  sidebarLayout(
                                    
                                    sidebarPanel(
                                      helpText("Entries over the Years"),
                                      
                                      radioButtons(inputId = "genderline",
                                                   label = "Which gender do you want to see?",
                                                   choices = list("Male" = "men",
                                                                  "Female" = "women",
                                                                  "Both" = "total"),
                                                   selected = ("total")),
                                      
                                      sliderInput(inputId = "animation", 
                                                  label = "Years:",
                                                  min = 1972, max = 2017,
                                                  value = 1972, step = 1,
                                                  animate = animationOptions(loop = TRUE),
                                                  sep = ""),
                                      tags$h5("Press play to begin!")),
                                    
                                    mainPanel(
                                      plotOutput(outputId = "line")
                                    )
                                  )),
                         
                         tabPanel(title = "US Finishers",
                                  sidebarLayout(
                                    sidebarPanel(
                                      helpText("Boston Marathon finishers from the US"),
                                      
                                      checkboxGroupInput(inputId = "age",
                                                         label = "Which age range(s) do you want to see?",
                                                         choices = unique(states$age.range),
                                                         selected =  unique(states$age.range)),
                                      
                                      checkboxGroupInput(inputId = "gender",
                                                         label = "Which gender do you want to see?",
                                                         choices = unique(states$Gender),
                                                         selected = unique(states$Gender)),
                                      
                                      sliderInput(inputId = "year",
                                                  label = "Choose a range of years:",
                                                  min = 2001, max = 2017,
                                                  value = c(2015, 2017),
                                                  sep = ""),
                                      actionButton("action",
                                                   label = "Click to Update"),
                                      p("Please be patient while the server loads")),
                                    
                                    
                                    mainPanel(
                                      tabsetPanel(
                                        tabPanel(
                                          title = "Map",
                                          plotOutput(outputId = "map")),
                                        tabPanel(
                                          title = "Data Table",
                                          DT::dataTableOutput(outputId = "ustable")
                                        )
                                      ) 
                                    ))),
                         tabPanel(title = "All Finishers",
                                  verticalLayout(
                                    helpText("We can also look at a table of all the finishers from all around the world."),
                                    
                                    checkboxGroupInput(inputId = "vars",
                                                       label = "Which variable(s) do you want to see?",
                                                       choices = list("Age",
                                                                      "Gender",
                                                                      "City",
                                                                      "State",
                                                                      "Country",
                                                                      "Year"),
                                                       selected = c("Age", 
                                                                    "Gender",
                                                                    "City", 
                                                                    "State",
                                                                    "Country",
                                                                    "Year"))),
                                  
                                  DT::dataTableOutput(outputId = "table")
                         ),
                         tabPanel(title = "Summary",
                                  sidebarLayout(
                                    
                                    sidebarPanel(
                                      helpText("What about some summary statistics from the last 16 years?"),
                                      
                                      checkboxGroupInput(inputId = "sumyear",
                                                         label = "Which year(s) do you want to see?",
                                                         choices = summary$Year,
                                                         selected = summary$Year)),
                                    
                                    mainPanel(
                                      DT::dataTableOutput(outputId = "summary")
                                    )
                                    
                                  )),
                         tabPanel(title = "More Information",
                                  sidebarLayout(
                                    sidebarPanel(width = 6,
                                                 br(),
                                                 p(h4("The following are links to the data we used for this app:"),
                                                   br(),
                                                   tags$a(href="https://github.com/llimllib/bostonmarathon", "Data from 2001-2014"),
                                                   br(),
                                                   tags$a(href="https://www.kaggle.com/rojour/boston-results/data", "Data from 2015-2017"),
                                                   br(),
                                                   tags$a(href="http://www.baa.org/races/boston-marathon/boston-marathon-history/participation.aspx", "Participation Data from 1972-2017"),
                                                   br(),
                                                   "Dataset for",
                                                   tags$a(href="http://www.fonz.net/blog/archives/2008/04/06/csv-of-states-and-state-abbreviations/", "state abbreviations"),
                                                   "to join datasets"),
                                                 br()),
                                    mainPanel(width = 6,
                                              br(),
                                              br(),
                                              p(tags$img(src = "https://upload.wikimedia.org/wikipedia/en/thumb/c/c5/Bostonmarathonlogo.jpg/220px-Bostonmarathonlogo.jpg")))
                                  ))
             ))
)

shinyApp(ui = ui, server = server)