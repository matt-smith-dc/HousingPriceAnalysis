
library(tidyr)
library(tigris)
library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(tidyverse)

library(fredr)
library(forecast)
library(shiny)
library(shinydashboard)
options(tigris_use_cache = TRUE)



# Set FRED key to pull data from FRB STL:
fredr_set_key('')

data.path <- ''
setwd(data.path)

#Parameters
zips.year <- 2010
data.month <- '2024-11-30'
state <- 'VA'

get_zipcode_data <- function(data.path, zips.year, zips.state) {
  cb <- ifelse(zips.year == 2000, TRUE, FALSE)
  if(getwd() != data.path) {
    setwd(data.path)
  }
  
  # Get current date and format folder name as YYYY-MM
  current_folder <- format(Sys.Date(), "%Y-%m")
  zips.file.name <- paste0('zipcodes_',zips.year, '_', zips.state, '.rds')
  
  # Check if the monthly folder exists, create if it doesn't
  if (!dir.exists(current_folder)) {
    dir.create(current_folder)
  }
  # Set working directory to the monthly folder
  setwd(current_folder)
  print(cat("Working directory is now:", getwd()))
  
  zip.col.name <- ifelse(zips.year==2000, 'ZCTA', 'ZCTA5CE10')
  
  if(!file.exists(zips.file.name)) {
    #Download data if it doesn't exist in monthly data folder
    zipcodes <- zctas(year=zips.year, state=zips.state, cb=cb)
    saveRDS(zipcodes, file=zips.file.name)
    print('Zipcodes file didn\'t exist. Downloaded and saved locally.')
  } else {
    zipcodes <- readRDS(zips.file.name)
    print('Zipcodes file already existed. Read from monthly folder.')
  }
  
  zipcodes <- zipcodes %>% select({{zip.col.name}}, geometry)
  colnames(zipcodes) <- c('ZCTA', 'geometry')
  return(zipcodes)
}

get_us_states <- function() {
  states.file.name <- paste0('states.rds')
  if(!file.exists(states.file.name)) {
    #Download data if it doesn't exist in monthly data folder
    us.states <- states()
    saveRDS(us.states, file=states.file.name)
    print('US States file didn\'t exist. Downloaded and saved locally.')
  } else {
    us.states <- readRDS(states.file.name)
    print('US States file already existed. Read from monthly folder.')
  }
  return(us.states)
}



mortgage.data <- na.omit(fredr(series_id="MORTGAGE30US", observation_start = as.Date('2000-01-01')))
mortgage.data <- mortgage.data %>% select(date, value)
colnames(mortgage.data) <- c('date', 'mortgage.30.yr')

#https://www.zillow.com/research/data/
csv.name <- 'Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv'
housing.data <- read.csv(csv.name)
years <- colnames(housing.data)[10:length(colnames(housing.data))]

housing.data <- housing.data %>% pivot_longer(cols = all_of(years),
                                              names_to = "Month",
                                              values_to = "Value"
)
housing.data$Month <- as.Date(gsub('\\.', '-', substr(housing.data$Month,2,11)), "%Y-%m-%d")

#Add latest mortgage rate to each housing date
housing.dates.rates <- housing.data %>% select(Month) %>% unique() %>%
  left_join(mortgage.data, join_by(Month >= date)) %>%
  group_by(Month) %>%
  slice_max(date, n=1) %>%
  ungroup()

housing.mortgage.data <- housing.data %>% merge(y=housing.dates.rates,
                                                by.x='Month',by.y='Month')
housing.mortgage.data <- housing.mortgage.data[
  order(housing.mortgage.data$RegionName,
        housing.mortgage.data$Month), ]
housing.mortgage.data <- housing.mortgage.data %>%
  group_by(RegionName) %>%
  mutate( d1Value = (Value/lag(Value)) - 1,
          d3MortgageRate = (mortgage.30.yr/lag(mortgage.30.yr, n=3)-1)) %>%
  ungroup()

housing.mortgage.data %>% #filter(Month < '2015-12-31' & StateName=='VA') %>%
  select(d1Value, d3MortgageRate) %>%
  filter(!is.na(d1Value) & !is.na(d3MortgageRate)) %>%
  summarize(across(c(d3MortgageRate), ~ cor(.x, d1Value)))

#housing.mortgage.data %>% filter(Month < '2015-12-31' & StateName == 'VA') %>%
#  select
housing.mortgage.data %>% filter(Month < '2015-12-31' & StateName == 'VA') %>%
  ggplot(., aes(x=d1Value, y=d3MortgageRate)) +
  geom_point()

################################################################################
#Single State & Data Analysis

#View example
#housing.data[housing.data$RegionName == 20191,]

state.housing.data <- housing.mortgage.data[housing.mortgage.data$State == state, ] %>%
  select(RegionName,City, Metro, CountyName, Month, Value) %>%
  filter(Month==data.month)

zipcodes <- get_zipcode_data(data.path, zips.year, state)

combined.data <- merge(x=state.housing.data, y=zipcodes, by.x='RegionName', by.y='ZCTA', all.y=TRUE)


low <- min(combined.data[!is.na(combined.data$Value), 'Value'])
avg <- mean(combined.data[!is.na(combined.data$Value), 'Value'])
high <- max(combined.data[!is.na(combined.data$Value), 'Value'])

us.states <- states()
state.boundary <- us.states %>% filter(STUSPS == state)

p <- combined.data %>% ggplot() +
  geom_sf(data = state.boundary)+
  geom_sf(aes(geometry=geometry, fill = Value,
              text = paste(RegionName, ': $', format(
                Value, bigmark=',',scientific=FALSE))))+
  scale_fill_viridis_c(option = "plasma", na.value="white",
                       labels = comma, breaks=c(low,avg,high))

pg <- ggplotly(p, tooltip = 'text')
pg


################################################################################
# Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Housing Analysis"),
  dashboardSidebar(sidebarMenu(
    menuItem("Filters", tabName = "filters", icon = icon("filter")),
    selectInput(
      inputId = "state",
      label = "Select State:",
      choices = sort(unique(housing.data$State)), # Replace with actual state choices
      selected = "VA"
    ),
    selectInput(
      inputId = "data_month",
      label = "Select Month:",
      choices = sort(unique(housing.data$Month)),
      selected = max(housing.data$Month) # Default to the most recent month
    )
  )),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("plot1"))
    )
    
  )
)

server <- function(input, output) {
  # Reactive expression to filter housing data based on inputs
  filtered_housing_data <- reactive({
    housing.data %>%
      filter(State == input$state, Month == as.Date(input$data_month))
  })
  
  zipcodes_data <- reactive({
    get_zipcode_data(data.path, zips.year, input$state)
  })
  
  state_boundaries <- reactive({
    us.states <- get_us_states()
    us.states %>% filter(STUSPS == input$state)
  })
  
  output$plot1 <- renderPlotly({
    state.housing.data <- filtered_housing_data()
    zipcodes <- zipcodes_data()
    
    combined.data <- merge(
      x = state.housing.data,
      y = zipcodes,
      by.x = "RegionName",
      by.y = "ZCTA",
      all.y = TRUE
    )
    state.boundary <- state_boundaries()
    
    low <- min(combined.data[!is.na(combined.data$Value), 'Value'])
    avg <- mean(combined.data[!is.na(combined.data$Value), 'Value'])
    high <- max(combined.data[!is.na(combined.data$Value), 'Value'])
    
    p <- combined.data %>% ggplot() +
      geom_sf(data = state.boundary)+
      geom_sf(aes(geometry=geometry, fill = Value,
                  text = paste(RegionName, ': $', format(
                    Value, bigmark=',',scientific=FALSE))))+
      scale_fill_viridis_c(option = "plasma", na.value="white",
                           labels = comma, breaks=c(low,avg,high))
    ggplotly(p, tooltip = 'text')
    
  })
  
}

shinyApp(ui, server)

