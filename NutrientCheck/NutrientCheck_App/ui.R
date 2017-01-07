# Hello!
# This is a Shiny web application to help you visualize the nutrient content of your food
# intake.
# You can run the application by clicking 'Run App' above.
#
# Find out more about this app on the link below:
#
#   https://github.com/agrou/agrou.github.io
#

## Introduction
#
# NutrientData package contains data from the composition of foods:
# Raw, Processed and Prepared.The original data source is the USDA National Nutrient
# Database for Standard Reference, Release 28 (2015).
#
#
# First download the data available on github:
# https://github.com/56north/NutrientData/blob/master/data/ABBREV.rda

## Load the data
#data("ABBREV")
load("assets/ABBREV.rda", envir = .GlobalEnv)

# or download the R Package NutrientData, available on
# github: https://github.com/56north/NutrientData

#devtools::install_github("56north/NutrientData")

# Load required libraries for the shiny app
library(shiny)
library(dplyr)
library(magrittr)
library(tidyr)
library(plotly)

## Data Processing

#### The aim is to get data into a format that is suitable to be used in the shiny app.
# The objective is to be able to filter information by nutrient and select multiple
# food ingredients. The original data has the nutrient information for the standard portion
# sizes of each food ingredient. However this measure doesn't allways correspond to
# the household portion sizes, for example, we don't "normally" eat 100g of butter.
# Thus, the app allows the user to choose between visualizing nutrient content for
# standard portion sizes or for household portion size or even for a self input
# of a specific quantity.

# Limitations of the app:
# Be aware that the ideal is to have a selection of a specific quantity for each food
# ingredient. In this case it is recommended to use the 'Dynamic' option considering
# that the selected quantity will not be realistic for every type of food.

#print(str(ABBREV))
assign("NutData", 
  ABBREV %>% # create a new dataset with variables of interest
    # select only some nutrients for this example, and rename the variables
    select(NDB_No, Food = Shrt_Desc, Calories = Energ_Kcal, Protein = `Protein_(g)`,
           Total_Lipids = `Lipid_Tot_(g)`, SatFat = `FA_Sat_(g)`,
           MUFAs = `FA_Mono_(g)`, PUFAs = `FA_Poly_(g)`, Carbohydrates = `Carbohydrt_(g)`,
           Fiber = `Fiber_TD_(g)`, Sugar = `Sugar_Tot_(g)`, Calcium = `Calcium_(mg)`,
           Iron = `Iron_(mg)`, VitC = `Vit_C_(mg)`, VitB12 = contains("Vit_B12"),
           Portion = GmWt_1) %>%
    mutate(Food = toupper(Food)) %>% #get the food names all into the same format
    # make data into a long format to enable filtering specific nutrients
    gather("Nutrient", "Standard Portion", 3:15) %>%
    # Calculate nutrient content for household portion sizes
    mutate(`Household Portion` = round(Portion * `Standard Portion` / 100, 3))
  , envir = .GlobalEnv)

#print(str(NutData)) #,unselect this command if you want to have a look at the dataset

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel(div(strong("Nutrient Check"), style ="color: #116963;")),
    br(),
  sidebarLayout(

    sidebarPanel(
      width = 4,

      radioButtons("PortionInput", "Choose the portion size",
                        choices = c("Standard", "Household", "Dynamic"),
                        selected = "Standard"),
      br(),

      selectInput("foodInput", "Select your food ingredients",
                    choices = unique(NutData$Food),
                    selected = c("BANANAS,RAW", "AVOCADOS,RAW,ALL COMM VAR"), multiple = TRUE),
      br(),
      selectInput("NutrientInput", "Select a Nutrient",
                  choices = unique(NutData$Nutrient),
                  multiple = FALSE),
      br(),

      column(12,
             sliderInput("grams", "Dynamic Portion: Select a quantity", min = 1, max = 500, value = 100, post = "g")),


      br(),
      submitButton("Update View")

    ),

    mainPanel(
      plotlyOutput("NutrientPlot", width = "100%", height = "400px"),
      br(),
      br(),
      tableOutput("NutrientTable")
    )

    )
  )
)

#publish(user = "agrou", repo = "https://github.com/agrou/agrou.github.io/tree/gh-pages")
