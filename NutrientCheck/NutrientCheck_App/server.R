#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

# Load required libraries
library(shiny)
library(plotly)



# Define server logic required to draw a bar plot
shinyServer(function(input, output) {
  output$NutrientPlot <- renderPlotly({

    filtered <- NutData %>%
      filter(Nutrient == input$NutrientInput,
             Food %in% input$foodInput) %>%
              mutate(`Dynamic Portion` = round(input$grams * `Standard Portion` / 100, 3))


    if(input$grams != "100g" && input$PortionInput == "Standard"){


      plot_ly(filtered, x = ~Food, y = ~`Standard Portion`, color = ~Food, type = "bar")
    }
    else if(input$PortionInput == "Standard"){
      plot_ly(filtered, x = ~Food, y = ~`Standard Portion`, color = ~Food, type = "bar")
    }

    else if(input$PortionInput == "Household"){

      plot_ly(filtered, x = ~Food, y = ~`Household Portion`, color = ~Food, type = "bar")
    }
    else if (input$PortionInput == "Dynamic" && input$grams != "100g"){
      plot_ly(filtered, x = ~Food, y = ~`Dynamic Portion`, color = ~Food, type = "bar")
    }
    else{
      plot_ly(filtered, x = ~Food, y = ~`Standard Portion`, color = ~Food, type = "bar")
    }


  })
  # generate a data table dependent on the user input
  output$NutrientTable <- renderTable({

    filtered <- NutData %>%
      filter(Nutrient == input$NutrientInput,
             Food %in% input$foodInput) %>%
      mutate(`Dynamic Portion` = round(input$grams * `Standard Portion` / 100, 3)) %>%
      select(`Food ingredient` = Food, `Household Portion size (g)` = Portion, Nutrient,
             `Per Standard Portion`=`Standard Portion`,
             `Per Household Portion` = `Household Portion`,
             `Per Dynamic Portion`=`Dynamic Portion`)
  })
})


