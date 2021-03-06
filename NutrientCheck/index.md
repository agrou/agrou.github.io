---
title: "NutrientCheck App"
subtitle: "07 janeiro 2017"
author: "agrou"
framework: io2012
highlighter: highlight.js # {highlight.js, prettify, highlight}
hitheme: tomorrow
widgets: [mathjax, bootstrap]
mode: selfcontained # {standalone, draft}
knit: slidify::knit2slides
---

## Introduction 

**NutrientCheck** is a Shiny web application to help you visualize the nutrient content of food.
  
**Aim:** Check the nutritional content of one or more food ingredients. 

**Data Source**: [package NutrientData](https://github.com/56north/NutrientData) contains data from the composition of foods (USDA National Nutrient Database). 


<img src="assets/img/NutrientCheck.png" title="plot of chunk NutritionApp" alt="plot of chunk NutritionApp" width="600px" />


---
## Brief explanation of the Shiny App

1. Data Processing

Data ('ABBREV') was manipulated to select just some variables (nutrients) as an example. 
A calculation for the nutrient content of household portions was added as well as for the quantity inputed by the user.

2. UI/Server

UI has 4 input 'fields' and 1 action button, to update information as the input changes. 
Server defines how the plot and the table are presented, with conditions to filter observations or calculate new data, dependent on the user input.

3. App Interface

Allows the user to choose between visualizing nutrient content of 1 or more foods, for **standard portion sizes** or for **household portion size** or even for a self input, that I called **dynamic portion size**.

---
## Instructions

1. The app starts with two examples of ingredients, you can erase it and write down your own search. 

Click on **'Update View'** button to activate new display.

2: Select your food ingredient (one or more)

3: Select one nutrient

4: Choose how you want to see the information in the plot, selecting one of the 3 options of 'Choose a portion size':

* 'Standard': 100g for each ingredient. 
* 'Household': This measure is different for each food ingredient and depends on the purpose of use for the ingredient. Be aware that you will be comparing food for different portion sizes.
* 'Dynamic': select the quantity you want but, on the opposite of the 'Household' option, this will define the same quantity for all the selected food ingredients. 

---
## Limitations 

Be aware that the ideal is to have a selection of a specific quantity for each food ingredient. In this case it is recommended to use the 'Dynamic' option considering
that the selected quantity will not be realistic for every type of food.

**Find out more about this app on the link below:**

https://github.com/agrou/agrou.github.io/NutrientCheck

**To run the app, copy the code to your R console and use the following code:** 

```r
library(shiny)

runGitHub("agrou.github.io", username = "agrou", 
          ref ="master", subdir = "/NutrientCheck/NutrientCheck_App")
```





