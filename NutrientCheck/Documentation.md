---
title: "App Documentation"
author: "agrou"
date: "January 5, 2017"
output:
  html_document:
    mathjax: null
---

## NutrientCheck App Documentation

This is a Shiny web application to help you visualize the nutrient content of your food intake.

Find out more about this app on the link below:

To run the app on your computer, open R studio and copy this code to your R console:

```r
library(shiny)

runGitHub("agrou.github.io", username = "agrou", 
          ref ="master", subdir = "/NutrientCheck/NutrientCheck_App")
```

https://github.com/agrou/agrou.github.io/tree/master/NutrientCheck


## Introduction

The package NutrientData package contains data from the composition of foods: Raw, Processed and Prepared.The original data source is the USDA National Nutrient Database for Standard Reference, Release 28 (2015).

The data is available for download [here](https://github.com/56north/NutrientData/blob/master/data/ABBREV.rda) 

To use this app follow the chapter 'Instructions'.

## Aim

The aim is to be able to select information by searching for a specific nutrient and select one ingredient, or multiple food ingredients for comparison. The original data has the nutrient information for the standard portion sizes of each food ingredient. However this measure doesn't allways correspond to the household portion sizes, for example, we don't "normally" eat 100g of butter. 
Thus, the app allows the user to choose between visualizing nutrient content for standard portion sizes or for household portion size or even for a self input of a specific quantity.

## Instructions

Here are some guiding steps to use the app:

Note that the app starts with two examples of food ingredients, but you can erase them and make your own search, by writing the name or part of the name of a food ingredient.
Every time you change your selection you should also click on the button 'Update View' to activate the new display.

1st: Select your food ingredient (one or more)

2nd: Select a nutrient

3rd: Choose how you want to see the information in the plot, selecting one of the 3 options of 'Choose a portion size':

* The 'Standard' gives you 100g for each ingredient. 
* The 'Household' gives you the food ingredients in the quantity they are normally consumed (it's called an household measure). Be aware that this measure is different for each food ingredient and depends on the purpose for the ingredient (to consume it solo or to cook it in a recipe). Be aware that you will be comparing food for different portion sizes in this option.
* The 'Dynamic' option allows you to select the quantity you want (in grams) but, on the opposite of the 'Household' option, this will define the same quantity for all the selected food ingredients. Therefore, this option is ideal to use with one single food.


### Limitations of the app:
Be aware that the ideal is to have a selection of a specific quantity for each food ingredient. In this case it is recommended to use the 'Dynamic' option considering
that the selected quantity will not be realistic for every type of food.
