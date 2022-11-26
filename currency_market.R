## SOLVE FUNCTIONS FOR HOMEWORK II
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tibble, ggplot2, data.table, kableExtra)


V_exr = function(d_slope, d_inter, s_slope, s_inter){
  
  # Function Home
  
  d_curve_inv = function(p){
    q_d = (1/d_slope)*(p - d_inter)
    return(q_d)
  }
  
  s_curve_inv = function(p){
    q_s = (1/s_slope)*(p - s_inter)
    return(q_s)
  }
  
  d_curve = function(q_d){
    p = d_inter + d_slope*q_d
    return(p)
  }
  
  s_curve = function(q_s){
    p = s_inter + s_slope*q_s
    return(p)
  }
  
  q_a = uniroot(function(x) d_curve(x)-s_curve(x),c(10,1e8))$root
  p_a = s_curve(q_a)
  
  df = tibble(
    `Exchange Rate` = c(p_a),
    `Unit of Currency` = c(q_a)
  )
  
  return(df)
  
}

# Testing on example from class
# V_exr(d_slope, d_inter, s_slope, s_inter)

V_exr(-0.075, 4, 0.025, 0.5)
V_exr(-0.075, 4.5, 0.025, 0.5)


price = 2220
inputs = 670
tariff_final = 25
tariff_input = 12

IV_tariff = function(price, inputs, tariff_final, tariff_input){
  
  df = tibble(
    Variable = c("Price of Domestic Final Good",
                 "Value of Imported Inputs",
                 "Domestic Value-Added",
                 "Effective Rate of Protection, %"),
    `No Tariff` = c(price, inputs, price-inputs, 0),
    `+ Tariff on Final Good` = c(price*(1+tariff_final/100),
                                 inputs, price*(1+tariff_final/100) - inputs,
                                 100*(price*(1+tariff_final/100) - inputs - (price-inputs))/(price-inputs) ),
    `+ Tariff on Input Good` = c(price*(1+tariff_final/100),
                                 inputs*(1+tariff_input/100),
                                 price*(1+tariff_final/100) - inputs*(1+tariff_input/100),
                                 100*(price*(1+tariff_final/100) - inputs*(1+tariff_input/100) - (price-inputs))/(price-inputs))
  )
  
  return(df)
  
}