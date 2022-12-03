## SOLVE FUNCTIONS FOR HOMEWORK II
if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tibble, ggplot2, data.table, kableExtra)


V_exr = function(d_slope, d_inter, s_slope, s_inter, inter_d_update, inter_s_update){
  
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

  
  d_curve_inv = function(p){
    q_d = (1/(d_slope+inter_d_update))*(p - d_inter)
    return(q_d)
  }
  
  s_curve_inv = function(p){
    q_s = (1/(s_slope+inter_s_update))*(p - s_inter)
    return(q_s)
  }
  
  d_curve = function(q_d){
    p = d_inter + inter_d_update + d_slope*q_d
    return(p)
  }
  
  s_curve = function(q_s){
    p = s_inter + inter_s_update + s_slope*q_s
    return(p)
  }
  
  q_a_new = uniroot(function(x) d_curve(x)-s_curve(x),c(10,1e8))$root
  p_a_new = s_curve(q_a_new)
  
  
  df = tibble(
    `Exchange Rate` = c(p_a, p_a_new),
    `Unit of Currency` = c(q_a, q_a_new)
  )
  
  return(df)
  
}

# Testing on example from class
# V_exr(d_slope, d_inter, s_slope, s_inter)
V_exr(-0.075, 4, 0.025, 0.5, 0.5, 0) %>% 
  mutate(delta_e = 100*(`Exchange Rate`-lag(`Exchange Rate`))/lag(`Exchange Rate`))
# Practice Problems

V_exr(-0.061, 57, 0.012, 2, 2, 0) %>% 
  mutate(delta_e = 100*(`Exchange Rate`-lag(`Exchange Rate`))/lag(`Exchange Rate`))

V_exr(-0.29, 180, 0.251, 50, 0, -40) %>% 
  mutate(delta_e = 100*(`Exchange Rate`-lag(`Exchange Rate`))/lag(`Exchange Rate`))

V_exr(-45, 1080, 22, 200, 10, -80) %>% 
  mutate(delta_e = 100*(`Exchange Rate`-lag(`Exchange Rate`))/lag(`Exchange Rate`))

V_exr(-45, 1080, 22, 200, 10, -80) %>% 
  mutate(delta_e = 100*(`Exchange Rate`-lag(`Exchange Rate`))/lag(`Exchange Rate`))


## Final Exam
V_exr(-5.2, 1000, 1/8
      , 2, 12, 7) %>% 
  mutate(delta_e = 100*(`Exchange Rate`-lag(`Exchange Rate`))/lag(`Exchange Rate`))

