# Download and upload the required packages
if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse)
if (!require('dfoptim'))
  install.packages("dfoptim")
library(dfoptim) # For minimization routines
if (!require('numDeriv'))
  install.packages("numDeriv")
library(numDeriv) # To compute Hessian matrix
if (!require('odin'))
  install.packages("odin")
library(odin) 
if (!require('processx'))
  install.packages("processx")
library(processx) 
if (!require('rio'))
  install.packages("rio")
library(rio) 
if (!require('dplyr'))
  install.packages("dplyr")
library(dplyr) 
if (!require('ggplot2'))
  install.packages("ggplot2")
library(ggplot2) 



