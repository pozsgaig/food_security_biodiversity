library(rstudioapi)
library(openxlsx)
library(readr)
library(tidyverse)
library(leaflet)
library(rnaturalearth)      #for downloading maps
library(sf)                 #for manipulating downloaded maps
library(sp)
library(geodata)
library(dismo)
library(raster)
library(ggmap)
library(ggpubr)
library(ggnewscale)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

getwd()
