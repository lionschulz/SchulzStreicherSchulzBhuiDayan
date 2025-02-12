# Libraries
library(colorspace)
library(sjmisc)
library(ggsci)
library(wesanderson)
library(DEoptim)
library(afex)
library(patchwork)
library(docstring)
library(jsonlite)
library(plotrix)
library(ggridges)
library(arm)
library(NCmisc)
library(ggpubr)
library(MetBrewer)
library(tools)
library(magrittr)
library(scales)
library(stringr)
library(jsonlite)
library(GGally)
library(ggpattern)
library(psych)
library(ggcorrplot)
library(nFactors)
library(GPArotation)
library(kableExtra)
library(tidystats)
library(report)

library(tidyverse) # load last so its functions take precedent

# Enable anti-aliasing on Windows
if(Sys.info()['sysname'] == "Windows"){
  
  trace(grDevices::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
  
  
  # Enable anti-aliasing on Windows
  trace(grDevices:::png, quote({
    if (missing(type) && missing(antialias)) {
      type <- "cairo-png"
      antialias <- "subpixel"
    }
  }), print = FALSE)
  
  
}



# switch Rstudio modes
darkmode <- function(){
  rstudioapi::applyTheme("One Dark")
}

lightmode <- function(){
  rstudioapi::applyTheme("Textmate (default)")
}