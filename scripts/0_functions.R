# Installing chromote
#install.packages("chromote", repos = c('https://rstudio.r-universe.dev', 'https://cloud.r-project.org'))
# Installing webshot2
#remotes::install_github("rstudio/webshot2")
# Installing ggh4x
#devtools::install_github("teunbrand/ggh4x")

pacman::p_load(ggplot2, ggthemr, rio, here, dplyr, tidyverse, lubridate, 
               magrittr, rmarkdown, knitr, kableExtra, toOrdinal, gridExtra, 
               ggpubr, gridtext, grid, patchwork, png, webshot2, ggforce,
               ggh4x)

