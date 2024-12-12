#######################################################################
#### This script should run ONCE before you start using the tool. #####
#######################################################################

### this may take some time!            ###
### and you need an internet connection ###

# Install pacman
install.packages("pacman")
install.packages("remotes")
install.packages("devtools")

# Install chromote
install.packages("chromote", repos = c('https://rstudio.r-universe.dev', 'https://cloud.r-project.org'))

# Install webshot2
remotes::install_github("rstudio/webshot2")

# Install ggh4x
devtools::install_github("teunbrand/ggh4x")

# Install tidy text
devtools::install_github("juliasilge/janeaustenr")
install.packages('SnowballC', repos='http://cran.us.r-project.org')
devtools::install_github("ropensci/tokenizers")
remotes::install_github("juliasilge/tidytext")

# Install ggthemr
devtools::install_github('Mikata-Project/ggthemr')

# Install or load all the others
pacman::p_load(ggplot2, rio, here, dplyr, tidyverse, lubridate, 
               magrittr, rmarkdown, knitr, kableExtra, toOrdinal, gridExtra, 
               ggpubr, gridtext, grid, patchwork, png, webshot2, ggforce,
               stringr, forcats)

