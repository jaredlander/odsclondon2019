# make sure all packages are installed
install.packages(c(
'dplyr', 
'tidyr', 
'ggplot2', 
'purrr', 
'stringr', 
'DBI', 
'dbplyr', 
'jsonlite', 
'readr', 
'readxl', 
'rvest', 
'tidyverse', 
'ggthemes', 
'ggridges', 
'rmarkdown', 
'usethis', 
'piggyback', 
'here',
'rsample', 
'dials',
'parsnip',
'yardstick',
'remotes',
'devtools',
'glmnet', 'xgboost', 'coefplot', 'DiagrammeR', 'dygraphs',
'shiny', 'shinythemes', 'flexdashboard', 'shinyjs'
))

remotes::install_github("tidymodels/tune")
