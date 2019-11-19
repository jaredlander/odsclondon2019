library(dplyr)
library(purrr)
library(ggplot2)

comps <- list.files('data', pattern='^Comp_', full.names=TRUE) %>% 
    map_df(readr::read_csv)


library(rsample)
data_split <- initial_split(data=comps, prop=0.8, strata='SalaryCY')
data_split

train <- training(data_split)
test <- testing(data_split)


ggplot(train, aes(SalaryCY)) + geom_histogram()

ggplot(train, aes(x=SalaryCY, fill=Title)) + 
    geom_histogram() + 
    scale_x_log10()
