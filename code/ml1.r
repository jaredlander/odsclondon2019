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


names(train)

# y ~ x

sal1 <- lm(SalaryCY ~ Region + Title + Sector + 
               Years + Reports + Level + 
               Career + Floor,
           data=train)
sal1
summary(sal1)

library(coefplot)
coefplot(sal1, sort='magnitude')


sal2 <- lm(SalaryCY ~ Region + Title + Sector + 
               Years + Reports + Level + 
               Career + Floor,
           data=train,
           subset=Title != 'MD')
coefplot(sal2, sort='magnitude')


sal2.1 <- lm(log10(SalaryCY) ~ Region + Title + Sector + 
               scale(Years) + scale(Reports) + Level + 
               Career + scale(Floor),
           data=train,
           subset=Title != 'MD')
coefplot(sal2.1, sort='magnitude')


library(recipes)

sal_rec <- recipe(SalaryCY ~ Region + Title + Sector + Years + Reports + Level + Career + Floor,
                  data=train) %>% 
    step_log(SalaryCY, base=10) %>% 
    step_zv(all_predictors()) %>% 
    step_nzv(all_predictors()) %>% 
    step_knnimpute(all_predictors()) %>% 
    step_BoxCox(all_numeric(), -SalaryCY) %>% 
    # step_center(all_numeric()) %>% 
    # step_scale(all_numeric()) %>% 
    step_normalize(all_numeric(), -SalaryCY) %>% 
    # step_filter(Title != 'MD") %>% 
    step_other(all_nominal(), other='Misc') %>% 
    step_dummy(all_nominal()) %>% 
    prep()
sal_rec

sal_ready <- sal_rec %>% 
    juice()
sal_ready

sal3 <- lm(SalaryCY ~ ., data=sal_ready)
coefplot(sal3, sort='magnitude')
