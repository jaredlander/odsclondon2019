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

sal_test <- sal_rec %>% 
    bake(new_data=test)


library(glmnet)


# lm: data.frame/tibble
# glmnet: matrices

sal_x <- sal_rec %>% 
    juice(all_predictors(), composition='matrix')
class(sal_x)
sal_y <- sal_rec %>% 
    # juice(all_outcomes(), composition='matrix')
    juice(SalaryCY, composition='matrix')
head(sal_x)
head(sal_y)

sal4 <- glmnet(x=sal_x, y=sal_y, family='gaussian', alpha=1, standardize=FALSE, nlambda=100)
coef(sal4)
sal4 %>% coef() %>% as.matrix() %>% View()
sal4$lambda
dim(coef(sal4))
plot(sal4, xvar='lambda')
plot(sal4, xvar='lambda', label=TRUE)

coefpath(sal4)
coefplot(sal4, sort='magnitude', lambda=exp(-6))
coefplot(sal4, sort='magnitude', lambda=exp(-10))
coefplot(sal4, sort='magnitude', lambda=exp(-2))

sal5 <- cv.glmnet(x=sal_x, y=sal_y, family='gaussian', 
               alpha=1, standardize=FALSE, nlambda=100,
               nfolds=10)
coefpath(sal5)
plot(sal5)

coefplot(sal5, sort='magnitude', lambda='lambda.min')
coefplot(sal5, sort='magnitude', lambda='lambda.1se')

sal6 <- cv.glmnet(x=sal_x, y=sal_y, 
                  family='gaussian', alpha=0, 
                  standardize=FALSE, nlambda=100,
                  nfolds=10)
coefpath(sal6)

sal7 <- cv.glmnet(x=sal_x, y=sal_y, 
                  family='gaussian', alpha=0.5, 
                  standardize=FALSE, nlambda=100,
                  nfolds=10)
coefpath(sal7)

test_x <- sal_rec %>% 
    bake(all_predictors(), new_data=test, composition='matrix')

sal_preds_7 <- predict(sal7, newx=test_x, s='lambda.1se')
