# Comapny Information: https://www.landeranalytics.com/
# Slides and Musings: https://www.jaredlander.com/
# R Conference: https://www.rstats.nyc/
# Meetup: https://nyhackr.org/
# Book: https://amzn.to/2O05A5z

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

# linear models ####

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


library(parsnip)

spec_lm <- linear_reg() %>% 
    set_engine('lm')
spec_lm

sal8 <- spec_lm %>% 
    fit(SalaryCY ~ ., data=sal_ready)

spec_glmnet <- linear_reg() %>% 
    set_engine('glmnet')
sal9 <- spec_glmnet %>% 
    fit(SalaryCY ~ ., data=sal_ready)

linear_reg() %>% 
    set_engine('stan') %>% 
    fit(SalaryCY ~ ., data=sal_ready)
linear_reg() %>% 
    set_engine('keras') %>% 
    fit(SalaryCY ~ ., data=sal_ready)
# set_engine('spark')

parsnip::logistic_reg
parsnip::boost_tree
parsnip::rand_forest

sal8

sal_test <- sal_rec %>% 
    bake(everything(), new_data=test)

sal_preds_8 <- predict(sal8, new_data=sal_test)
sal_preds_8

library(yardstick)

sal_preds_8 %>% 
    bind_cols(sal_test) %>%
    mutate(SalaryCY=10^SalaryCY, .pred=10^.pred) %>%
    rmse(truth=SalaryCY, estimate=.pred)


# decision trees ####

# http://www.r2d3.us/visual-intro-to-machine-learning-part-1/
# scattering of slides at https://www.jaredlander.com/talks/


library(xgboost)

test_y <- sal_rec %>% 
    bake(SalaryCY, new_data=test, composition='matrix')
head(test_y)

train_xg <- xgb.DMatrix(data=sal_x, label=sal_y)
# DON'T USE TEST DATA AS VALIDATION DATA
# WE'RE JUST DOING IT OUT OF LAZINESS
test_xg <- xgb.DMatrix(data=test_x, label=test_y)

sal9 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=1,
    watchlist=list(train=train_xg)
)
sal9
sal9 %>% 
    xgb.plot.multi.trees()

sal10 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=100,
    watchlist=list(train=train_xg)
)

sal11 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=500,
    watchlist=list(train=train_xg)
)

sal12 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=2000,
    watchlist=list(train=train_xg),
    print_every_n=20
)

sal13 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=1000,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=20
)

library(dygraphs)
dygraph(sal13$evaluation_log)


sal14 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=1000,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=10,
    early_stopping_rounds=50
)

sal14 %>% 
    xgb.importance(model=.) %>% 
    .[1:10, ] %>% 
    xgb.plot.importance()


sal15 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=500,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=5,
    early_stopping_rounds=50,
    max_depth=10
)

sal16 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=500,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=5,
    early_stopping_rounds=50,
    max_depth=2
)

sal17 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=500,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=5,
    early_stopping_rounds=50,
    max_depth=2,
    subsample=0.5
)

sal18 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=500,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=5,
    early_stopping_rounds=50,
    max_depth=2,
    subsample=0.5,
    colsample_bytree=0.5
)

sal19 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=1,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=5,
    early_stopping_rounds=50,
    max_depth=2,
    subsample=0.5,
    colsample_bytree=0.5,
    num_parallel_tree=500
)

sal20 <- xgb.train(
    data=train_xg,
    objective='reg:squarederror',
    booster='gbtree',
    eval_metric='rmse',
    nrounds=20,
    watchlist=list(train=train_xg, validate=test_xg),
    print_every_n=1,
    early_stopping_rounds=50,
    max_depth=2,
    subsample=0.5,
    colsample_bytree=0.5,
    num_parallel_tree=50
)

# for each unique combination of hyperparameters
# run cross-validation
# get cross-validated error
# the combination with the best error wins

# set up cross-validation ####
# rsample
sal_cv <- vfold_cv(data=train, v=5, strata='SalaryCY')

# set up model specification
# parsnip
# devtools::install_github('tidymodels/tune')
library(dials)
library(tune)
spec_xg <- boost_tree(
    mode='regression', 
    learn_rate=0.3, 
    trees=tune(id='NumTrees'),
    tree_depth=tune()
) %>% 
    set_engine('xgboost')
spec_xg

parameters(spec_xg)

trees()
tree_depth()

sal_xg_params <-
    spec_xg %>% 
    parameters() %>% 
    update(
        NumTrees=trees(range=c(20, 500)),
        tree_depth=tree_depth(range=c(2, 8))
    )
sal_xg_params$object

# make grid for tuning over ####

sal_grid <- grid_random(sal_xg_params, size=6)
sal_grid

sal_rec <- recipe(SalaryCY ~ Region + Title + Sector + Years + Reports + 
                      Level + Career + Floor,
                  data=train) %>% 
    step_nzv(all_predictors()) %>% 
    step_BoxCox(all_numeric(), -SalaryCY) %>% 
    step_other(all_nominal(), other='Misc') %>% 
    step_dummy(all_nominal(), one_hot=TRUE)
sal_rec
