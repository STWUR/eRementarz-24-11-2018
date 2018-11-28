library(mlr)
library(dplyr)
library(ggplot2)

set.seed(15390)

mieszkania <- read.csv(file = "https://raw.githubusercontent.com/STWUR/STWUR-2017-06-07/master/data/mieszkania_dane.csv", 
                       encoding = "UTF-8") %>% 
  na.omit

# poznajmy dane

head(mieszkania)

dim(mieszkania)

group_by(mieszkania, dzielnica) %>% 
  summarise(length(dzielnica))

group_by(mieszkania, dzielnica) %>% 
  summarise(min(cena_m2),
            max(cena_m2))

ggplot(mieszkania, aes(x = rok, y = cena_m2)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ dzielnica) +
  theme_bw()

ggplot(mieszkania, aes(x = rok, y = cena_m2)) +
  stat_density2d(aes(alpha = ..level..), color = "black", contour = TRUE, 
                 geom = "polygon") +
  facet_wrap(~ dzielnica) +
  theme_bw()


# 1. Sprawdź w jakiej dzielnicy jest najwięcej mieszkń poniżej 5000 PLN za m2.

filter(mieszkania, cena_m2 < 5000) %>% 
  group_by(dzielnica) %>% 
  summarise(length(dzielnica))

x1 <- table(mieszkania[["dzielnica"]], mieszkania[["cena_m2"]] < 5000)

x2 <- mutate(mieszkania, less5000 = cena_m2 < 5000) %>% 
  group_by(dzielnica, less5000) %>% 
  summarise(length(dzielnica))

mieszkania[["dziel", exact = FALSE]]
mieszkania$dziel

# 2. Czy jest zależość między ceną za m^2 i piętrem na którym znajduje się mieszkanie?

ggplot(mieszkania, aes(x = pietro, y = cena_m2)) +
  geom_point()

cor(mieszkania[["pietro"]], mieszkania[["cena_m2"]], method = "spearman")

ggplot(mieszkania, aes(x = factor(pietro), y = cena_m2)) +
  geom_boxplot()

head(mieszkania)

library(reshape2)

melt(mieszkania, id.vars = c("dzielnica", "cena_m2")) %>% 
  ggplot(aes(x = value, y = cena_m2)) +
  geom_point() +
  facet_wrap(~ dzielnica + variable, scales = "free_x")

melt(mieszkania, id.vars = c("dzielnica", "cena_m2")) %>%
  filter(variable %in% c("n_pokoj", "pietro", "pietro_maks")) %>% 
  ggplot(aes(x = factor(value), y = cena_m2)) +
  geom_boxplot() +
  facet_wrap(~ dzielnica + variable, scales = "free_x")

# model ------------------
library(mlr)

predict_price <- makeRegrTask(id = "price", 
                              data = mieszkania, target = "cena_m2")

learnerRF <- makeLearner("regr.ranger")

listLearners() %>% filter(type == "regr") %>% pull(class)

cv_scheme <- makeResampleDesc("CV", iters = 5)

resample(learnerRF, predict_price, cv_scheme, measures = list(mse, rmse))

parameters_set <- makeParamSet(
  makeIntegerParam("num.trees", lower = 400, upper = 1000),
  makeIntegerParam("min.node.size", lower = 1, upper = 5)
)

# mlrMBO

optimal_rf <- tuneParams(learnerRF, predict_price, cv_scheme, 
                         par.set = parameters_set, 
                         control = makeTuneControlGrid(resolution = 2L))

# Porownaj makeTuneControlRandom(maxit = 5) i makeTuneControlGrid(resolution = 2L). Która metoda daje lepszy model? 

generateHyperParsEffectData(optimal_rf)[["data"]] %>% 
  ggplot(aes(x = num.trees, y = min.node.size, 
             fill = sqrt(mse.test.mean), label = round(sqrt(mse.test.mean), 2))) +
  geom_tile() +
  geom_label(fill = "white") +
  theme_bw()


best_par <- data.frame(optimal_rf[["opt.path"]]) %>% 
  filter(mse.test.mean == min(mse.test.mean))

worst_par <- data.frame(optimal_rf[["opt.path"]]) %>% 
  filter(mse.test.mean == max(mse.test.mean))

model_best <- ranger(formula = cena_m2 ~ ., data = mieszkania, num.trees = best_par[["num.trees"]],
                     min.node.size = best_par[["min.node.size"]])

model_worst <- ranger(formula = cena_m2 ~ ., data = mieszkania, 
                      num.trees = worst_par[["num.trees"]],
                      min.node.size = worst_par[["min.node.size"]])

library(auditor)

audit_best <- audit(model_best, data = mieszkania, y = mieszkania[["cena_m2"]], 
                    label = "best", 
                    predict.function = function(m, data) predict(m, data)[["predictions"]])

audit_worst <- audit(model_worst, data = mieszkania, y = mieszkania[["cena_m2"]], label = "worst", 
                     predict.function = function(m, data) predict(m, data)[["predictions"]])

plotModelCorrelation(audit_best, audit_worst)
plotResidualDensity(audit_best, audit_worst)
plotScaleLocation(audit_best, audit_worst)
plotTwoSidedECDF(audit_best, audit_worst)

save(model_best, model_worst, file = "./cz2/models.RData")

# Stwórz optymalny model optymalizując również parametr mtry. Wykorzystując auditora porównaj go z model_best i model_worst.