library(plm)
library(clubSandwich)
library(stargazer)
library(alpaca)
library(texreg)
library(corrplot)


library(ggplot2)
library(graphics)
library(knitr)
library(car)
library(gplots)


dd <- read.csv("C:\\Users\\verei\\Desktop\\Jupyter_Notebooks\\Econometric_project\\global_data.csv", sep = ";")

# Нормируем FDI, так как иначе получим вырожденную матрицу (слишком большой дискриминант матрицы)
dd$FDI <- dd$FDI / 100000000
dd$net_migration <- dd$net_migration / 100000

table(dd$YEAR)

dd <- na.omit(dd)

dd_panel <- pdata.frame(dd, index = c("COUNTRY", "YEAR"))

# Data visualization


hist(dd$agriculture_value_added, 111,
     xlab = "Добавленная стоимость",
     main = "Добавленная стоимость")
hist(dd$agricultural_land, 111,
     xlab = "Аграрные земли",
     main = "Аграрные земли")
hist(dd$cereal_yield, 111,
     xlab = "Урожайность зерновых культур",
     main = "Урожайность зерновых культур")
hist(dd$crop_index, 111,
     xlab = "Индекс урожайности",
     main = "Индекс урожайности")
hist(dd$export, 111,
     xlab = "Экспорт",
     main = "Экспорт")
hist(dd$employment_agriculture, 111,
     xlab = "Занятость в агропромышленности",
     main = "Занятость в агропромышленности")
hist(dd$import, 111,
     xlab = "Импорт",
     main = "Импорт")
hist(dd$inflation_cost_prices, 111,
     xlab = "Инфляция потребительский цен",
     main = "Инфляция потребительский цен")
hist(dd$inflation_deflator, 111,
     xlab = "Инфляция (дефлятор)",
     main = "Инфляция (дейлятор)")
hist(dd$natural_resources_rent, 111,
     xlab = "Пользование природными ресурсами",
     main = "Пользование природными ресурсами")
hist(dd$total_tax, 111,
     xlab = "Ставка налога",
     main = "Ставка налога")
hist(dd$GDP, 111,
     xlab = "ВВП",
     main = "ВВП")
hist(dd$rural_population, 111,
     xlab = "Население деревень",
     main = "Население деревень")
hist(dd$consumption, 111,
     xlab = "Потребление",
     main = "Потребление")
hist(dd$FDI, 111,
     xlab = "Внешние иностранные инвестиции",
     main = "Внешние иностранные инвестиции")



# КАРТИНОЧКИ!
# coplot(agriculture_value_added ~ YEAR|COUNTRY, data = dd)
coplot(agriculture_value_added ~ YEAR|COUNTRY, type="l", data = dd)
coplot(agriculture_value_added ~ YEAR|COUNTRY, type="b", data = dd)

scatterplot(agriculture_value_added ~ YEAR|COUNTRY, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data = dd)

plotmeans(consumption ~ YEAR, main="Heterogeineity across year", data = dd)

detach("package:gplots")

plot(x = dd$urban_population, y = dd$agriculture_value_added, main = "", 
     xlab = "", ylab = "Добавленная стоимость", 
     col = "green")

plot(x = dd$employment_agriculture, y = dd$agriculture_value_added, main = "", 
     xlab = "", ylab = "Добавленная стоимость", 
     col = "green")

plot(x = dd$agricultural_land, y = dd$agriculture_value_added, main = "", 
     xlab = "", ylab = "Добавленная стоимость", 
     col = "green")


myplot <- ggplot(data = dd,
                 mapping = aes(x = urban_population,
                               y = agriculture_value_added)) +
                   geom_point(shape = 1, col = "green", size = 2, ) +
                   xlim(c(min(dd$urban_population) * 0.9, max(dd$urban_population) * 1.01)) +
                   ylim(c(min(dd$agriculture_value_added) * 0.9, max(dd$agriculture_value_added) * 1.01)) +
                   labs(title = "Зависимость добавленной стоимости от показателей модели")

print(myplot)

# New models

m2_FE <- plm(agriculture_value_added ~ import + export 
             + agricultural_land + natural_resources_rent + rural_population 
             + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
             + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
             data = dd_panel,
             effect = "individual", 
             model = "within")
summary(m2_FE, vcov = vcovHC(m2_FE, cluster = "group"))

m2_RE <- plm(agriculture_value_added ~ import + export 
             + agricultural_land + natural_resources_rent + rural_population
             + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
             + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
             data = dd_panel,
             effect = "individual", 
             model = "random", 
             #random.method = "amemiya"
             )
summary(m2_RE, vcov = vcovHC(m2_RE, cluster = "group"))

#m2_TE <- plm(agriculture_value_added ~ import + export 
#             + agricultural_land + natural_resources_rent + rural_population 
#             + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
#             + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
#             data = dd_panel,
#             effect = "time", 
#             model = "within")
#summary(m2_TE, vcov = vcovHC(m2_TE, cluster = "group"))

m3_RE <- pggls(agriculture_value_added ~ import + export 
             + agricultural_land + natural_resources_rent + rural_population
             + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
             + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
             data = dd_panel,
             model = "pooling")
summary(m3_RE, vcov = vcovHC(m3_RE, cluster = "group"))
# vcovHC(m3_RE, method = "arellano", type="HC3")

m3 <- plm(agriculture_value_added ~ import + export 
          + agricultural_land + natural_resources_rent + rural_population 
          + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
          + employment_agriculture + inflation_deflator + consumption + net_migration + FDI,
          data = dd_panel,
          model = "pooling")
summary(m3, vcov = vcovHC(m3, cluster = "group"))

pFtest(m2_FE, m3)
# Выбираем fixed effect model (не pooling)

phtest(m2_FE, m2_RE)
# p-value > 0.05, ничего не можем сказать

plmtest(m3, type="bp")
# Нулевая гипотеза H0: нет значимой разницы между странами -- отвергается => используем random effect model

plmtest(m3, effect="time")
# Нет оснований отвергать нулевую гипотезу => временного эффекта нет

plmtest(m3, effect="individual")
# Индивидуальный эффект

pcdtest(m2_FE, test="lm")
# cross-sectional dependence

pcdtest(m2_FE, test="cd")
# cross-sectional dependence

pbgtest(m2_FE)
# serial correlation



# Тест на гомоскедастичность не делаем -- у нас робастные ошибки!

stargazer(m2_FE, m2_RE,
          title = "Результаты регрессии",
          type = "latex", 
          se = c(list(coef(summary(m2_FE, vcov = vcovHC(m2_FE, cluster = "group")))[, 2]),
                 list(coef(summary(m2_RE, vcov = vcovHC(m2_RE, cluster = "group")))[, 2])),
          star.cutoffs = c(0.05, 0.01, 0.001))

texreg(list(m3_RE),
       caption="Результаты регрессии",
       model.names=c("RE by FGLS"),
       stars = c(0.05, 0.01, 0.001),
       digits = 6)

m2_RE_1 <- plm(agriculture_value_added ~ import + export 
             + agricultural_land + natural_resources_rent + rural_population
             + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
             + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
             data = dd_panel[dd$GDP > 1e+11, ],
             effect = "individual", 
             model = "random", 
             #random.method = "amemiya"
)
summary(m2_RE_1, vcov = vcovHC(m2_RE_1, cluster = "group"))

m2_RE_2 <- plm(agriculture_value_added ~ import + export 
               + agricultural_land + natural_resources_rent + rural_population
               + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
               + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
               data = dd_panel[dd$GDP < 1e+11, ],
               effect = "individual", 
               model = "random", 
               #random.method = "amemiya"
)
summary(m2_RE_2, vcov = vcovHC(m2_RE_2, cluster = "group"))

m2_RE_3 <- plm(agriculture_value_added ~ import + export 
               + agricultural_land + natural_resources_rent + rural_population
               + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
               + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
               data = dd_panel[abs(dd$FDI) > 500, ],
               effect = "individual", 
               model = "random", 
               #random.method = "amemiya"
)
summary(m2_RE_3, vcov = vcovHC(m2_RE_3, cluster = "group"))

m2_RE_4 <- plm(agriculture_value_added ~ import + export 
               + agricultural_land + natural_resources_rent + rural_population
               + total_tax + inflation_cost_prices + crop_index + cereal_yield + livestock_index 
               + employment_agriculture + inflation_deflator + consumption + net_migration,# + FDI,
               data = dd_panel[abs(dd$FDI) < 500, ],
               effect = "individual", 
               model = "random", 
               #random.method = "amemiya"
)
summary(m2_RE_4, vcov = vcovHC(m2_RE_4, cluster = "group"))

stargazer(m2_RE_1, m2_RE_2, m2_RE_3, m2_RE_4,
          title = "Результаты регрессии по подвыборкам",
          type = "latex", 
          se = c(list(coef(summary(m2_RE_1, vcov = vcovHC(m2_RE_1, cluster = "group")))[, 2]),
                 list(coef(summary(m2_RE_2, vcov = vcovHC(m2_RE_2, cluster = "group")))[, 2]),
                 list(coef(summary(m2_RE_3, vcov = vcovHC(m2_RE_3, cluster = "group")))[, 2]),
                 list(coef(summary(m2_RE_4, vcov = vcovHC(m2_RE_4, cluster = "group")))[, 2])),
          star.cutoffs = c(0.05, 0.01, 0.001))

m2_RE_5 <- plm(agriculture_value_added ~ import + export 
             + agricultural_land + rural_population + employment_agriculture + inflation_cost_prices 
             + consumption + net_migration,
             data = dd_panel,
             effect = "individual", 
             model = "random", 
             #random.method = "amemiya"
)
summary(m2_RE_5, vcov = vcovHC(m2_RE_5, cluster = "group"))

stargazer(m2_RE_5,
          title = "Результаты регрессии по классическим переменным",
          type = "latex",
          #report = "vc*sp",
          se = list(coef(summary(m2_RE_5, vcov = vcovHC(m2_RE_5, cluster = "group")))[, 2]),
          star.cutoffs = c(0.05, 0.01, 0.001))
