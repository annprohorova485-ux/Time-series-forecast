library(fpp3)
library(ggplot2)
library(dplyr)
library(readxl)
library(tsibble)
library(lubridate)
library(patchwork)
library(tseries)
install.packages("forecast")
library(forecast)
# Задание 1
passengers_data <- read_excel("International_Report_Passengers.xlsx")

ts_data1 <- passengers_data |>
  mutate(Month = yearmonth(paste(Year, Month, "01", sep = "-"))) |>
  group_by(Month) |>
  summarise(Total_Passengers = sum(Total, na.rm = TRUE)) |>
  ungroup() |>
  as_tsibble(index = Month)

glimpse(ts_data1)

ts_data <- ts_data1 |>
  filter(year(Month) < 2013)

# Задание 2
p1 <- ts_data |>
  autoplot(Total_Passengers) +
  labs(title = "Исходный ряд: Международный авиапассажиропоток",
       y = "Количество пассажиров", x = "Год")

p2 <- ts_data |>
  mutate(Diff1 = difference(Total_Passengers)) |>
  autoplot(Diff1) +
  labs(title = "Обычные разности (первого порядка)",
       y = "Изменение пассажиров", x = "Год")

p3 <- ts_data |>
  mutate(Seasonal_Diff = difference(Total_Passengers, 12)) |>
  autoplot(Seasonal_Diff) +
  labs(title = "Сезонные разности (лаг = 12 месяцев)",
       y = "Сезонное изменение", x = "Год")

p1 
p2 / p3

#Компоненты ряда
ts_data2 <- ts_data |>
  fill_gaps() |>
  mutate(Total_Passengers = na.interp(Total_Passengers))

dcmp <- ts_data2 |>
  model(stl = STL(Total_Passengers ~ trend(window = 13) + season(window = "periodic")))

p4 <- components(dcmp) |>
  autoplot() +
  labs(title = "STL декомпозиция ряда на компоненты")

p4

acf1 <- ts_data2 |> 
  ACF(Total_Passengers) |> 
  autoplot() + labs(title = "ACF исходного ряда")

pacf1 <- ts_data2 |> 
  PACF(Total_Passengers) |> 
  autoplot() + labs(title = "PACF исходного ряда")

acf2 <- ts_data2 |> 
  ACF(Total_Passengers |> difference()) |> 
  autoplot() + labs(title = "ACF обычных разностей")

pacf2 <- ts_data2 |> 
  PACF(Total_Passengers |> difference()) |> 
  autoplot() + labs(title = "PACF обычных разностей")

acf3 <- ts_data2 |> 
  ACF(Total_Passengers |> difference(12)) |> 
  autoplot() + labs(title = "ACF сезонных разностей")

pacf3 <- ts_data2 |> 
  PACF(Total_Passengers |> difference(12)) |> 
  autoplot() + labs(title = "PACF сезонных разностей")

(acf1 / pacf1) 
(acf2 / pacf2) 
(acf3 / pacf3)

# Задание 3
kpss_result <- ts_data2 |>
  features(Total_Passengers, unitroot_kpss)

adf_result <- ts_data2 |>
  features(Total_Passengers, unitroot_ndiffs)

kpss_result
adf_result

# Задание 4
lambda <- ts_data2 |>
  features(Total_Passengers, guerrero) |>
  pull(lambda_guerrero)

lambda

data_bc <- ts_data2 |>
  mutate(Passengers_BC = box_cox(Total_Passengers, lambda))

p <- ts_data2 |>
  autoplot(Total_Passengers) +
  labs(title = "Исходный ряд", y = "Пассажиры")

p_bc <- data_bc |>
  autoplot(Passengers_BC) +
  labs(title = paste("Бокса-Кокса (λ =", round(lambda, 3), ")"), 
       y = "Преобразованные значения")

p / p_bc

acf_bc <- data_bc |> 
  ACF(Passengers_BC) |> 
  autoplot() + labs(title = "ACF после преобразования")

pacf_bc <- data_bc |> 
  PACF(Passengers_BC) |> 
  autoplot() + labs(title = "PACF после преобразования")

acf_bc / pacf_bc


# Задание 5
data_period <- ts_data2 |>
  filter(year(Month) >= 2002, year(Month) < 2013)

train_data <- data_period |>
  filter(year(Month) < 2010)

test_data <- data_period |>
  filter(year(Month) >= 2010)

train_data
test_data

# Задание 6
models_all <- train_data |>
  model(
    naive = NAIVE(Total_Passengers),
    snaive = SNAIVE(Total_Passengers),
    ets_auto = ETS(Total_Passengers),
    ets_aaa = ETS(Total_Passengers ~ error("A") + trend("A") + season("A")),
    ets_mmm = ETS(Total_Passengers ~ error("M") + trend("M") + season("M")),
    arima_auto = ARIMA(Total_Passengers),
    arima_110 = ARIMA(Total_Passengers ~ pdq(1,1,0) + PDQ(0,1,1)),
    arima_212 = ARIMA(Total_Passengers ~ pdq(2,1,2) + PDQ(0,1,1)),
    theta = THETA(Total_Passengers),
    stl_arima = decomposition_model(
      STL(Total_Passengers ~ trend(window = 13) + season(window = "periodic")),
      ARIMA(season_adjust ~ pdq(1,1,0))
    )
  )
arima_auto_report <- models_all |>
  select(arima_auto) |>
  report()

# Прогнозы всех моделей
fc_all <- models_all |>
  forecast(h = nrow(test_data))

accuracy_all <- fc_all |>
  accuracy(test_data)

# Сравнение моделей
models_rank <- accuracy_all |>
  select(.model, RMSE, MAE, MAPE) |>
  arrange(RMSE)

models_rank

best_3 <- models_rank |>
  slice(1:3) |>
  pull(.model)

best_3

# График лучших моделей
p_best <- fc_all |>
  filter(.model %in% best_3) |>
  autoplot(data_period, level = NULL) +
  labs(title = "Прогнозы трех лучших моделей",
       subtitle = paste("Лучшие:", paste(best_3, collapse = ", ")),
       y = "Пассажиры", x = "Год")

# График всех моделей
p_all <- fc_all |>
  ggplot(aes(x = Month, y = .mean, color = .model)) +
  geom_line(alpha = 0.7) +
  geom_line(data = data_period, aes(x = Month, y = Total_Passengers), 
            color = "gray") +
  geom_line(data = test_data, aes(x = Month, y = Total_Passengers), 
            color = "black") +
  labs(title = "Сравнение всех моделей",
       y = "Пассажиры", x = "Год",
       color = "Модель")

p_best
p_all

# Получаем прогнозы трех лучших моделей
fc_best3 <- fc_all |>
  filter(.model %in% best_3)

# Задание 7
best_model <- data_period |>
  model(
    stl_arima = decomposition_model(
      STL(Total_Passengers ~ trend(window = 13) + season(window = "periodic")),
      ARIMA(season_adjust ~ pdq(1,1,0))
    )
  )

p_res <- best_model |>
  gg_tsresiduals() +
  labs(title = "Остатки модели STL+ARIMA")

fc_24 <- best_model |>
  forecast(h = 24)

p_fc <- fc_24 |>
  autoplot(data_period) +
  labs(title = "Прогноз на 2 года вперед",
       subtitle = "Модель: STL+ARIMA",
       y = "Пассажиры", x = "Год")

p_res
p_fc

# Задание 8
full_data <- ts_data1 |>
  filter(year(Month) >= 1990, year(Month) < 2013)

p_full <- full_data |>
  ggplot(aes(x = Month, y = Total_Passengers)) +
  geom_line() +
  # Структурный сдвиг (начало роста)
  geom_vline(xintercept = as.Date("2000-01-01"), 
             color = "red", linetype = "dashed", size = 1, alpha = 0.7) +
  # Событие 9/11
  geom_vline(xintercept = as.Date("2001-09-01"), 
             color = "orange", linetype = "dashed", size = 1, alpha = 0.7) +
  # Подписи периодов
  annotate("text", x = as.Date("1995-01-01"), y = 100000,
           label = "1990-1999", color = "blue", size = 4) +
  annotate("text", x = as.Date("2005-01-01"), y = 300000,
           label = "2000-2012", color = "darkgreen", size = 4) +
  # Подписи событий
  annotate("text", x = as.Date("2001-06-01"), y = 200000,
           label = "9/11 2001", color = "orange", size = 3.5, angle = 90) +
  annotate("text", x = as.Date("1999-10-01"), y = 200000,
           label = "Структурный\nсдвиг", color = "red", size = 3.5, angle = 90) +
  labs(title = "Международный авиапассажиропоток (1990-2012)",
       subtitle = "Красная линия: начало роста (2000), Оранжевая: 11 сентября 2001",
       y = "Пассажиры", x = "Год")

period_90s <- full_data |> filter(year(Month) < 2000)
period_00s <- full_data |> filter(year(Month) >= 2000)

# Таблица сравнения
comparison_full <- tibble(
  Период = c("1990-1999", "2000-2012"),
  `Среднее` = c(mean(period_90s$Total_Passengers), 
                mean(period_00s$Total_Passengers)),
  `Медиана` = c(median(period_90s$Total_Passengers), 
                median(period_00s$Total_Passengers)),
  `Изменение` = c("-", 
                  paste0("+", round(mean(period_00s$Total_Passengers) / 
                                      mean(period_90s$Total_Passengers) * 100 - 100, 0), "%"))
)

comparison_full

# Boxplot 
p_box_full <- full_data |>
  mutate(Период = ifelse(year(Month) < 2000, "1990-1999", "2000-2012")) |>
  ggplot(aes(x = Период, y = Total_Passengers, fill = Период)) +
  geom_boxplot() +
  labs(title = "Сравнение распределения по десятилетиям",
       y = "Пассажиры", x = "Период") +
  scale_fill_manual(values = c("1990-1999" = "lightblue", "2000-2012" = "lightgreen"))

# Сезонность в разных периодах
p_season_90s <- period_90s |>
  gg_season(Total_Passengers) +
  labs(title = "Сезонность 1990-1999")

p_season_00s <- period_00s |>
  gg_season(Total_Passengers) +
  labs(title = "Сезонность 2000-2012")


p_full
comparison_full
p_box_full
p_season_90s / p_season_00s
