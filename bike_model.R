fit_bike_demand <- function(data_path = file.path("data", "hour.csv")) {
  if (!file.exists(data_path)) {
    stop("Cannot find data/hour.csv. Run from the project folder.")
  }

  bikes <- read.csv(data_path)
  categorical_columns <- c(
    "season", "yr", "mnth", "hr", "holiday", "weekday",
    "workingday", "weathersit"
  )
  bikes[categorical_columns] <- lapply(bikes[categorical_columns], factor)

  split_index <- floor(nrow(bikes) * 0.8)
  train <- bikes[seq_len(split_index), ]
  test <- bikes[(split_index + 1):nrow(bikes), ]

  model <- lm(
    cnt ~ season + yr + mnth + hr + holiday + weekday + workingday +
      weathersit + temp + atemp + hum + windspeed,
    data = train
  )
  predictions <- predict(model, newdata = test)
  actual <- test$cnt

  list(
    model = model,
    actual = actual,
    predictions = predictions,
    metrics = c(
      mae = mean(abs(actual - predictions)),
      rmse = sqrt(mean((actual - predictions)^2)),
      r_squared = 1 - sum((actual - predictions)^2) /
        sum((actual - mean(actual))^2)
    )
  )
}