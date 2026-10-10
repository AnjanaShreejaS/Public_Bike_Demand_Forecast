source("bike_model.R")
result <- fit_bike_demand()

cat("Test set results\n")
cat(sprintf("MAE:  %.2f bikes\n", result$metrics["mae"]))
cat(sprintf("RMSE: %.2f bikes\n", result$metrics["rmse"]))
cat(sprintf("R-squared: %.3f\n", result$metrics["r_squared"]))
cat("\nFirst 10 test predictions:\n")
print(data.frame(
  actual = result$actual,
  predicted = round(result$predictions)
)[seq_len(10), ])