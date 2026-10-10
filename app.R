if (!requireNamespace("shiny", quietly = TRUE)) {
  stop("Install Shiny first with install.packages('shiny').")
}

source("bike_model.R")
demand_fit <- fit_bike_demand()
model_levels <- demand_fit$model$xlevels

month_names <- month.name
weekday_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("\
      @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=Manrope:wght@600;700;800&display=swap');
      :root { --ink:#172b27; --muted:#697a73; --green:#176b52; --mint:#e3f1e9; --coral:#e27b5e; --line:#dce5de; --paper:#f6f8f5; }
      body { background-color:var(--paper); background-image:radial-gradient(#d7e2d9 0.7px, transparent 0.7px); background-size:18px 18px; color:var(--ink); font-family:'DM Sans',sans-serif; }
      .container-fluid { max-width:1380px; padding:28px 30px 40px; }
      .masthead { display:flex; align-items:center; justify-content:space-between; gap:16px; padding:8px 2px 25px; border-bottom:1px solid var(--line); margin-bottom:24px; }
      .brand { display:flex; align-items:center; gap:12px; }
      .brand-mark { display:grid; place-items:center; width:42px; height:42px; color:white; background:var(--green); border-radius:12px; font-size:21px; font-weight:700; }
      .brand-title { margin:0; font:800 22px 'Manrope',sans-serif; letter-spacing:0; }
      .brand-subtitle { color:var(--muted); font-size:12px; margin-top:2px; }
      .dataset-tag { color:var(--green); font-size:12px; font-weight:700; border:1px solid #b8d5c5; border-radius:20px; padding:8px 12px; background:#f0f7f2; }
      h2 { color:var(--ink); font:700 18px 'Manrope',sans-serif; margin:0 0 18px; }
      .well { background:#fff; border:1px solid var(--line); box-shadow:0 8px 24px rgba(29,61,46,.05); border-radius:10px; padding:22px; }
      .control-label { color:#42554c; font-size:12px; font-weight:700; }
      .form-control, .selectize-input { border-color:#d5e0d8; border-radius:6px; box-shadow:none; color:var(--ink); }
      .form-control:focus, .selectize-input.focus { border-color:var(--green); box-shadow:0 0 0 2px rgba(23,107,82,.12); }
      .irs--shiny .irs-bar, .irs--shiny .irs-handle { background:var(--green); border-color:var(--green); }
      .prediction-band { display:flex; justify-content:space-between; align-items:flex-end; gap:20px; padding:24px 26px; background:var(--ink); color:#fff; border-radius:10px; margin-bottom:16px; }
      .prediction-label { color:#bad0c5; font-size:12px; font-weight:700; text-transform:uppercase; }
      .prediction-number { font:800 46px 'Manrope',sans-serif; line-height:1.05; margin-top:7px; }
      .prediction-unit { color:#c9d9d1; font-size:13px; padding-bottom:5px; }
      .metric-grid { display:grid; grid-template-columns:repeat(3,minmax(0,1fr)); gap:12px; margin-bottom:16px; }
      .metric { background:#fff; border:1px solid var(--line); border-radius:8px; padding:15px 17px; }
      .metric-label { color:var(--muted); font-size:11px; font-weight:700; text-transform:uppercase; }
      .metric-value { color:var(--ink); font:700 21px 'Manrope',sans-serif; margin-top:6px; }
      .plot-panel { background:#fff; border:1px solid var(--line); border-radius:8px; padding:18px 16px 10px; }
      .plot-heading { display:flex; justify-content:space-between; align-items:center; margin:0 4px 8px; }
      .plot-heading h2 { margin:0; }
      .plot-note { color:var(--muted); font-size:11px; }
      .shiny-plot-output { width:100%; }
      @media (max-width:767px) { .container-fluid { padding:16px 14px 26px; } .masthead { align-items:flex-start; } .brand-title { font-size:18px; } .dataset-tag { font-size:10px; padding:7px 9px; } .prediction-number { font-size:38px; } .metric-grid { gap:7px; } .metric { padding:12px 10px; } .metric-value { font-size:17px; } }
    "))
  ),
  div(class = "masthead",
    div(class = "brand",
      div(class = "brand-mark", "B"),
      div(
        h1(class = "brand-title", "Bike Demand Forecast"),
        div(class = "brand-subtitle", "Hourly rental demand · Washington, DC")
      )
    ),
    div(class = "dataset-tag", "UCI BIKE SHARING · 2011–2012")
  ),
  fluidRow(
    column(
      width = 4,
      sidebarPanel(
        h2("Forecast conditions"),
        selectInput("season", "Season", choices = c("Winter" = "1", "Spring" = "2", "Summer" = "3", "Fall" = "4"), selected = "3"),
        fluidRow(
          column(6, selectInput("year", "Year", choices = c("2011" = "0", "2012" = "1"), selected = "1")),
          column(6, selectInput("month", "Month", choices = setNames(as.character(seq_len(12)), month_names), selected = "6"))
        ),
        sliderInput("hour", "Hour of day", min = 0, max = 23, value = 8, step = 1),
        fluidRow(
          column(6, selectInput("weekday", "Day", choices = setNames(as.character(0:6), weekday_names), selected = "2")),
          column(6, checkboxInput("workingday", "Working day", value = TRUE))
        ),
        checkboxInput("holiday", "Public holiday", value = FALSE),
        selectInput("weather", "Weather", choices = c("Clear / partly cloudy" = "1", "Mist / cloudy" = "2", "Light rain or snow" = "3", "Heavy rain or snow" = "4"), selected = "1"),
        sliderInput("temperature", "Temperature (°C)", min = -8, max = 39, value = 22, step = 1),
        sliderInput("feels_like", "Feels like (°C)", min = -16, max = 50, value = 24, step = 1),
        sliderInput("humidity", "Humidity (%)", min = 0, max = 100, value = 55, step = 1),
        sliderInput("wind", "Wind speed (km/h)", min = 0, max = 67, value = 12, step = 1)
      )
    ),
    column(
      width = 8,
      div(class = "prediction-band",
        div(div(class = "prediction-label", "Estimated rentals"), div(class = "prediction-number", textOutput("prediction", inline = TRUE))),
        div(class = "prediction-unit", "bikes per hour")
      ),
      div(class = "metric-grid",
        div(class = "metric", div(class = "metric-label", "Mean absolute error"), div(class = "metric-value", textOutput("mae", inline = TRUE))),
        div(class = "metric", div(class = "metric-label", "Root mean square error"), div(class = "metric-value", textOutput("rmse", inline = TRUE))),
        div(class = "metric", div(class = "metric-label", "Test R-squared"), div(class = "metric-value", textOutput("r_squared", inline = TRUE)))
      ),
      div(class = "plot-panel",
        div(class = "plot-heading", h2("Model performance"), div(class = "plot-note", "Chronological holdout · final 20%")),
        plotOutput("performance_plot", height = "360px")
      )
    )
  )
)

server <- function(input, output, session) {
  prediction_input <- reactive({
    data.frame(
      season = factor(input$season, levels = model_levels$season),
      yr = factor(input$year, levels = model_levels$yr),
      mnth = factor(input$month, levels = model_levels$mnth),
      hr = factor(input$hour, levels = model_levels$hr),
      holiday = factor(as.integer(input$holiday), levels = model_levels$holiday),
      weekday = factor(input$weekday, levels = model_levels$weekday),
      workingday = factor(as.integer(input$workingday), levels = model_levels$workingday),
      weathersit = factor(input$weather, levels = model_levels$weathersit),
      temp = (input$temperature + 8) / 47,
      atemp = (input$feels_like + 16) / 66,
      hum = input$humidity / 100,
      windspeed = input$wind / 67
    )
  })

  output$prediction <- renderText({
    max(0, round(predict(demand_fit$model, newdata = prediction_input())))
  })
  output$mae <- renderText(sprintf("%.1f bikes", demand_fit$metrics[["mae"]]))
  output$rmse <- renderText(sprintf("%.1f bikes", demand_fit$metrics[["rmse"]]))
  output$r_squared <- renderText(sprintf("%.3f", demand_fit$metrics[["r_squared"]]))

  output$performance_plot <- renderPlot({
    sample_size <- min(240, length(demand_fit$actual))
    sample_rows <- seq_len(sample_size)
    chart_values <- range(demand_fit$actual[sample_rows], demand_fit$predictions[sample_rows])
    plot(
      sample_rows, demand_fit$actual[sample_rows], type = "l", lwd = 2,
      col = "#176b52", ylim = chart_values,
      xlab = "Hourly observations in test period", ylab = "Rentals per hour",
      bty = "n", las = 1, cex.axis = 0.85, cex.lab = 0.9
    )
    lines(sample_rows, demand_fit$predictions[sample_rows], lwd = 2, col = "#e27b5e")
    grid(col = "#e7ece8", lty = 1)
    legend(
      "topleft", legend = c("Actual", "Predicted"),
      col = c("#176b52", "#e27b5e"), lty = 1, lwd = 2,
      bty = "n", horiz = TRUE, cex = 0.85
    )
  })
}

shinyApp(ui = ui, server = server)