library(data.table)
library(ggplot2)
library(stringr)
library(GGally)
library(Krafthack2022)

# Plot settings:
base_height <- 1152
base_width <- 2048

dt <- as.data.table(arrow::read_parquet("../Data/input_dataset-2.parquet"))
old_names <- names(dt)
setnames(dt, old_names, str_replace_all(old_names, " ", "_"))

test_dt <- as.data.table(arrow::read_parquet("../Data/prediction_input.parquet"))
old_names_test <- names(test_dt)
setnames(test_dt, old_names_test, str_replace_all(old_names_test, " ", "_"))

bolt_pretension <- fread("../Data/bolt_pretension.csv")

numeric_columns <- names(dt)[!names(dt) %in% c("timepoints", "mode")]
for (column in numeric_columns) {
  ggplot(dt, aes_string("timepoints", column, colour = "mode")) +
    geom_point(size = 0.1)

  base_height <- 1152
  base_width <- 2048
  ggsave(
    paste0(column, "_ts.png"),
    path = "images",
    width = 1.5 * base_width,
    height = 1.5 * base_height,
    units = "px"
  )
}

numeric_columns <- names(dt)[!names(dt) %in% c("timepoints", "mode")]
for (column in numeric_columns) {
  ggplot(
      dt[mode == "operation"],
      aes_string("timepoints", column)
    ) +
    geom_point(size = 0.1)

  base_height <- 1152
  base_width <- 2048
  ggsave(
    paste0(column, "_operation_ts.png"),
    path = "images",
    width = 1.5 * base_width,
    height = 1.5 * base_height,
    units = "px"
  )
}

predictors <- names(test_dt)[-c(1, 8)]
response <- "Bolt_3_Tensile"
suppressWarnings(GGally::ggpairs(
  dt,
  aes(colour = mode, fill = mode),
  columns = c(response, predictors, "mode"),
  legend = 1,
  upper = list("continuous" = "blank"),
  switch = "both"
)) +
  theme(legend.position = "bottom")

responses <- paste0("Bolt_", 1:6, "_Tensile")
for (response in responses) {
  plots <- lapply(predictors, function(p) {
    ggplot(dt, aes_string(p, response, colour = "mode")) +
      geom_point(size = 0.1)
  })
  pp <- ggpubr::ggarrange(plotlist = plots, nrow = 2, ncol = 3)
  ggsave(
    paste0(response, "_scatter.png"),
    path = "images",
    width = 1.5 * base_width,
    height = 1.5 * base_height,
    units = "px"
  )
}

## With burnin and shutdown periods.
dt <- find_burnin_and_shutdown(dt)
responses <- paste0("Bolt_", 1:6, "_Tensile")
for (response in responses) {
  plots <- lapply(predictors, function(p) {
    ggplot(dt, aes_string(p, response, colour = "mode")) +
      geom_point(size = 0.1)
  })
  pp <- ggpubr::ggarrange(plotlist = plots, nrow = 2, ncol = 3)
  ggsave(
    paste0(response, "_scatter.png"),
    path = "images",
    width = 1.5 * base_width,
    height = 1.5 * base_height,
    units = "px"
  )
}



