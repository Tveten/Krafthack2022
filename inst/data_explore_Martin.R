library(data.table)
library(ggplot2)
library(stringr)
library(GGally)
library(Krafthack2022)

# Plot settings:
base_height <- 1152
base_width <- 2048

data <- read_krafthack_data("../Data")
input2 <- data[[2]]
test_dt <- data[[3]]

for (column in numeric_columns) {
  ggplot(input2, aes_string("timepoints", column, colour = "mode")) +
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

numeric_columns <- names(input2)[!names(input2) %in% c("timepoints", "mode")]
for (column in numeric_columns) {
  ggplot(
      input2[mode == "operation"],
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
  input2,
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
    ggplot(input2, aes_string(p, response, colour = "mode")) +
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
data <- read_krafthack_data("../Data")
input2 <- data[[2]]
test_dt <- data[[3]]
input2 <- find_burnin_and_shutdown(input2)
predictors <- names(test_dt)[-c(1, 8)]
responses <- paste0("Bolt_", 1:6, "_Tensile")
for (response in responses) {
  plots <- lapply(predictors, function(p) {
    ggplot(input2, aes_string(p, response, colour = "mode")) +
      geom_point(size = 0.1)
  })
  pp <- ggpubr::ggarrange(plotlist = plots, nrow = 2, ncol = 3)
  ggsave(
    paste0(response, "_scatter_more_modes.png"),
    plot = pp,
    path = "images",
    width = 1.5 * base_width,
    height = 1.5 * base_height,
    units = "px"
  )
}

for (k in input2[, unique(segment_nr)]) {
  ggplot(input2[segment_nr == k], aes(timepoints, Unit_4_Power, colour = mode)) +
    geom_point(size = 0.1)
  ggsave(
    paste0("power_epoch", k, ".png"),
    path = "images",
    width = 1.5 * base_width,
    height = 1.5 * base_height,
    units = "px"
  )
}

