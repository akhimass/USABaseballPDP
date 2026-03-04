#!/usr/bin/env Rscript

# Main entry point to run the full PDP reporting pipeline.

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "stringr",
  "patchwork",
  "gt",
  "scales",
  "janitor",
  "rmarkdown"
)

install_if_missing <- function(pkgs) {
  installed <- rownames(installed.packages())
  to_install <- setdiff(pkgs, installed)
  if (length(to_install) > 0) {
    message("Installing missing packages: ", paste(to_install, collapse = ", "))
    install.packages(to_install, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}

install_if_missing(required_packages)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
  library(patchwork)
  library(gt)
  library(scales)
  library(janitor)
  library(rmarkdown)
})

run_pipeline <- function(excel_path) {
  root_dir <- getwd()

  dir.create(file.path(root_dir, "R"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(root_dir, "reports"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(root_dir, "outputs"), showWarnings = FALSE, recursive = TRUE)

  source(file.path(root_dir, "R", "utils.R"), local = TRUE)

  if (!file.exists(excel_path)) {
    stop("Excel file not found at path: ", excel_path)
  }

  message("Reading Excel data...")
  trackman_path <- file.path(root_dir, "outputs", "trackman_data.csv")
  drift_path <- file.path(root_dir, "outputs", "2d_drift.csv")

  trackman_data <- readxl::read_excel(excel_path, sheet = "Trackman Data") %>%
    janitor::remove_empty("cols")
  drift_data <- readxl::read_excel(excel_path, sheet = "2D-Drift") %>%
    janitor::remove_empty("cols")

  utils_write_csv_safe(trackman_data, trackman_path)
  utils_write_csv_safe(drift_data, drift_path)

  message("Rendering Pitcher 2 report (PDF)...")
  try({
    rmarkdown::render(
      input = file.path(root_dir, "reports", "pitcher2_report.Rmd"),
      output_format = "pdf_document",
      output_file = file.path(root_dir, "outputs", "Pitcher2_Pitching_Report.pdf"),
      params = list(excel_path = excel_path),
      envir = new.env()
    )
  }, silent = TRUE)

  message("Rendering Pitcher 2 report (HTML fallback)...")
  rmarkdown::render(
    input = file.path(root_dir, "reports", "pitcher2_report.Rmd"),
    output_format = "html_document",
    output_file = file.path(root_dir, "outputs", "Pitcher2_Pitching_Report.html"),
    params = list(excel_path = excel_path),
    envir = new.env()
  )

  message("Rendering Hitter 5 report (PDF)...")
  try({
    rmarkdown::render(
      input = file.path(root_dir, "reports", "hitter5_report.Rmd"),
      output_format = "pdf_document",
      output_file = file.path(root_dir, "outputs", "Hitter5_Hitting_Report.pdf"),
      params = list(excel_path = excel_path),
      envir = new.env()
    )
  }, silent = TRUE)

  message("Rendering Hitter 5 report (HTML fallback)...")
  rmarkdown::render(
    input = file.path(root_dir, "reports", "hitter5_report.Rmd"),
    output_format = "html_document",
    output_file = file.path(root_dir, "outputs", "Hitter5_Hitting_Report.html"),
    params = list(excel_path = excel_path),
    envir = new.env()
  )

  message("Generating Player 7 2D drift plots...")
  drift_plot <- make_player7_2d_drift_plot(drift_data)

  ggplot2::ggsave(
    filename = file.path(root_dir, "outputs", "Player7_2DDrift.png"),
    plot = drift_plot,
    width = 8,
    height = 4,
    dpi = 300
  )

  ggplot2::ggsave(
    filename = file.path(root_dir, "outputs", "Player7_2DDrift.pdf"),
    plot = drift_plot,
    width = 8,
    height = 4
  )

  message("All outputs generated in the 'outputs' directory.")

  invisible(TRUE)
}

if (sys.nframe() == 0) {
  default_excel_path <- "/Users/akhichappidi/PDP/2025 PDP Data Exercise.xlsx"
  run_pipeline(default_excel_path)
}

