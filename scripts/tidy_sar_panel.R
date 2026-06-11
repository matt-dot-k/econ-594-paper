library(readr)
library(tidyverse)
library(collapse)
library(zoo)

sar_panel <- read_csv("./data_files/sar_data_master_panel.csv")

interp_vars <- c("current_pauc_total", "current_pauc_qty", "current_apuc_total", "current_apuc_qty")

# Transform panel to apply interpolation and construct cost-growth variables
sar_panel <- sar_panel |>
    arrange(
        program, report_date) |>
    group_by(
        program) |>
    mutate(
        across(all_of(interp_vars), ~ na.approx(., na.rm = FALSE))) |>
    ungroup() |>
    mutate(
        baseline_pauc_est = case_when(
            baseline_pauc_qty == 0 ~ NA_real_,
            .default = baseline_pauc_total / baseline_pauc_qty),
        baseline_apuc_est = case_when(
            baseline_apuc_qty == 0 ~ NA_real_,
            .default = baseline_apuc_total / baseline_apuc_qty),
        current_pauc_est = case_when(
            current_pauc_qty == 0 ~ NA_real_,
            .default = current_pauc_total / current_pauc_qty),
        current_apuc_est = case_when(
            current_apuc_qty == 0 ~ NA_real_,
            .default = current_apuc_total / current_apuc_qty),
        report_date = year(report_date),
        duration = report_date - base_year,
        milestone_c = as.integer(baseline_type == "PdE")) |>
    fmutate(
        pauc_diff_baseline = (current_pauc_est - baseline_pauc_est) / baseline_pauc_est,
        apuc_diff_baseline = (current_apuc_est - baseline_apuc_est) / baseline_apuc_est,
        pauc_diff_yoy = fgrowth(current_pauc_est, g = program, scale = 1),
        apuc_diff_yoy = fgrowth(current_apuc_est, g = program, scale = 1)) |>
    group_by(
        cohort) |>
    mutate(
        cohort_size = n_distinct(program)) |>
    ungroup() |>
    relocate(
        c("duration", "milestone_c"), .before = "baseline_pauc_total")