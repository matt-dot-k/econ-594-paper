library(readr)
library(tidyverse)
library(collapse)
library(zoo)

sar_panel <- read_csv("./data/sar_data_master_panel.csv")
interp_vars <- c("current_pauc_total", "current_pauc_qty", "current_apuc_total", "current_apuc_qty")

# Transform panel to apply interpolation and construct cost-growth variables
sar_panel <- sar_panel |>
    arrange(
        program, report_date) |>
    filter(
        !(program %in% c("JTRS GMR", "JLENS", "C-130 AMP"))) |> 
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
        baseline_pauc_log = case_when(
            baseline_pauc_total == 0 ~ NA_real_,
            .default = log(baseline_pauc_total)),
        baseline_apuc_log = case_when(
            baseline_apuc_total == 0 ~ NA_real_,
            .default = log(baseline_apuc_total)),
        current_pauc_log = case_when(
            current_pauc_total == 0 ~ NA_real_,
            .default = log(current_pauc_total)),
        current_apuc_log = case_when(
            current_apuc_total == 0 ~ NA_real_,
            .default = log(current_apuc_total)),
        report_date = lubridate::year(report_date),
        duration = report_date - base_year,
        milestone_c = as.integer(baseline_type == "PdE"),
        event_time = report_date - cohort,
        event_time = if_else(
            cohort == 0,
            -999L,
            as.integer(event_time))) |>
    fmutate(
        pauc_growth = (current_pauc_est - baseline_pauc_est) / baseline_pauc_est,
        apuc_growth = (current_apuc_est - baseline_apuc_est) / baseline_apuc_est,
        pauc_diff_yoy = fgrowth(current_pauc_est, g = program, scale = 1),
        apuc_diff_yoy = fgrowth(current_apuc_est, g = program, scale = 1)) |>
    relocate(
        c("duration", "milestone_c"), .before = "baseline_pauc_total")