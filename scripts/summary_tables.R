source("./scripts/tidy_sar_panel.R")
source("./scripts/functions.R")

# ----- Apply DoD deflators for summary statistics -----
sar_panel_real <- sar_panel |>
    mutate(
        deflator = deflator_ratio(base_year, to_year = 2024),
        baseline_pauc_total_2024 = baseline_pauc_total * deflator,
        current_pauc_total_2024 = current_pauc_total * deflator,
        treatment_group = if_else(cohort == 0, "control", "treated")
    )

portfolio_levels <- unique(sar_panel_real$platform_portfolio)

# Build summary statistics tables
summary_stats <- sar_panel_real |>
    mutate(
        pauc_growth = pauc_growth * 100) |>
    summarise(
        across(
            c(baseline_pauc_total_2024, current_pauc_total_2024, pauc_growth),
            list(mean = ~ mean(.x, na.rm = TRUE),
                 sd = ~ sd(.x, na.rm = TRUE),
                 min = ~ min(.x, na.rm = TRUE),
                 max = ~ max(.x, na.rm = TRUE)),
            .names = "{.col}__{.fn}")) |>
    pivot_longer(
        everything(),
        names_to = c("variable", "stat"),
        names_sep = "__") |>
    pivot_wider(
        names_from = stat, values_from = value) |>
    relocate(
        mean, sd, min, max, .after = variable)

group_summary <- function(data) {
    tibble(
        n_programs = n_distinct(data$program),
        baseline_pauc_total_2024 = mean(data$baseline_pauc_total_2024, na.rm = TRUE),
        current_pauc_total_2024 = mean(data$current_pauc_total_2024, na.rm = TRUE),
        pauc_growth = mean(data$pauc_growth, na.rm = TRUE)) |>
        bind_cols(
            data |>
                distinct(program, platform_portfolio) |>
                count(platform_portfolio) |>
                complete(platform_portfolio = portfolio_levels, fill = list(n = 0)) |>
                mutate(pct = n / sum(n) * 100) |>
                select(platform_portfolio, pct) |>
                pivot_wider(names_from = platform_portfolio, values_from = pct, names_prefix = "pct_"))
}

comparison_table <- bind_rows(
    all_programs = group_summary(sar_panel_real),
    treated = group_summary(
        filter(sar_panel_real, treatment_group == "treated")),
    control = group_summary(
        filter(sar_panel_real, treatment_group == "control")),
        .id = "group") |>
    pivot_longer(
        -group, names_to = "statistic", values_to = "value") |>
    pivot_wider(
        names_from = group, values_from = value) |>
    relocate(
        all_programs, treated, control, .after = statistic)