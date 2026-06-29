library(fixest)
library(didimputation)
library(ggthemes)

source("tidy_sar_panel.R")

outcomes <- c("current_pauc_log", "pauc_growth", "current_apuc_log", "apuc_growth")

# Run baseline BJS event study
es_baseline <- lapply(setNames(outcomes, outcomes), \(y) {
    did_imputation(
        data = sar_panel,
        yname = y,
        gname = "cohort",
        tname = "report_date",
        idname = "program",
        horizon = 1:10,
        pretrends = -6:-1,
        cluster_var = "program",
        first_stage = ~ duration + milestone_c | report_date + program
    )
})

# Baseline event study estimates
outcome_labels <- c(
    current_apuc_log = "Event Study: Average Procurement Unit Cost (Logged)",
    apuc_growth = "Average Procurement Unit Cost: Change from Baseline",
    apuc_diff_yoy = "Average Procurement Unit Cost: Annual Change"
)

plot_event_study <- function(model, .id = "outcome") {
    # Extract data from event study
    plot_data <- bind_rows(model, .id = "outcome") |>
        mutate(
            term = if_else(
                str_starts(term, "pre"),
                -as.numeric(str_extract(term, "\\d+")),
                as.numeric(term)
            ),
            pre_period = term < 1)

    # Create event study plots        
    plots <- lapply(setNames(outcomes, outcomes), \(y) {
    plot_data |>
        filter(
            outcome == y) |>
        ggplot(
            aes(x = term, y = estimate, color = pre_period)) +
        geom_hline(
            yintercept = 0, linetype = "dashed", color = "grey10") +
        geom_vline(
            xintercept = 0.5, linetype = "dashed", color = "grey10") +
        geom_point(
            size = 2.5) +
        geom_errorbar(
            aes(ymin = conf.low, ymax = conf.high), width = 0.25, linewidth = 1.0) +
        scale_x_continuous(
            breaks = \(x) seq(floor(min(x)), ceiling(max(x)))) +
        scale_color_manual(
            values = c("TRUE" = "#ff7741", "FALSE" = "#3eb489"),
            guide  = "none") +
        coord_cartesian(xlim = c(-11, 10)) +
        labs(
            title = outcome_labels[[y]],
            x = "Years relative to merger",
            y = "Estimated effect") +
        theme_fivethirtyeight() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 12, face = "bold"),
            panel.background = element_rect(fill = "#ffffff"),
            plot.background = element_rect(fill = "#ffffff"))
    })
    return(plots)
}