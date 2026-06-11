library(fixest)
library(didimputation)
library(ggthemes)

source("data_clean.R")

outcomes <- c("pauc_diff_baseline", "apuc_diff_baseline", "pauc_diff_yoy", "apuc_diff_yoy")

# Run baseline BJS event study
es_baseline <- lapply(setNames(outcomes, outcomes), \(y)
    did_imputation(
        data = sar_panel,
        yname = y,
        gname = "cohort",
        tname = "report_date",
        idname = "program",
        horizon = 1:6,
        pretrends = TRUE,
        cluster_var = "program",
        wname = "cohort_size"
    )
)

# Baseline event study estimates
outcome_labels <- c(
    pauc_diff_baseline = "Program Acquisition Unit Cost: Change from Baseline",
    apuc_diff_baseline = "Average Procurement Unit Cost: Change from Baseline",
    pauc_diff_yoy = "Program Acquisition Unit Cost: Year-over-Year Change",
    apuc_diff_yoy = "Average Procurement Acquisition Cost: Year-over-Year Change"
)

plot_data <- bind_rows(es_baseline, .id = "outcome") |>
    mutate(
        term = if_else(
            str_starts(term, "pre"),
            -as.numeric(str_extract(term, "\\d+")),
            as.numeric(term)
        ),
        pre_period = term < 0
    )

event_plots <- lapply(setNames(outcomes, outcomes), \(y) {
    plot_data |>
        filter(
            outcome == y) |>
        ggplot(
            aes(x = term, y = estimate, color = pre_period)
    ) +
        geom_hline(
            yintercept = 0, linetype = "dashed", color = "grey60"
    ) +
        geom_vline(
            xintercept = 0, linetype = "dashed", color = "grey60"
    ) +
        geom_point(
            size = 3.0
    ) +
        geom_errorbar(
            aes(ymin = conf.low, ymax = conf.high), width = 0.25, linewidth = 1.0
    ) +
        scale_x_continuous(
            breaks = \(x) seq(floor(min(x)), ceiling(max(x)))
    ) +
        scale_color_manual(
            values = c("TRUE" = "#ff7741", "FALSE" = "#3eb489"),
            guide  = "none"
    ) +
        coord_cartesian(xlim = c(-6, 6)) +
        labs(
            title = outcome_labels[[y]],
            x = "Event time (years relative to merger)",
            y = "Estimated effect",
            caption = "95% CIs. Red = pre-treatment, blue = post-treatment."
    ) +
        theme(
            plot.title = element_text(size = 20, face = "bold"),
            axis.title = element_text(size = 18, face = "bold"),
            axis.text = element_text(size = 16, face = "bold")
    )
})