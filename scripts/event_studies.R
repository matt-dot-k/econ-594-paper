library(fixest)
library(didimputation)

# ===== Load prelim R scripts =====
source("./scripts/tidy_sar_panel.R")
source("./scripts/functions.R")

# ===============================================================
# ==================== BASELINE EVENT STUDIES ===================
# ===============================================================

outcomes <- c("current_apuc_log", "apuc_growth", "apuc_diff_yoy")

es_baseline_bjs <- es_wrapper(
    data = sar_panel,
    outcomes = outcomes,
    first_stage = list(
        controls = ~ duration + I(duration^2) + milestone_c | program + report_date),
    horizon = TRUE,
    pretrends = TRUE
)

es_baseline_twfe <- twfe_wrapper(
    data = sar_panel,
    outcomes = outcomes,
    first_stage = list(
        controls = ~ duration + I(duration^2) + milestone_c | program + report_date
    )
)

# ==============================================================
# ==================== HETEROGENEITY CHECKS ====================
# ==============================================================

# ========== Merging Contractor Subsample ==========
merger_parties <- c(
    "Lockheed Martin", "Raytheon", "Sikorksy", "Northrop Grumman", "Harris Corporation",
    "L3 Technologies", "L3Harris", "ITT Exelis", "Longbow LLC", "Alliant TechSystems (ATK)",
    "Pratt & Whitney", "Rockwell Collins"
)

sar_panel_subsample <- sar_panel |>
    filter(
        prime_contractor %in% merger_parties |
        secondary_contractor %in% merger_parties)

es_subsample_bjs <- es_wrapper(
    data = sar_panel_subsample,
    outcomes = outcomes,
    first_stage = list(
        ~ duration + I(duration^2) + milestone_c | report_date + program),
    horizon = TRUE,
    pretrends = TRUE
)

es_subsample_twfe <- twfe_wrapper(
    data = sar_panel_subsample,
    outcomes = outcomes,
    first_stage = list(
        controls = ~ duration + I(duration^2) + milestone_c | program + report_date
    )
)

# ========== Vertical Foreclosure (No Orbital ATK Programs) ==========
sar_panel_noatk <- sar_panel_subsample |>
    filter(
        !(prime_contractor %in% c("Alliant TechSystems (ATK)")),
        !(secondary_contractor %in% c("Alliant TechSystems (ATK)"))
    )

es_noatk_bjs <- es_wrapper(
    data = sar_panel_noatk,
    outcomes = outcomes,
    first_stage = list(
        ~ duration + I(duration^2) + milestone_c | report_date + program),
    horizon = TRUE,
    pretrends = TRUE
)

es_noatk_twfe <- twfe_wrapper(
    data = sar_panel_noatk,
    outcomes = outcomes,
    first_stage = list(
        controls = ~ duration + I(duration^2) + milestone_c | program + report_date
    )
)

cat("\n", rep("=", 20),"Subsample Estimations Complete", rep("=", 20),"\n", sep = "")

# =============== Effects by Service Branch ===============
sar_panel_usaf <- sar_panel |>
    filter(
        service == "Air Force"
    )

es_usaf_bjs <- es_wrapper(
    data = sar_panel_usaf,
    outcomes = c("apuc_growth"),
    first_stage = list(
        ~ duration + I(duration^2) + milestone_c | program + report_date),
    horizon = TRUE,
    pretrends = TRUE
)

es_usaf_twfe <- twfe_wrapper(
    data = sar_panel_usaf,
    outcomes = c("apuc_growth"),
    first_stage = list(
        controls = ~ duration + I(duration^2) + milestone_c | program + report_date
    )
)

sar_panel_army <- sar_panel |>
    filter(
        service == "Army"
    )

es_army_bjs <- es_wrapper(
    data = sar_panel_army,
    outcomes = c("apuc_growth"),
    first_stage = list(
        ~ duration + I(duration^2) + milestone_c | program + report_date),
    horizon = TRUE,
    pretrends = TRUE
)

es_army_twfe <- twfe_wrapper(
    data = sar_panel_army,
    outcomes = c("apuc_growth"),
    first_stage = list(
        controls = ~ duration + I(duration^2) + milestone_c | program + report_date
    )
)

sar_panel_usn <- sar_panel |>
    filter(
        service == "Navy"
    )

es_usn_bjs <- es_wrapper(
    data = sar_panel_usn,
    outcomes = c("apuc_growth"),
    first_stage = list(
        ~ duration + I(duration^2) + milestone_c | program + report_date),
    horizon = TRUE,
    pretrends = TRUE
)

es_usn_twfe <- twfe_wrapper(
    data = sar_panel_usn,
    outcomes = c("apuc_growth"),
    first_stage = list(
        controls = ~ duration + I(duration^2) + milestone_c | program + report_date
    )
)

# ===============================================
# ==================== PLOTS ====================
# ===============================================

outcome_labels <- c(
    current_apuc_log = "Total Procurement Cost (logged)",
    apuc_diff_yoy = "APUC: Annual Change",
    apuc_growth = "APUC: Change from Baseline"
)

# =============== Format Event Study Estimates ===============
es_baseline_data <- bind_rows(
    tidy_event_study(es_baseline_bjs) |> mutate(method = "BJS Imputation"),
    tidy_twfe_es(es_baseline_twfe) |> mutate(method = "Static TWFE")
)

es_subsample_data <- bind_rows(
    tidy_event_study(es_subsample_bjs) |> mutate(method = "BJS Imputation"),
    tidy_twfe_es(es_subsample_twfe) |> mutate(method = "Static TWFE")
)

es_noatk_data <- bind_rows(
    tidy_event_study(es_noatk_bjs) |> mutate(method = "BJS Imputation"),
    tidy_twfe_es(es_noatk_twfe) |> mutate(method = "Static TWFE")
)

es_army_data <- bind_rows(
    tidy_event_study(es_army_bjs) |> mutate(method = "BJS Imputation"),
    tidy_twfe_es(es_army_twfe) |> mutate(method = "Static TWFE")
)

es_usaf_data <- bind_rows(
    tidy_event_study(es_usaf_bjs) |> mutate(method = "BJS Imputation"),
    tidy_twfe_es(es_usaf_twfe) |> mutate(method = "Static TWFE")
)

es_usn_data <- bind_rows(
    tidy_event_study(es_usn_bjs) |> mutate(method = "BJS Imputation"),
    tidy_twfe_es(es_usn_twfe) |> mutate(method = "Static TWFE")
)

# =============== Generate Event Study Charts ===============
baseline_chart <- es_baseline_data |>
    ggplot(
        aes(x = term, y = estimate, color = method, shape = method)) +
    geom_hline(
        yintercept = 0, linetype = "dashed", color = "grey10") +
    geom_vline(
        xintercept = -0.5, linetype = "dashed", color = "grey10") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        position = position_dodge(width = 0.3),
        linewidth = 0.5) +
    geom_point(
        position = position_dodge(width = 0.3), size = 1.5) +
    scale_x_continuous(
        breaks = seq(-6, 6)) +
    scale_y_continuous(
        limits = c(-0.36, 0.36)) +
    facet_wrap(
        ~ outcome, ncol = 1) +
    scale_color_manual(
        values = c("#2a78d6", "#4a3aa7")) +
    labs(
        x = "Years Relative to Merger", y = "Estimated Effect") +
    theme_thesis()

subsample_chart <- es_subsample_data |>
    ggplot(
        aes(x = term, y = estimate, color = method, shape = method)) +
    geom_hline(
        yintercept = 0, linetype = "dashed", color = "grey10") +
    geom_vline(
        xintercept = -0.5, linetype = "dashed", color = "grey10") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        position = position_dodge(width = 0.3),
        linewidth = 0.5) +
    geom_point(
        position = position_dodge(width = 0.3), size = 1.5) +
    scale_x_continuous(
        breaks = seq(-6, 6)) +
    scale_y_continuous(
        limits = c(-0.36, 0.36)) +
    facet_wrap(
        ~ outcome, ncol = 1) +
    scale_color_manual(
        values = c("#2a78d6", "#4a3aa7")) +
    labs(
        x = "Years Relative to Merger", y = "Estimated Effect") +
    theme_thesis()

noatk_chart <- es_noatk_data |>
    ggplot(
        aes(x = term, y = estimate, color = method, shape = method)) +
    geom_hline(
        yintercept = 0, linetype = "dashed", color = "grey10") +
    geom_vline(
        xintercept = -0.5, linetype = "dashed", color = "grey10") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        position = position_dodge(width = 0.3),
        linewidth = 0.5) +
    geom_point(
        position = position_dodge(width = 0.3), size = 1.5) +
    scale_x_continuous(
        breaks = seq(-6, 6)) +
    scale_y_continuous(
        limits = c(-0.36, 0.36)) +
    facet_wrap(
        ~ outcome, ncol = 1) +
    scale_color_manual(
        values = c("#2a78d6", "#4a3aa7")) +
    labs(
        x = "Years Relative to Merger", y = "Estimated Effect") +
    theme_thesis()

army_chart <- es_army_data |>
    ggplot(
        aes(x = term, y = estimate, color = method, shape = method)) +
    geom_hline(
        yintercept = 0, linetype = "dashed", color = "grey10") +
    geom_vline(
        xintercept = -0.5, linetype = "dashed", color = "grey10") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        position = position_dodge(width = 0.3),
        linewidth = 0.5) +
    geom_point(
        position = position_dodge(width = 0.3), size = 1.5) +
    scale_x_continuous(
        breaks = seq(-6, 6)) +
    scale_y_continuous(
        limits = c(-0.6, 0.6)) +
    facet_wrap(
        ~ outcome, ncol = 1) +
    scale_color_manual(
        values = c("#2a78d6", "#4a3aa7")) +
    labs(
        x = "Years Relative to Merger", y = "Estimated Effect") +
    theme_thesis()

usaf_chart <- es_usaf_data |>
    ggplot(
        aes(x = term, y = estimate, color = method, shape = method)) +
    geom_hline(
        yintercept = 0, linetype = "dashed", color = "grey10") +
    geom_vline(
        xintercept = -0.5, linetype = "dashed", color = "grey10") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        position = position_dodge(width = 0.3),
        linewidth = 0.5) +
    geom_point(
        position = position_dodge(width = 0.3), size = 1.5) +
    scale_x_continuous(
        breaks = seq(-6, 6)) +
    scale_y_continuous(
        limits = c(-0.6, 0.6)) +
    facet_wrap(
        ~ outcome, ncol = 1) +
    scale_color_manual(
        values = c("#2a78d6", "#4a3aa7")) +
    labs(
        x = "Years Relative to Merger", y = "Estimated Effect") +
    theme_thesis()
    
usn_chart <- es_usn_data |>
    ggplot(
        aes(x = term, y = estimate, color = method, shape = method)) +
    geom_hline(
        yintercept = 0, linetype = "dashed", color = "grey10") +
    geom_vline(
        xintercept = -0.5, linetype = "dashed", color = "grey10") +
    geom_errorbar(
        aes(ymin = conf.low, ymax = conf.high),
        width = 0.15,
        position = position_dodge(width = 0.3),
        linewidth = 0.5) +
    geom_point(
        position = position_dodge(width = 0.3), size = 1.5) +
    scale_x_continuous(
        breaks = seq(-6, 6)) +
    scale_y_continuous(
        limits = c(-0.6, 0.6)) +
    facet_wrap(
        ~ outcome, ncol = 1) +
    scale_color_manual(
        values = c("#2a78d6", "#4a3aa7")) +
    labs(
        x = "Years Relative to Merger", y = "Estimated Effect") +
    theme_thesis()

# =============== Export Event Study Charts ===============
ggsave("./charts/chart_baseline.png", baseline_chart, width = 7, height = 9, units = "in")
ggsave("./charts/chart_subsample.png", subsample_chart, width = 7, height = 9, units = "in")
ggsave("./charts/chart_noatk.png", noatk_chart, width = 7, height = 9, units = "in")
ggsave("./charts/chart_army.png", army_chart, width = 7, height = 4, units = "in")
ggsave("./charts/chart_usaf.png", usaf_chart, width = 7, height = 4, units = "in" )
ggsave("./charts/chart_usn.png", usn_chart, width = 7, height = 4, units = "in")