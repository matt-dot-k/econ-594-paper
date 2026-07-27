# --------------------------------------------------------
# -------------------- Helper Scripts --------------------
# --------------------------------------------------------

# ----- Wrapper function to calculate DoD deflator given a base year -----
deflator_ratio <- function(base_year, to_year = 2024, deflator_path = "./data/dod_deflators.csv") {
    deflators <- read_csv(deflator_path, show_col_types = FALSE)
    base_index <- deflators$total_deflator[match(base_year, deflators$fiscal_year)]
    to_index <- deflators$total_deflator[match(to_year, deflators$fiscal_year)]

    if (anyNA(base_index)) {
        missing_years <- unique(base_year[is.na(base_index)])
        stop("No deflator found for base year(s): ", paste(missing_years, collapse = ", "))
    }
    if (anyNA(to_index)) stop("No deflator found for to_year: ", to_year)

    ratio <- to_index / base_index
    return(ratio)
}

# ----- Wrapper function for running BJS event studies -----
es_wrapper <- function(data, outcomes, first_stage, horizon = TRUE, pretrends = TRUE,
                       gname = "cohort", tname = "report_date", idname = "program",
                       cluster_var = "program", wname = "baseline_pauc_log") {

    if (inherits(first_stage, "formula")) first_stage <- list(first_stage)
    if (is.null(names(first_stage))) names(first_stage) <- paste0("fs", seq_along(first_stage))

    to_periods <- function(x, from) {
        if (is.null(x) || isTRUE(x) || length(x) > 1) return(x)
        from:(from + x - 1)
    }
    horizon <- to_periods(horizon, from = 0)
    pretrends <- to_periods(pretrends, from = 1)

    lapply(first_stage, \(fs) {
        lapply(setNames(outcomes, outcomes), \(y) {
            did_imputation(
                data = data,
                yname = y,
                gname = gname,
                tname = tname,
                idname = idname,
                horizon = horizon,
                pretrends = pretrends,
                cluster_var = cluster_var,
                first_stage = fs,
                wname = wname
            )
        })
    })
}

# ----- Wrapper function for static TWFE models -----
twfe_wrapper <- function(data, outcomes, first_stage, wname = "baseline_pauc_log", cluster_var = "program") {

    if (inherits(first_stage, "formula")) first_stage <- list(first_stage)
    if (is.null(names(first_stage))) names(first_stage) <- paste0("fs", seq_along(first_stage))

    lapply(first_stage, \(fs) {
        fs_rhs <- sub("^~\\s*", "", paste(deparse(fs), collapse = " "))
        lapply(setNames(outcomes, outcomes), \(y) {
            fml <- as.formula(paste(y, "~ i(event_time, ref = -999) +", fs_rhs))
            feols(
                fml,
                data = data,
                weights = as.formula(paste("~", wname)),
                cluster = as.formula(paste("~", cluster_var))
            )
        })
    })
}

# ----- Wrapper functions for extracting event study data -----
tidy_event_study <- function(model, xlim = c(-6, 7)) {
    bind_rows(lapply(model, bind_rows, .id = "outcome"), .id = "spec") |>
        mutate(
            term = if_else(
                str_starts(term, "pre"),
                -as.numeric(str_extract(term, "\\d+")),
                as.numeric(term)
            ),
            pre_period = term < 1,
            outcome = outcome_labels[outcome]
        ) |>
        filter(term >= xlim[1], term <= xlim[2])
}

tidy_twfe_es <- function(model, xlim = c(-6, 7)) {
    bind_rows(
        lapply(model, \(spec) {
            bind_rows(
                lapply(spec, \(m) broom::tidy(m, conf.int = TRUE)),
                .id = "outcome")
        }),
        .id = "spec"
    ) |>
        mutate(
            term = as.numeric(str_extract(term, "-?\\d+$")),
            pre_period = term < 1,
            outcome = outcome_labels[outcome]
        ) |>
        filter(term >= xlim[1], term <= xlim[2])
}

# ----- Custom ggplot theme -----
theme_thesis <- function(base_size = 12) {
    theme_minimal(base_size = base_size) +
        theme(
            axis.title = element_text(size = rel(1.0),face = "bold", color = "grey10"),
            axis.text = element_text(size = rel(0.8), color = "grey30"),
            axis.ticks = element_line(color = "grey70", linewidth = 0.3),
            panel.grid.major = element_line(color = "grey88", linewidth = 0.3),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(1, "lines"),
            panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4),
            strip.text = element_text(size = rel(1.05), face = "bold", color = "grey10"),
            strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.4),
            legend.title = element_blank(),
            legend.text = element_text(size = rel(0.95)),
            legend.position = "bottom",
            plot.caption = element_text(size = rel(0.75), color = "grey40"),
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA)
        )
}