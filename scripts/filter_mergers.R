library(tidyverse)
library(stringdist)

mergers <- read.csv("mergers_list.csv") |>
    mutate(
        deal_value = as.numeric(gsub(",", "", deal_value)),
        net_debt_of_target = as.numeric(gsub(",", "", net_debt_of_target)),
        date_announced = as.Date(date_announced, format = "%m-%d-%Y"),
        date_effective = as.Date(date_effective, format = "%m-%d-%Y"),
        year = year(date_effective)
    )

top10_deals <- mergers |>
    filter(between(year, 2011, 2020)) |>
    slice_max(deal_value, n = 15) |>
    select(date_effective, target_full_name, acquiror_full_name, deal_value)

sipri_us_top30 <- read.csv("sipri_top_100.csv") |>
    filter(country == "United States") |>
    arrange(rank_2010) |>
    slice_head(n = 30) |>
    select(rank_2010, company)

# Name changes and subsidiary-to-parent mappings:
# L-3 Communications rebranded as L3 Technologies in 2016
# Sikorsky was a UTC subsidiary; Orbital ATK's predecessor ATK = Alliant Techsystems (SIPRI rank 22 US)
sipri_aliases <- c(
    "L3 Technologies" = "L-3 Communications",
    "Sikorsky Aircraft" = "United Technologies Corp.",
    "Orbital ATK" = "Alliant Techsystems"
)

apply_aliases <- function(name) {
    for (alias in names(sipri_aliases)) {
        if (grepl(alias, name, ignore.case = TRUE)) return(sipri_aliases[[alias]])
    }
    name
}

normalize_co <- function(x) {
    x |>
        tolower() |>
        gsub("[.,]", "", x = _) |>
        gsub("-", " ", x = _) |>
        gsub("\\b(corp|inc|ltd|llc|co|company|corporation|group|holdings|international|the)\\b", " ", x = _) |>
        trimws() |>
        gsub("\\s+", " ", x = _)
}

sipri_norm <- normalize_co(sipri_us_top30$company)

best_sipri_match <- function(name) {
    name <- apply_aliases(name)
    dists <- stringdist(normalize_co(name), sipri_norm, method = "jw")
    i <- which.min(dists)
    if (dists[i] < 0.15) sipri_us_top30$company[i] else NA_character_
}

mergers_top30 <- mergers |>
    mutate(
        target_sipri = sapply(target_full_name, best_sipri_match),
        acquiror_sipri = sapply(acquiror_full_name, best_sipri_match)) |>
    filter(
        !is.na(target_sipri), !is.na(acquiror_sipri)) |>
    select(
        date_effective, target_full_name, acquiror_full_name, deal_value)
