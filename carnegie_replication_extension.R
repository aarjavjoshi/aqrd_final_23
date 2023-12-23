library(tidyverse)
library(haven)
library(zoo)
library(gt)
library(glue)
library(modelsummary)
library(fixest)
library(countrycode)
library(data.table)

# Data Cleaning ----------------------------------------------

## Load replication data
load("data/carnegie_marinov_main.rdata")
df <- x

## Extract covariates
covars <- df |> 
  colnames() |> 
  str_subset("cov") |> 
  sapply(function(df) str_c("lag(", df, ", 2)", sep = "")) |> 
  str_c(collapse = " + ")

## Load extension data
newdata <- read_csv("data/newdata.csv") |>
  ## Remove unnecessary rows
  filter(!is.na(`Series Code`)) |> 
  select(-`Series Code`) |> 
  ## Configure panel orientation
  pivot_longer(
    cols = ends_with("]"),
    names_to = "year",
    values_to = "value"
    )|> 
  mutate(year = as.integer(str_sub(year, start = 1, end = 4))) |>
  rename(country = `Country Name`,
         abbrev = `Country Code`) |> 
  pivot_wider(
    id_cols = c(country, abbrev, year),
    names_from = "Series Name",
    values_from = "value"
    ) |> 
  ## Rename World Bank World Development Indicators (WDI) variables
  rename(
    ext_exchange = `Official exchange rate (LCU per US$, period average)`,
    ext_inflation = `Inflation, GDP deflator: linked series (annual %)`,
    ext_current_account = `Current account balance (BoP, current US$)`,
    ext_debt = `External debt stocks, total (DOD, current US$)`,
    ext_claims = `Claims on central government, etc. (% GDP)`,
    ext_savings = `Adjusted net savings, excluding particulate emission damage (current US$)`,
    ext_mfg = `Manufacturing, value added (current US$)`,
    ext_industry = `Industry (including construction), value added (current US$)`,
    ext_capital = `Gross capital formation (current US$)`,
    ext_trade = `Trade (% of GDP)`,
    ext_unemp = `Unemployment, total (% of total labor force) (modeled ILO estimate)`,
    ext_co2 = `CO2 emissions (metric tons per capita)`,
    ext_energy = `Energy use (kg of oil equivalent per capita)`,
    ext_mortality = `Death rate, crude (per 1,000 people)`,
    ext_migration = `Net migration`,
    ext_military = `Military expenditure (current USD)`,
    ext_hiv = `Incidence of HIV, all (per 1,000 uninfected population)`,
    ext_lifespan = `Life expectancy at birth, total (years)`
    ) |> 
  ## Format extension variables as numerics
  mutate_at(
    vars(starts_with("ext_")), 
    as.numeric
    ) |> 
  ## Treat missing values
  mutate(
    ext_hiv = case_when(
      ext_hiv %in% ".." ~ 0,
      .default = as.numeric(ext_hiv)
      )
    ) |> 
  ## Scale specific large variables
  mutate(
    ext_migration = ext_migration / 1000,
    ext_debt = ext_debt / 1000000000,
    ext_capital = ext_capital / 1000000000
  ) |> 
  ## Create three-year rolling averages to use as outcomes in 2SLS regression
  mutate_at(
    vars(starts_with("ext_")), 
    list(avg = ~ rollapply(., width = 3, FUN = mean, align = "left", partial = 0))
    ) |> 
  ## Translate between inconsistent country nomenclature between World Bank and Correlates of War (used by Carnegie & Marinov) 
  mutate(
    ccode = countrycode(
      source = abbrev, 
      origin = "wb",
      destination = "cown"
      )
  ) |> 
  relocate(c(year, ccode), .before = 1)

## Merge authors' data with extension data
replicat <- newdata |> 
  right_join(
    df, 
    by = join_by(ccode, year)
  ) |> 
  group_by(ccode) |> 
  filter(year >= 1987, year <= 2006)

# Table 1 - Summary Stats ------------------------------------------------------

# summ <- df |>
#   select(EV, l2CPcol2, new_empinxavg, polity2avg, ihme_ayem, wdi_exp, wdi_fdi, wdi_imp, wvs_rel) |>
#   pivot_longer(cols = everything()) |>
#   group_by(name) |>
#   summarize(mean = mean(value, na.rm = T),
#             sd = sd(value, na.rm = T),
#             nobs = sum(!is.na(value))) |>
#   mutate(name = case_when(name == "EV" ~ "EU Aid",
#                           name == "l2CPcol2" ~ "Former Colony Status (t-2)",
#                           name == "new_empinxavg" ~ "4-Year Human Empowerment Index",
#                           name == "polity2avg" ~ "4-Year Polity IV Score",
#                           name == "ihme_ayem" ~"Male Average Years Education",
#                           name == "wdi_exp" ~ "Log Exports",
#                           name == "wdi_fdi" ~ "Foreign Direct Investment",
#                           name == "wdi_imp" ~ "Log Imports",
#                           name == "wvs_rel" ~ "Religiosity",
#                           TRUE ~ NA))
# dat |>
#   gt(rowname_col = "name") |>
#   fmt_number(columns = 2:3, decimals = 2) |>
#   cols_label(mean = "Mean",
#              sd = "Standard Deviation",
#              nobs = "Number of Observations\n(excluding NAs)")

replicat |>
  ungroup() |> 
  select(EV, l2CPcol2, new_empinx, polity2, ext_mortality, ext_migration, ext_hiv, ext_unemp, ext_debt, ext_capital, covihme_ayem, covwdi_exp, covwdi_fdi, covwdi_imp, covwvs_rel, coviNY_GDP_PETR_RT_ZS, covdemregion, covloggdp, covloggdpC) |>
  mutate(EV = exp(EV),
         covwdi_exp = exp(covwdi_exp),
         covwdi_imp = exp(covwdi_imp),
         covloggdp = exp(covloggdp) / 1000000000,
         covloggdpC = exp(covloggdpC) / 1000) |> 
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  summarize(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    N = sum(!is.na(value))
  ) |>
  mutate(category = recode(
    name,
    l2CPcol2 = "Instrument",
    EV = "Treatment",
    new_empinx = "Outcome",
    polity2 = "Outcome",
    ext_mortality = "Outcome",
    ext_migration = "Outcome",
    ext_hiv = "Outcome",
    ext_unemp = "Outcome",
    ext_debt = "Outcome",
    ext_capital = "Outcome",
    .default = "Covariate"
  )) |>
  mutate(name = recode_factor(
    name,
    EV = "EU Aid (millions of USD)",
    l2CPcol2 = "Share former colony (t - 2)",
    new_empinx = "CIRI Human Empowerment Index (0 to 14), 4-year avg.",
    polity2 = "Polity IV score (-10 to 10), 4-year avg.",
    ext_mortality = "Death rate (per thousand)",
    ext_migration = "Net migration (thousands)",
    ext_hiv = "HIV rate (per thousand)",
    ext_unemp = "Unemployment rate",
    ext_debt = "External Debt Stocks (billions of USD)",
    ext_capital = "Gross Capital Formation (billions of USD)",
    covihme_ayem = "Average male education (years)",
    covwdi_exp = "Exports (billions of USD)",
    covwdi_imp = "Imports (billions of USD)",
    covwdi_fdi = "Foreign direct investment (billions of USD)",
    covwvs_rel = "Religiosity",
    covdemregion = "Democracies in Region",
    covloggdp = "GDP (billions of USD)",
    covloggdpC = "GDP per capita (billions of USD)",
    coviNY_GDP_PETR_RT_ZS = "Petroleum revenues"
  )) |>
  arrange(name) |>
  group_by(category) |>
  gt() |>
  cols_align("left", columns = 1) |>
  cols_align("center", columns = 2:4) |>
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  fmt_integer(columns = N) |>
  cols_label(name ~ "",
             mean ~ "Mean",
             sd ~ "Std. Dev.") |> 
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_row_groups()) |>
  gtsave("figures/sumstats.tex")


# Table 2 - Replicating Authors' Regressions -----------------------------------

## Generate models
mods1 <- list(
  "one" = feols(
      new_empinxavg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
      data = replicat, 
      cluster = c("ccode", "year"),
      panel.id = ~ccode + year
    ),
  "two" = feols(
      as.formula(glue("new_empinxavg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
      data = replicat, 
      cluster = c("ccode", "year"),
      panel.id = ~ccode+year
    ),
  "three" = feols(
      polity2avg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
      data = replicat,
      cluster = c("ccode", "year"),
      panel.id = ~ ccode + year
    ),
  "four" = feols(
      as.formula(glue("polity2avg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
      data = replicat,
      cluster = c("ccode", "year"),
      panel.id = ~ ccode + year
    )
  )

## Compile regresion table
fig1 <- modelsummary(
  mods1,
  gof_map = NA,
  coef_map = list("fit_lag(EV)" = "Effect of Aid"),
  add_rows = data.frame(
    label = c(
      "Countries", 
      "Years", 
      "Covariates", 
      "Country Fixed Effects", 
      "Year Fixed Effects", 
      "N"
      ),
    one = c(
      mods1[["one"]][["fixef_sizes"]][["ccode"]], 
      mods1[["one"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods1[["one"]][["nobs"]]
      ),
    two = c(
      mods1[["two"]][["fixef_sizes"]][["ccode"]], 
      mods1[["two"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods1[["two"]][["nobs"]]
    ),
    three = c(
      mods1[["three"]][["fixef_sizes"]][["ccode"]], 
      mods1[["three"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods1[["three"]][["nobs"]]
    ),
    four = c(
      mods1[["four"]][["fixef_sizes"]][["ccode"]], 
      mods1[["four"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods1[["four"]][["nobs"]]
    )
    ),
  output = "gt"
) |> 
  sub_values(
    columns = 1, 
    rows = 2, 
    values = "", 
    replacement = "(Standard Error)"
    ) |> 
  tab_spanner(
    label = md("CIRI Human<br>Empowerment Index"),
    columns = 2:3,
    level = 1
    ) |> 
  tab_spanner(
    label = md("Polity IV<br>Combined Score"), 
    columns = 4:5, 
    level = 1
  ) |> 
  tab_spanner(
    label = md("Dependent Variable<br>(4-Year Average)"), 
    columns = 1, 
    level = 1
  ) |> 
  ## Remove column labels to leave tab spanners
  cols_label(everything() ~ "") |> 
  ## Text style
  tab_options(
    column_labels.border.bottom.style = "none",
    column_labels.padding = 4,
    column_labels.font.weight = "bold"
  ) |> 
  tab_style(
    style = list(
      cell_text(align = "left",
                v_align = "middle")
    ),
    locations = cells_column_spanners(
      spanners = starts_with("Dependent")
    )
  ) |> 
  fmt_integer(rows = 8) |> 
  tab_source_note(source_note = "Note: In columns 2 and 4, the following covariates are not shown: Average Years Education, Log Exports, FDI, Log Imports, Religiosity, Petroleum Revenues, Democracies in Region, Log GDP, and Log GDP per Capita. Dummies indicating missing values are also not shown. Fixed effects held for country and year. Standard errors, shown in parantheses, are clustered at the levels of country and year. First-stage coefficient on Twice-Lagged Former Colony Status for the CIRI regression is 0.160 (SE = 0.047). First-stage coefficient on the Twice-Lagged Former Colony Status for Polity IV Combined Score regression is 0.171 (SE = 0.053).") |> 
  tab_header(
    title = md("**Table 2**: Two-Stage Least Squares Estimates of Governance Effects of Logged EU Aid (in Year *t-1*)"),
    subtitle = md("Dependent Variables Averaged over Years *t* through *t+3*")
  ) 
fig1 |> gtsave("figures/replication1.tex")

# Table 3 - Extension with First Stage Regression ---------------------------------

## Generate models
mods2 <- list(
  "Naive" = feols(lag(EV) ~ l2CPcol2 | ccode + year, 
               panel.id = ~ccode + year,
               data = replicat),
  "Mortality" = feols(lag(EV) ~ l2CPcol2 + lag(ext_mortality, 2) | ccode + year, 
                           panel.id = ~ccode + year,
                           data = replicat),
  "Migration" = feols(lag(EV) ~ l2CPcol2 + lag(ext_migration, 2) | ccode + year, 
                           panel.id = ~ccode + year,
                           data = replicat),
  "HIV" = feols(lag(EV) ~ l2CPcol2 + lag(ext_hiv, 2) | ccode + year, 
                     panel.id = ~ccode + year,
                     data = replicat)
)

## Compile figure
fig2 <- modelsummary(
  mods2,
  gof_map = NA,
  coef_map = list(
    "l2CPcol2" = "Former Colony Status (t-1)",
    "lag(ext_mortality, 2)" = "Crisis (t-1)",
    "lag(ext_lifespan, 2)" = "Crisis (t-1)",
    "lag(ext_migration, 2)" = "Crisis (t-1)",
    "lag(ext_hiv, 2)" = "Crisis (t-1)"
    ),
  add_rows = data.frame(
    label = c("Countries", "Years", "Crisis", "Country Fixed Effects", "Year Fixed Effects", "N"),
    `Naive` = c(
      mods2[["Naive"]][["fixef_sizes"]][["ccode"]], 
      mods2[["Naive"]][["fixef_sizes"]][["year"]], 
      "None", 
      "Yes",
      "Yes",
      mods2[["Naive"]][["nobs"]]
    ),
    `Mortality` = c(
      mods2[["Mortality"]][["fixef_sizes"]][["ccode"]], 
      mods2[["Mortality"]][["fixef_sizes"]][["year"]], 
      "Death Rate (per thousand)", 
      "Yes",
      "Yes",
      mods2[["Mortality"]][["nobs"]]
    ),
    `Migration` = c(
      mods2[["Migration"]][["fixef_sizes"]][["ccode"]], 
      mods2[["Migration"]][["fixef_sizes"]][["year"]], 
      "Net Migration", 
      "Yes",
      "Yes",
      mods2[["Migration"]][["nobs"]]
    ),
    `HIV` = c(
      mods2[["HIV"]][["fixef_sizes"]][["ccode"]], 
      mods2[["HIV"]][["fixef_sizes"]][["year"]], 
      "HIV Rate (per thousand)", 
      "Yes",
      "Yes",
      mods2[["HIV"]][["nobs"]]
    )
  ),
  output = "gt"
) |> 
  sub_values(
    columns = 1, 
    rows = 2, 
    values = "", 
    replacement = "(Standard Error)"
  ) |> 
  sub_values(
    columns = 1, 
    rows = 4, 
    values = "", 
    replacement = "(Standard Error)"
  ) |> 
  cols_label(
    matches(" ") ~ "Effect on EU Aid"
  ) |> 
  tab_options(
    column_labels.border.bottom.style = "none",
    column_labels.padding = 4,
    column_labels.font.weight = "bold"
  ) |>  
  fmt_integer(rows = matches("N")) |> 
  tab_source_note(source_note = "Note: Fixed effects held for country and year. Robust standard errors (accounting for multi-way clustering at the levels of country and year) are shown in parentheses. HIV regression only contains years starting with 1991, at the onset of the World Bank's HIV data collection.") |>
  tab_header(
    title = md("**Table 3**: Fixed Effect Estimates of First-Stage Regression"),
    subtitle = md("Effect of Humanitarian Crisis and Colony Status (Each in Year *t-1*) on EU Aid")
    )
fig2 |> gtsave("figures/extension1.tex")

# Table 4 - Extension with Public Health Data ----------------------------------

## Generate models
mods3 <- list(
  "mortality1" = feols(ext_mortality_avg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "mortality2" = feols(as.formula(glue("ext_mortality_avg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "migration1" = feols(ext_migration_avg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "migration2" = feols(as.formula(glue("ext_migration_avg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "hiv1" = feols(ext_hiv_avg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
                 data = replicat,
                 panel.id = ~ccode + year,
                 cluster = c("ccode", "year")),
  "hiv2" = feols(as.formula(glue("ext_hiv_avg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
                 data = replicat,
                 panel.id = ~ccode + year,
                 cluster = c("ccode", "year"))
)

## Compile figures
fig3 <- modelsummary(
  mods3,
  gof_map = NA,
  coef_map = list("fit_lag(EV)" = "Effect of Aid"),
  add_rows = data.frame(
    label = c("Countries", "Years", "Covariates", "Country Fixed Effects", "Year Fixed Effects", "N"),
    mortality1 = c(
      mods3[["mortality1"]][["fixef_sizes"]][["ccode"]], 
      mods3[["mortality1"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods3[["mortality1"]][["nobs"]]
    ),
    mortality2 = c(
      mods3[["mortality2"]][["fixef_sizes"]][["ccode"]], 
      mods3[["mortality2"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods3[["mortality2"]][["nobs"]]
    ),
    migration1 = c(
      mods3[["migration1"]][["fixef_sizes"]][["ccode"]], 
      mods3[["migration1"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods3[["migration1"]][["nobs"]]
    ),
    migration2 = c(
      mods3[["migration2"]][["fixef_sizes"]][["ccode"]], 
      mods3[["migration2"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods3[["migration2"]][["nobs"]]
    ),
    hiv1 = c(
      mods3[["hiv1"]][["fixef_sizes"]][["ccode"]], 
      mods3[["hiv1"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods3[["hiv1"]][["nobs"]]
    ),
    hiv2 = c(
      mods3[["hiv2"]][["fixef_sizes"]][["ccode"]], 
      mods3[["hiv2"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods3[["hiv2"]][["nobs"]]
    )
  ),
  output = "gt"
) |> 
  sub_values(
    columns = 1, 
    rows = 2, 
    values = "", 
    replacement = "(Standard Error)"
  ) |> 
  tab_spanner(
    label = "Death Rate (per thousands)",
    columns = starts_with("mortality"),
    level = 1
  ) |> 
  tab_spanner(
    label = "Net Migration (thousands)", 
    columns = starts_with("migration"), 
    level = 1
  ) |> 
  tab_spanner(
    label = "HIV Rate (per thousands)",
    columns = starts_with("hiv"),
    level = 1
  ) |> 
  tab_spanner(
    label = "Dependent Variable (3-Year Average)", 
    columns = 1, 
    level = 1
  ) |> 
  cols_label(everything() ~ "") |> 
  tab_options(
    column_labels.border.bottom.style = "none",
    column_labels.padding = 4,
    column_labels.font.weight = "bold"
  ) |> 
  tab_style(
    style = list(
      cell_text(align = "left",
                v_align = "bottom")
    ),
    locations = cells_column_spanners(
      spanners = starts_with("Dependent")
    )
  ) |> 
  fmt_integer(rows = 8) |> 
  tab_source_note(source_note = "Note: In columns 2, 4, and 6, the following covariates are not shown: Average Years Education, Log Exports, FDI, Log Imports, Religiosity, Petroleum Revenues, Democracies in Region, Log GDP, and Log GDP per Capita. Dummies indicating missing values are also not shown. Fixed effects held for country and year. Standard errors, shown in parantheses, are clustered at the levels of country and year. First-stage coefficient on Twice-Lagged Former Colony Status for the Death Rate and Net Migration regressions are 0.174 (SE = 0.059). First-stage coefficient on Twice-Lagged Former Colony Status for the HIV Rate regression is 0.117 (SE = 0.058). HIV regression only contains years starting with 1990, at the onset of the World Bank's HIV data collection.") |> 
  tab_header(
    title = md("**Table 4**: Two-Stage Least Squares Estimates of Public Health Effects of Logged EU Aid (in Year *t-1*)"),
    subtitle = md("Dependent Variables Averaged over Years *t* through *t+2*")
  )
fig3 |>  gtsave("figures/extension2.tex")

# Table 5 - Extension with Economic Data ---------------------------------------

## Generate models
mods4 <- list(
  "unemp1" = feols(ext_unemp_avg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "unemp2" = feols(as.formula(glue("ext_unemp_avg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "debt1" = feols(ext_debt_avg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "debt2" = feols(as.formula(glue("ext_debt_avg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
                       data = replicat,
                       panel.id = ~ccode + year,
                       cluster = c("ccode", "year")),
  "capital1" = feols(ext_capital_avg ~ 1 | ccode + year | lag(EV) ~ l2CPcol2, 
                 data = replicat,
                 panel.id = ~ccode + year,
                 cluster = c("ccode", "year")),
  "capital2" = feols(as.formula(glue("ext_capital_avg ~ {covars} | ccode + year | lag(EV) ~ l2CPcol2")), 
                 data = replicat,
                 panel.id = ~ccode + year,
                 cluster = c("ccode", "year"))
)

## Compile figures
fig4 <- modelsummary(
  mods4,
  gof_map = NA,
  coef_map = list("fit_lag(EV)" = "Effect of Aid"),
  add_rows = data.frame(
    label = c("Countries", "Years", "Covariates", "Country Fixed Effects", "Year Fixed Effects", "N"),
    unemp1 = c(
      mods4[["unemp1"]][["fixef_sizes"]][["ccode"]], 
      mods4[["unemp1"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods4[["unemp1"]][["nobs"]]
    ),
    unemp2 = c(
      mods4[["unemp2"]][["fixef_sizes"]][["ccode"]], 
      mods4[["unemp2"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods4[["unemp2"]][["nobs"]]
    ),
    debt1 = c(
      mods4[["debt1"]][["fixef_sizes"]][["ccode"]], 
      mods4[["debt1"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods4[["debt1"]][["nobs"]]
    ),
    debt2 = c(
      mods4[["debt2"]][["fixef_sizes"]][["ccode"]], 
      mods4[["debt2"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods4[["debt2"]][["nobs"]]
    ),
    capital1 = c(
      mods4[["capital1"]][["fixef_sizes"]][["ccode"]], 
      mods4[["capital1"]][["fixef_sizes"]][["year"]], 
      "No", 
      "Yes",
      "Yes",
      mods4[["capital1"]][["nobs"]]
    ),
    capital2 = c(
      mods4[["capital2"]][["fixef_sizes"]][["ccode"]], 
      mods4[["capital2"]][["fixef_sizes"]][["year"]], 
      "Yes", 
      "Yes",
      "Yes",
      mods4[["capital2"]][["nobs"]]
    )
  ),
  output = "gt"
) |> 
  sub_values(
    columns = 1, 
    rows = 2, 
    values = "", 
    replacement = "(Standard Error)"
  ) |> 
  tab_spanner(
    label = md("Unemployment Rate<br>(%age of Labor Force)"),
    columns = starts_with("unemp"),
    level = 1
  ) |> 
  tab_spanner(
    label = md("External Debt Stocks<br>(billions of USD)"), 
    columns = starts_with("debt"), 
    level = 1
  ) |> 
  tab_spanner(
    label =md("Gross Capital Formation<br>(billions of USD)"),
    columns = starts_with("capital"),
    level = 1
  ) |> 
  tab_spanner(
    label = "Dependent Variable (3-Year Average)", 
    columns = 1, 
    level = 1
  ) |> 
  cols_label(everything() ~ "") |> 
  tab_options(
    column_labels.border.bottom.style = "none",
    column_labels.padding = 4,
    column_labels.font.weight = "bold"
  ) |> 
  tab_style(
    style = list(
      cell_text(align = "left",
                v_align = "bottom")
    ),
    locations = cells_column_spanners(
      spanners = starts_with("Dependent")
    )
  ) |> 
  fmt_integer(rows = 8) |> 
  tab_source_note(source_note = "Note: In columns 2, 4, and 6, the following covariates are not shown: Average Years Education, Log Exports, FDI, Log Imports, Religiosity, Petroleum Revenues, Democracies in Region, Log GDP, and Log GDP per Capita. Dummies indicating missing values are also not shown. Fixed effects held for country and year. Standard errors, shown in parantheses, are clustered at the levels of country and year. First-stage coefficient on Twice-Lagged Former Colony Status for the Unemployment, Debt, and Capital regressions are 0.109 (SE = 0.048), 0.170 (SE = 0.081), and 0.193 (SE = 0.063), respectively. Unemployment regression only contains years starting with 1990, at the onset of the World Bank's unemployment data collection.") |> 
  tab_header(
    title = md("**Table 4**: Two-Stage Least Squares Estimates of Economic Effects of EU Aid (in Year *t-1*)"),
    subtitle = md("Dependent Variables Averaged over Years *t* through *t+2*")
  )
fig4 |>  gtsave("figures/extension3.tex")
