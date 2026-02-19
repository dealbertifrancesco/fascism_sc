rm(list = ls())

### Packages
library(fixest)
library(here)
library(dplyr)
# # Install devtools if needed
# install.packages("devtools")
# Install customtextable
# devtools::install_github("dealbertifrancesco/custom_tex_table")
library(customtextable)

### Load data
setwd(here())
clean_data_dir <- here("data", "processed")
tables_dir     <- here("output", "tables")
PREVIEW_DIR <- here("writing", "preview")

df_iv <- read.csv(file.path(clean_data_dir, "df_iv.csv"))

## ── Control vectors ────────────────────────────────────────────────────────
geo_ctrls      <- c("lpop1911", "larea", "centre_alt", "max_alt")
military_ctrls <- c("veterans")
economic_ctrls <- c("ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy")

## ── Selected sample (lpop1911 > 8.5 ≈ pop > ~5,000 in 1911) ──────────────
LPOP_CUTOFF <- 8.5
df_iv_sel   <- df_iv |> filter(lpop1911 > LPOP_CUTOFF)

cat(sprintf("Full sample:     %d obs\n", nrow(df_iv)))
cat(sprintf("Selected sample: %d obs (lpop1911 > %.1f)\n", nrow(df_iv_sel), LPOP_CUTOFF))

## ── Outcome inventory ──────────────────────────────────────────────────────
outcomes <- list(
  list(var = "fascist_branch",     label = "Fascist Branch",          file = "tab_fascist_branch"),
  list(var = "fascist_violence",   label = "Fascist Violence",        file = "tab_fascist_violence"),
  list(var = "fascist1924_vv",     label = "Fascist Vote Share 1924", file = "tab_fascist_1924"),
  list(var = "fascist1919_vv",     label = "Fascist Vote Share 1919", file = "tab_fascist_1919"),
  list(var = "fascist1921_vv",     label = "Fascist Vote Share 1921", file = "tab_fascist_1921"),
  list(var = "deportations_d",     label = "Deportations (Dummy)",    file = "tab_deportations_d"),
  list(var = "depo_pop_cap1",      label = "Deportations (PC)",       file = "tab_deportations_pc"),
  list(var = "dlf_1926",           label = "DLF 1926",                file = "tab_dlf_1926"),
  list(var = "antifa_d",           label = "Antifascists (Dummy)",    file = "tab_antifa_d"),
  list(var = "share_antifa_pop11", label = "Antifascists (PC)",       file = "tab_antifa_pc")
)

## ════════════════════════════════════════════════════════════════════════════
## MODEL RUNNERS
## ════════════════════════════════════════════════════════════════════════════

## ── 2a. Linear IV: z = stat or Monte directly ─────────────────────────────
run_iv_linear <- function(outcome_var, iv_var, data) {
  fml <- as.formula(paste0(
    outcome_var,
    " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv",
    " | province_fe",
    " | ass1900s_d ~ ", iv_var
  ))
  feols(fml, data = data, cluster = ~provincia1921)
}

## ── 2b. Propensity-score IV: z = pz (fitted prob from logit) ──────────────
# Steps: (i) logit of ass1900s_d on controls + raw IV → pz
#        (ii) feols with pz as instrument
run_iv_pz <- function(outcome_var, iv_var, data) {
  
  pz_col <- paste0("pz_", iv_var)
  
  # First-stage logit
  fml_logit <- as.formula(paste0(
    "ass1900s_d ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv + ",
    iv_var, " | province_fe"
  ))
  logit_mod        <- feglm(fml_logit, data = data, family = binomial())
  data[[pz_col]]   <- predict(logit_mod, newdata = data, type = "response")
  
  # Second stage: use pz as instrument
  fml_iv <- as.formula(paste0(
    outcome_var,
    " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv",
    " | province_fe",
    " | ass1900s_d ~ ", pz_col
  ))
  feols(fml_iv, data = data, cluster = ~provincia1921)
}

## ════════════════════════════════════════════════════════════════════════════
## EXTRACTORS
## ════════════════════════════════════════════════════════════════════════════

## First-stage coefficient + SE for the *actual instrument* used
## For linear IV the instrument is iv_var; for pz IV the instrument is pz_<iv_var>
get_fs <- function(mod, fs_iv_name) {
  fs  <- mod$iv_first_stage[[1]]
  coef <- fs$coefficients[fs_iv_name]
  se   <- sqrt(diag(fs$cov.scaled))[fs_iv_name]
  list(coef = as.numeric(coef), se = as.numeric(se))
}

## Second-stage LATE (endogenous regressor is always named "fit_ass1900s_d")
get_ss <- function(mod) {
  coef <- as.numeric(coef(mod)["fit_ass1900s_d"])
  se   <- as.numeric(se(mod)["fit_ass1900s_d"])
  list(coef = coef, se = se)
}

get_fstat <- function(mod) {
  tryCatch(fitstat(mod, "ivf")[[1]]$stat, error = function(e) NA_real_)
}

## ════════════════════════════════════════════════════════════════════════════
## TABLE BUILDER  (generic: works for both linear IV and pz IV)
## ════════════════════════════════════════════════════════════════════════════
# model_runner : one of run_iv_linear / run_iv_pz
# fs_iv_names  : character(2) — name of the instrument in the first stage for
#                stat and Monte respectively.
#                Linear IV → c("stat", "Monte")
#                PZ IV     → c("pz_stat", "pz_Monte")
# file_suffix  : appended to outcome file stem (e.g. "" or "_pz")

make_iv_table <- function(outcome_var, outcome_label, file_stem,
                          model_runner, fs_iv_names,
                          file_suffix = "") {
  
  datasets <- list(df_iv, df_iv_sel, df_iv, df_iv_sel)
  iv_vars  <- c("stat", "stat", "Monte", "Monte")
  
  # Run the four models
  models <- mapply(
    function(d, z) model_runner(outcome_var, z, d),
    datasets, iv_vars,
    SIMPLIFY = FALSE
  )
  
  # fs_iv_name per column: stat→fs_iv_names[1], Monte→fs_iv_names[2]
  fs_names_by_col <- c(fs_iv_names[1], fs_iv_names[1], fs_iv_names[2], fs_iv_names[2])
  
  # Panel A cells (first-stage coefficient on instrument)
  fs_cells <- mapply(
    function(m, nm) { v <- get_fs(m, nm); est(v$coef, v$se) },
    models, fs_names_by_col,
    SIMPLIFY = FALSE
  )
  
  # Panel B cells (LATE)
  ss_cells <- lapply(models, function(m) { v <- get_ss(m); est(v$coef, v$se) })
  
  # Bottom statistics
  n_obs        <- sapply(models, function(m) m$nobs)
  fstats       <- sapply(models, get_fstat)
  mean_d       <- c(mean(df_iv$ass1900s_d,     na.rm = TRUE),
                    mean(df_iv_sel$ass1900s_d,  na.rm = TRUE),
                    mean(df_iv$ass1900s_d,      na.rm = TRUE),
                    mean(df_iv_sel$ass1900s_d,  na.rm = TRUE))
  mean_y       <- c(mean(df_iv[[outcome_var]],     na.rm = TRUE),
                    mean(df_iv_sel[[outcome_var]],  na.rm = TRUE),
                    mean(df_iv[[outcome_var]],      na.rm = TRUE),
                    mean(df_iv_sel[[outcome_var]],  na.rm = TRUE))
  
  stat_cells <- c(
    as.list(round(n_obs,   0)),
    as.list(round(fstats,  2)),
    as.list(round(mean_d,  3)),
    as.list(round(mean_y,  3))
  )
  
  out_file <- file.path(tables_dir, paste0(file_stem, file_suffix, ".tex"))
  
  create_tex_table(
    filename  = out_file,
    cols      = c("(1) stat / Full", "(2) stat / Selected",
                  "(3) Monte / Full", "(4) Monte / Selected"),
    panels    = list(
      list(
        name  = "Panel A: First Stage",
        rows  = c("Instrument"),
        cells = fs_cells
      ),
      list(
        name  = "Panel B: Second Stage (LATE)",
        rows  = c("Mutual Aid Society (instrumented)"),
        cells = ss_cells
      )
    ),
    caption    = paste0("IV Results — ", outcome_label),
    stats      = c("Observations", "First-Stage F-stat", "Mean(D)", "Mean(Outcome)"),
    stat_cells = stat_cells,
    note       = paste0(
      "Notes: Clustered standard errors at province level in parentheses. ",
      "All specifications include province fixed effects and the full set of ",
      "geographic, economic, and military controls. ",
      "Selected sample: municipalities with lpop1911 $>$ 8.5 ($\\approx$ pop.~5\\,000 in 1911). ",
      "* $p<0.1$; ** $p<0.05$; *** $p<0.01$."
    )
  )
  
  cat("  → written:", out_file, "\n")
  invisible(models)
}

## ════════════════════════════════════════════════════════════════════════════
## LOOP: produce both table families for every outcome
## ════════════════════════════════════════════════════════════════════════════
for (o in outcomes) {
  cat("\n── Outcome:", o$label, "\n")
  
  # Linear IV tables (z = stat / Monte)
  cat("  [linear IV]\n")
  make_iv_table(
    outcome_var   = o$var,
    outcome_label = o$label,
    file_stem     = o$file,
    model_runner  = run_iv_linear,
    fs_iv_names   = c("stat", "Monte"),
    file_suffix   = ""         # e.g. tab_fascist_branch.tex
  )
  
  # Propensity-score IV tables (z = pz_stat / pz_Monte)
  cat("  [pz IV]\n")
  make_iv_table(
    outcome_var   = o$var,
    outcome_label = o$label,
    file_stem     = o$file,
    model_runner  = run_iv_pz,
    fs_iv_names   = c("pz_stat", "pz_Monte"),
    file_suffix   = "_pz"      # e.g. tab_fascist_branch_pz.tex
  )
}


######################## NOTE: REQUIRES LOCAL LATEX COMPILER OR TINITEX #############
local_tex_compiler <- "C:/Users/dealb/AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"
### PREVIEW FILE
preview_all_tables <- function(table_files, out_name = "preview_tables.pdf") {
  all_tables <- sapply(table_files, function(f) {
    paste(readLines(f), collapse = "\n")
  })
  
  full_doc <- paste0(
    "\\documentclass{article}\n",
    "\\usepackage{booktabs}\n",
    "\\usepackage{threeparttable}\n",
    "\\usepackage{array}\n",
    "\\usepackage{geometry}\n",
    "\\geometry{margin=1in}\n",
    "\\begin{document}\n",
    paste(all_tables, collapse = "\n\\clearpage\n"),
    "\n\\end{document}"
  )
  
  tmp_tex <- file.path(PREVIEW_DIR, "preview_tables.tex")
  tmp_pdf <- file.path(PREVIEW_DIR, "preview_tables.pdf")
  writeLines(full_doc, tmp_tex)
  
  pdflatex <- local_tex_compiler
  old_wd <- getwd()
  setwd(PREVIEW_DIR)
  on.exit(setwd(old_wd))
  
  result <- system2(pdflatex,
                    args   = c("-interaction=nonstopmode", "preview_tables.tex"),
                    stdout = TRUE, stderr = TRUE)
  
  if (!file.exists(tmp_pdf)) {
    cat(paste(result, collapse = "\n"), "\n")
    stop("Compilation failed — see pdflatex output above.")
  }
  
  cat("Preview saved to:", tmp_pdf, "\n")
  shell.exec(normalizePath(tmp_pdf))
  invisible(tmp_pdf)
}

# Usage
all_tex_files <- list.files(tables_dir, pattern = "\\.tex$", full.names = TRUE)
preview_all_tables(all_tex_files)