rm(list = ls())

### Packages
library(fixest)
library(here)
library(dplyr)
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
hist_ctrls <- c("crime1874", "freecity", "lnpop1000")

## ── Selected sample ────────────────────────────────────────────────────────
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

## ── IV inventory: add as many as you like ─────────────────────────────────
## Each entry: raw IV name, display label used in column headers
iv_specs <- list(
  list(raw = "stat",          label = "Statutes"),
  list(raw = "exposure_stat", label = "Exposure") 
)

## ════════════════════════════════════════════════════════════════════════════
## MODEL RUNNERS
## ════════════════════════════════════════════════════════════════════════════

run_iv_linear <- function(outcome_var, iv_var, data) {
  fml <- as.formula(paste0(
    outcome_var,
    " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + .[hist_ctrls] + psu1919_vv",
    " | province_fe",
    " | ass1900s_d ~ ", iv_var
  ))
  feols(fml, data = data, cluster = ~provincia1921)
}

run_iv_pz <- function(outcome_var, iv_var, data) {
  pz_col <- paste0("pz_", iv_var)
  fml_logit <- as.formula(paste0(
    "ass1900s_d ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + .[hist_ctrls] + psu1919_vv + ",
    iv_var, " | province_fe"
  ))
  logit_mod        <- feglm(fml_logit, data = data, family = binomial())
  data[[pz_col]]   <- predict(logit_mod, newdata = data, type = "response")
  fml_iv <- as.formula(paste0(
    outcome_var,
    " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + .[hist_ctrls] + psu1919_vv",
    " | province_fe",
    " | ass1900s_d ~ ", pz_col
  ))
  feols(fml_iv, data = data, cluster = ~provincia1921)
}

## ════════════════════════════════════════════════════════════════════════════
## EXTRACTORS
## ════════════════════════════════════════════════════════════════════════════

get_fs <- function(mod, fs_iv_name) {
  fs   <- mod$iv_first_stage[[1]]
  coef <- fs$coefficients[fs_iv_name]
  se   <- sqrt(diag(fs$cov.scaled))[fs_iv_name]
  list(coef = as.numeric(coef), se = as.numeric(se))
}

get_ss <- function(mod) {
  coef <- as.numeric(coef(mod)["fit_ass1900s_d"])
  se   <- as.numeric(se(mod)["fit_ass1900s_d"])
  list(coef = coef, se = se)
}

get_fstat <- function(mod) {
  tryCatch(fitstat(mod, "ivf")[[1]]$stat, error = function(e) NA_real_)
}

## ════════════════════════════════════════════════════════════════════════════
## TABLE BUILDER  — now driven by iv_specs, any number of IVs
## ════════════════════════════════════════════════════════════════════════════
## model_runner  : run_iv_linear or run_iv_pz
## fs_name_fn    : function(raw_iv_name) → name of instrument in first stage
##                 Linear: identity  (raw name is the FS instrument name)
##                 PZ:     prepend "pz_"

make_iv_table <- function(outcome_var, outcome_label, file_stem,
                          model_runner, fs_name_fn,
                          file_suffix = "") {
  
  # Expand: for every IV × {full, selected} → one column
  col_specs <- do.call(rbind, lapply(iv_specs, function(iv) {
    data.frame(
      raw      = iv$raw,
      label    = iv$label,
      sample   = c("Full", "Selected"),
      stringsAsFactors = FALSE
    )
  }))
  # col_specs row order: IV1/Full, IV1/Selected, IV2/Full, IV2/Selected, ...
  
  n_cols <- nrow(col_specs)
  
  # Column headers
  col_headers <- paste0(
    "(", seq_len(n_cols), ") ",
    col_specs$label, " / ", col_specs$sample
  )
  
  # Run models
  models <- vector("list", n_cols)
  for (i in seq_len(n_cols)) {
    d <- if (col_specs$sample[i] == "Full") df_iv else df_iv_sel
    models[[i]] <- model_runner(outcome_var, col_specs$raw[i], d)
  }
  
  # First-stage cells
  fs_cells <- vector("list", n_cols)
  for (i in seq_len(n_cols)) {
    fs_nm      <- fs_name_fn(col_specs$raw[i])
    v          <- get_fs(models[[i]], fs_nm)
    fs_cells[[i]] <- est(v$coef, v$se)
  }
  
  # Second-stage (LATE) cells
  ss_cells <- lapply(models, function(m) { v <- get_ss(m); est(v$coef, v$se) })
  
  # Summary statistics — one value per column
  n_obs  <- sapply(models, function(m) m$nobs)
  fstats <- sapply(models, get_fstat)
  mean_d <- sapply(seq_len(n_cols), function(i) {
    d <- if (col_specs$sample[i] == "Full") df_iv else df_iv_sel
    mean(d$ass1900s_d, na.rm = TRUE)
  })
  mean_y <- sapply(seq_len(n_cols), function(i) {
    d <- if (col_specs$sample[i] == "Full") df_iv else df_iv_sel
    mean(d[[outcome_var]], na.rm = TRUE)
  })
  
  stat_cells <- c(
    as.list(round(n_obs,   0)),
    as.list(round(fstats,  2)),
    as.list(round(mean_d,  3)),
    as.list(round(mean_y,  3))
  )
  
  out_file <- file.path(tables_dir, paste0(file_stem, file_suffix, ".tex"))
  
  create_tex_table(
    filename   = out_file,
    cols       = col_headers,
    panels     = list(
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
## LOOP
## ════════════════════════════════════════════════════════════════════════════
for (o in outcomes) {
  cat("\n── Outcome:", o$label, "\n")
  
  cat("  [linear IV]\n")
  make_iv_table(
    outcome_var   = o$var,
    outcome_label = o$label,
    file_stem     = o$file,
    model_runner  = run_iv_linear,
    fs_name_fn    = function(raw) raw,          # FS instrument name = raw IV name
    file_suffix   = ""
  )
  
  cat("  [pz IV]\n")
  make_iv_table(
    outcome_var   = o$var,
    outcome_label = o$label,
    file_stem     = o$file,
    model_runner  = run_iv_pz,
    fs_name_fn    = function(raw) paste0("pz_", raw),  # FS instrument name = pz_<raw>
    file_suffix   = "_pz"
  )
}

## ════════════════════════════════════════════════════════════════════════════
## PREVIEW
## ════════════════════════════════════════════════════════════════════════════
local_tex_compiler <- "C:/Users/dealb/AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"

preview_all_tables <- function(table_files, out_name = "preview_tables.pdf") {
  all_tables <- sapply(table_files, function(f) paste(readLines(f), collapse = "\n"))
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
  old_wd <- getwd()
  setwd(PREVIEW_DIR)
  on.exit(setwd(old_wd))
  result <- system2(local_tex_compiler,
                    args = c("-interaction=nonstopmode", "preview_tables.tex"),
                    stdout = TRUE, stderr = TRUE)
  if (!file.exists(tmp_pdf)) {
    cat(paste(result, collapse = "\n"), "\n")
    stop("Compilation failed — see pdflatex output above.")
  }
  cat("Preview saved to:", tmp_pdf, "\n")
  shell.exec(normalizePath(tmp_pdf))
  invisible(tmp_pdf)
}

all_tex_files <- list.files(tables_dir, pattern = "\\.tex$", full.names = TRUE)
preview_all_tables(all_tex_files)