rm(list = ls())

### Packages
library(fixest)
library(here)
library(dplyr)
library(hdm)          # Belloni, Chernozhukov, Hansen — double LASSO
library(customtextable)
library(parallel)
library(foreach)
library(doParallel)

## Suppress fixest notes/warnings (dropped obs, convergence messages, etc.)
setFixest_notes(FALSE)

## Parallel backend (Windows: PSOCK cluster)
N_CORES <- 16L
cl <- makeCluster(N_CORES)
registerDoParallel(cl)
on.exit(stopCluster(cl), add = TRUE)

### Load data
setwd(here())
clean_data_dir <- here("data", "processed")
tables_dir     <- here("output", "tables")
PREVIEW_DIR    <- here("writing", "preview")

df_iv <- read.csv(file.path(clean_data_dir, "df_iv.csv"))

## ── Control vectors ────────────────────────────────────────────────────────
geo_ctrls      <- c("lpop1911", "larea", "centre_alt", "max_alt")
military_ctrls <- c("veterans")
economic_ctrls <- c("ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy")
hist_ctrls     <- c("crime1874", "freecity", "lnpop1000")

all_ctrls <- c(geo_ctrls, economic_ctrls, military_ctrls, hist_ctrls, "psu1919_vv")

## ── LASSO candidate pool (broad set from df_iv columns) ───────────────────
## These are ALL potential controls the LASSO can draw from.
## all_ctrls above are always forced in; LASSO selects *additional* controls
## from this pool.
lasso_candidate_cols <- c(15,16,17,22,23,25,14,33,34,35,36,37,38,39,40,43,
                          59,60,75,85,95:107,113:125,136:143)
lasso_candidates <- names(df_iv)[lasso_candidate_cols]

## Remove any that are already in the always-included set (avoid duplication)
lasso_candidates <- setdiff(lasso_candidates, all_ctrls)

cat(sprintf("\nLASSO candidate pool: %d variables (on top of %d always-included controls)\n",
            length(lasso_candidates), length(all_ctrls)))
cat("  Always included: ", paste(all_ctrls, collapse = ", "), "\n")
cat("  LASSO candidates:", paste(lasso_candidates, collapse = ", "), "\n\n")

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

## ── IV inventory ───────────────────────────────────────────────────────────
iv_specs <- list(
  list(raw = "stat",  label = "Statutes"),
  list(raw = "Monte", label = "MdP")
)

endog_var <- "ass1900s_d"

## ════════════════════════════════════════════════════════════════════════════
## 1.  DOUBLE LASSO — CONTROL SELECTION
## ════════════════════════════════════════════════════════════════════════════
## Belloni, Chernozhukov, and Hansen (2014, REStud).
##
##   Step 1: LASSO of Y on X  →  S_Y
##   Step 2: LASSO of D on X  →  S_D
##   Union:  S = S_Y ∪ S_D
##
## Province FEs partialled out via demeaning.
## ════════════════════════════════════════════════════════════════════════════

demean_by_province <- function(data, vars) {
  out <- data
  for (v in vars) {
    out[[v]] <- out[[v]] - ave(out[[v]], out$province_fe, FUN = function(x) mean(x, na.rm = TRUE))
  }
  out
}

run_lasso_d <- function(endog_var, candidate_ctrls, data) {
  keep_vars <- c(endog_var, candidate_ctrls, "province_fe")
  d <- data[complete.cases(data[, keep_vars, drop = FALSE]), ]
  dm <- demean_by_province(d, c(endog_var, candidate_ctrls))
  X <- as.matrix(dm[, candidate_ctrls, drop = FALSE])
  D <- dm[[endog_var]]
  lasso_d <- rlasso(X, D)
  candidate_ctrls[which(coef(lasso_d)[-1] != 0)]
}

run_lasso_y <- function(outcome_var, candidate_ctrls, data) {
  keep_vars <- c(outcome_var, candidate_ctrls, "province_fe")
  d <- data[complete.cases(data[, keep_vars, drop = FALSE]), ]
  dm <- demean_by_province(d, c(outcome_var, candidate_ctrls))
  X <- as.matrix(dm[, candidate_ctrls, drop = FALSE])
  Y <- dm[[outcome_var]]
  lasso_y <- rlasso(X, Y)
  candidate_ctrls[which(coef(lasso_y)[-1] != 0)]
}

## ════════════════════════════════════════════════════════════════════════════
## 2.  MODEL RUNNERS
## ════════════════════════════════════════════════════════════════════════════

## --- 2a. Regular 2SLS ----------------------------------------------------
run_2sls <- function(outcome_var, iv_var, endog_var, ctrl_vars, data) {
  ctrl_str <- if (length(ctrl_vars) > 0) paste(ctrl_vars, collapse = " + ") else "1"
  fml <- as.formula(paste0(
    outcome_var, " ~ ", ctrl_str,
    " | province_fe",
    " | ", endog_var, " ~ ", iv_var
  ))
  suppressWarnings(feols(fml, data = data, cluster = ~provincia1921))
}

## --- 2b. Control Function -----------------------------------------------
##   First stage:  Pr(D=1 | X, Z) = Λ(Xγ + Zδ)        [logit]
##   Generalized residual:  v̂_i = D_i − p̂_i
##   Second stage (OLS):    Y_i = Xβ + D_i·α + v̂_i·ρ + ε_i
##
##   α  = structural coefficient on D
##   ρ  = CF coefficient (endogeneity test: H0: ρ = 0)
##   SEs: block bootstrap at province level (parallel, 16 cores)
##
##   Block bootstrap: resample provinces with replacement, keeping all
##   observations within each sampled province intact.
## ════════════════════════════════════════════════════════════════════════════

run_cf <- function(outcome_var, iv_var, endog_var, ctrl_vars, data,
                   n_boot = 500, seed = 42) {
  
  ctrl_str <- if (length(ctrl_vars) > 0) paste(ctrl_vars, collapse = " + ") else "1"
  
  # --- Point estimates (suppress warnings from feols/feglm) ---
  fml_logit <- as.formula(paste0(
    endog_var, " ~ ", ctrl_str, " + ", iv_var, " | province_fe"
  ))
  logit_mod <- suppressWarnings(feglm(fml_logit, data = data, family = binomial()))
  
  p_hat <- predict(logit_mod, newdata = data, type = "response")
  data$.vhat <- data[[endog_var]] - p_hat
  
  fml_ss <- as.formula(paste0(
    outcome_var, " ~ ", endog_var, " + .vhat + ", ctrl_str, " | province_fe"
  ))
  ss_mod <- suppressWarnings(feols(fml_ss, data = data, cluster = ~provincia1921))
  
  coef_D    <- as.numeric(coef(ss_mod)[endog_var])
  coef_vhat <- as.numeric(coef(ss_mod)[".vhat"])
  
  # --- Block bootstrap (parallel) ----------------------------------------
  # Pre-split data by province once (avoid repeated subsetting)
  clusters   <- unique(data$provincia1921)
  n_clust    <- length(clusters)
  clust_list <- split(data, data$provincia1921)
  
  # RNG seeds for reproducibility across parallel workers
  set.seed(seed)
  boot_seeds <- sample.int(.Machine$integer.max, n_boot)
  
  boot_results <- foreach(
    b          = seq_len(n_boot),
    .combine   = rbind,
    .packages  = c("fixest"),
    .export    = c("fml_logit", "fml_ss", "clust_list", "clusters",
                   "n_clust", "endog_var", "boot_seeds"),
    .errorhandling = "remove"
  ) %dopar% {
    
    setFixest_notes(FALSE)
    
    set.seed(boot_seeds[b])
    
    # Resample provinces with replacement (block bootstrap)
    boot_idx <- sample(clusters, n_clust, replace = TRUE)
    
    # Build bootstrap sample: all obs within each sampled province
    boot_data <- do.call(rbind, lapply(seq_along(boot_idx), function(j) {
      chunk <- clust_list[[ as.character(boot_idx[j]) ]]
      chunk$.boot_id <- j
      chunk
    }))
    
    tryCatch({
      logit_b <- suppressWarnings(
        feglm(fml_logit, data = boot_data, family = binomial())
      )
      p_hat_b <- predict(logit_b, newdata = boot_data, type = "response")
      boot_data$.vhat <- boot_data[[endog_var]] - p_hat_b
      
      ss_b <- suppressWarnings(
        feols(fml_ss, data = boot_data, cluster = ~.boot_id)
      )
      
      c(coef_D    = as.numeric(coef(ss_b)[endog_var]),
        coef_vhat = as.numeric(coef(ss_b)[".vhat"]))
    }, error = function(e) {
      c(coef_D = NA_real_, coef_vhat = NA_real_)
    })
  }
  
  # foreach with .combine = rbind returns a matrix
  boot_results <- as.data.frame(boot_results)
  
  se_D    <- sd(boot_results$coef_D, na.rm = TRUE)
  se_vhat <- sd(boot_results$coef_vhat, na.rm = TRUE)
  
  # First-stage logit coefficient on Z (for Panel A)
  fs_coef_z <- as.numeric(coef(logit_mod)[iv_var])
  fs_se_z   <- as.numeric(sqrt(diag(vcov(logit_mod)))[iv_var])
  
  list(
    coef_D     = coef_D,
    se_D       = se_D,
    coef_vhat  = coef_vhat,
    se_vhat    = se_vhat,
    fs_coef_z  = fs_coef_z,
    fs_se_z    = fs_se_z,
    nobs       = nobs(ss_mod)
  )
}

## ════════════════════════════════════════════════════════════════════════════
## 3.  EXTRACTORS (2SLS via fixest)
## ════════════════════════════════════════════════════════════════════════════

get_fs <- function(mod, fs_iv_name) {
  fs   <- mod$iv_first_stage[[1]]
  coef <- fs$coefficients[fs_iv_name]
  se   <- sqrt(diag(fs$cov.scaled))[fs_iv_name]
  list(coef = as.numeric(coef), se = as.numeric(se))
}

get_ss <- function(mod) {
  coef <- as.numeric(coef(mod)[paste0("fit_", endog_var)])
  se   <- as.numeric(se(mod)[paste0("fit_", endog_var)])
  list(coef = coef, se = se)
}

get_fstat <- function(mod) {
  tryCatch(fitstat(mod, "ivf")[[1]]$stat, error = function(e) NA_real_)
}

## ════════════════════════════════════════════════════════════════════════════
## 4.  TABLE BUILDERS
## ════════════════════════════════════════════════════════════════════════════
##
## Each table has 4 columns:
##   (1) Statutes / Full    (2) Statutes / Sel.
##   (3) Exposure / Full    (4) Exposure / Sel.
##
## Two .tex files per outcome:
##   <stem>_2sls.tex   Panel A: linear FS;   Panel B: LATE
##   <stem>_cf.tex     Panel A: logit FS;    Panel B: D coef + CF residual
## ════════════════════════════════════════════════════════════════════════════

build_col_specs <- function() {
  do.call(rbind, lapply(iv_specs, function(iv) {
    data.frame(
      raw    = iv$raw,
      label  = iv$label,
      sample = c("Full", "Sel."),
      stringsAsFactors = FALSE
    )
  }))
}

## ---------- 4a. 2SLS table ------------------------------------------------
make_2sls_table <- function(outcome_var, outcome_label, endog_var,
                            file_stem, lasso_ctrls) {
  
  col_specs <- build_col_specs()
  n_cols    <- nrow(col_specs)
  
  col_headers <- paste0(
    "(", seq_len(n_cols), ") ", col_specs$label, " / ", col_specs$sample
  )
  
  fs_cells   <- vector("list", n_cols)
  ss_cells   <- vector("list", n_cols)
  n_obs      <- numeric(n_cols)
  fstats     <- numeric(n_cols)
  mean_d_vec <- numeric(n_cols)
  mean_y_vec <- numeric(n_cols)
  
  for (i in seq_len(n_cols)) {
    samp_key <- col_specs$sample[i]
    d     <- if (samp_key == "Full") df_iv else df_iv_sel
    ctrls <- lasso_ctrls[[paste(outcome_var, samp_key, sep = "|")]]
    
    mod <- run_2sls(outcome_var, col_specs$raw[i], endog_var, ctrls, d)
    
    fs_v          <- get_fs(mod, col_specs$raw[i])
    fs_cells[[i]] <- est(fs_v$coef, fs_v$se)
    
    ss_v          <- get_ss(mod)
    ss_cells[[i]] <- est(ss_v$coef, ss_v$se)
    
    n_obs[i]      <- mod$nobs
    fstats[i]     <- get_fstat(mod)
    mean_d_vec[i] <- mean(d[[endog_var]], na.rm = TRUE)
    mean_y_vec[i] <- mean(d[[outcome_var]], na.rm = TRUE)
  }
  
  stat_cells <- c(
    as.list(round(n_obs, 0)),
    as.list(round(fstats, 2)),
    as.list(round(mean_d_vec, 3)),
    as.list(round(mean_y_vec, 3))
  )
  
  out_file <- file.path(tables_dir, paste0(file_stem, "_2sls.tex"))
  
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
    caption    = paste0("2SLS — ", outcome_label),
    stats      = c("Observations", "First-Stage F-stat", "Mean(D)", "Mean(Outcome)"),
    stat_cells = stat_cells,
    note       = paste0(
      "Notes: Standard two-stage least squares with linear first stage. ",
      "Clustered standard errors at the province level in parentheses. ",
      "Controls selected via double LASSO (Belloni, Chernozhukov \\& Hansen, 2014). ",
      "All specifications include province fixed effects. ",
      "Selected sample: municipalities with lpop1911 $>$ 8.5 ($\\approx$ pop.~5\\,000). ",
      "* $p<0.1$; ** $p<0.05$; *** $p<0.01$."
    )
  )
  
  cat("  → written:", out_file, "\n")
}

## ---------- 4b. CF table --------------------------------------------------
make_cf_table <- function(outcome_var, outcome_label, endog_var,
                          file_stem, lasso_ctrls) {
  
  col_specs <- build_col_specs()
  n_cols    <- nrow(col_specs)
  
  col_headers <- paste0(
    "(", seq_len(n_cols), ") ", col_specs$label, " / ", col_specs$sample
  )
  
  fs_cells   <- vector("list", n_cols)
  ss_cells   <- vector("list", n_cols)
  cf_cells   <- vector("list", n_cols)
  n_obs      <- numeric(n_cols)
  mean_d_vec <- numeric(n_cols)
  mean_y_vec <- numeric(n_cols)
  
  for (i in seq_len(n_cols)) {
    samp_key <- col_specs$sample[i]
    d     <- if (samp_key == "Full") df_iv else df_iv_sel
    ctrls <- lasso_ctrls[[paste(outcome_var, samp_key, sep = "|")]]
    
    cf_res <- run_cf(outcome_var, col_specs$raw[i], endog_var, ctrls, d)
    
    fs_cells[[i]] <- est(cf_res$fs_coef_z, cf_res$fs_se_z)
    ss_cells[[i]] <- est(cf_res$coef_D, cf_res$se_D)
    cf_cells[[i]] <- est(cf_res$coef_vhat, cf_res$se_vhat)
    
    n_obs[i]      <- cf_res$nobs
    mean_d_vec[i] <- mean(d[[endog_var]], na.rm = TRUE)
    mean_y_vec[i] <- mean(d[[outcome_var]], na.rm = TRUE)
  }
  
  stat_cells <- c(
    as.list(round(n_obs, 0)),
    as.list(round(mean_d_vec, 3)),
    as.list(round(mean_y_vec, 3))
  )
  
  out_file <- file.path(tables_dir, paste0(file_stem, "_cf.tex"))
  
  create_tex_table(
    filename   = out_file,
    cols       = col_headers,
    panels     = list(
      list(
        name  = "Panel A: First Stage (Logit)",
        rows  = c("Instrument"),
        cells = fs_cells
      ),
      list(
        name  = "Panel B: Second Stage",
        rows  = c("Mutual Aid Society", "CF Residual ($\\hat{v}$)"),
        cells = c(ss_cells, cf_cells)
      )
    ),
    caption    = paste0("Control Function — ", outcome_label),
    stats      = c("Observations", "Mean(D)", "Mean(Outcome)"),
    stat_cells = stat_cells,
    note       = paste0(
      "Notes: Control function estimates with logit first stage. ",
      "Panel~A reports logit coefficients on the instrument. ",
      "The generalized residual $\\hat{v}_i = D_i - \\hat{p}_i$ enters the ",
      "second-stage OLS directly; its coefficient ($\\hat{\\rho}$) provides a ",
      "test for endogeneity ($H_0$: $\\rho = 0$). ",
      "Standard errors via block bootstrap (500 replications) at the province level. ",
      "Controls selected via double LASSO (Belloni, Chernozhukov \\& Hansen, 2014). ",
      "All specifications include province fixed effects. ",
      "Selected sample: municipalities with lpop1911 $>$ 8.5 ($\\approx$ pop.~5\\,000). ",
      "* $p<0.1$; ** $p<0.05$; *** $p<0.01$."
    )
  )
  
  cat("  → written:", out_file, "\n")
}

## ════════════════════════════════════════════════════════════════════════════
## 5.  MAIN LOOP
## ════════════════════════════════════════════════════════════════════════════

## --- D-equation LASSO: run once (same D across all outcomes) -------------
sel_d <- list()
for (samp in c("Full", "Sel.")) {
  d <- if (samp == "Full") df_iv else df_iv_sel
  sel_d[[samp]] <- run_lasso_d(endog_var, lasso_candidates, d)
  cat(sprintf("\nD-equation LASSO [%s]: ", samp))
  cat(if (length(sel_d[[samp]]) > 0) paste(sel_d[[samp]], collapse = ", ") else "(none)", "\n")
}

for (o in outcomes) {
  
  cat("\n══════════════════════════════════════════════════════════════\n")
  cat("  Outcome:", o$label, "(", o$var, ")\n")
  cat("══════════════════════════════════════════════════════════════\n")
  
  # --- Y-equation LASSO (per outcome × sample) ---------------------------
  # Final controls = all_ctrls (forced) ∪ sel_Y ∪ sel_D
  lasso_ctrls <- list()
  
  for (samp in c("Full", "Sel.")) {
    d   <- if (samp == "Full") df_iv else df_iv_sel
    key <- paste(o$var, samp, sep = "|")
    
    sel_y <- run_lasso_y(o$var, lasso_candidates, d)
    lasso_selected <- union(sel_y, sel_d[[samp]])
    lasso_ctrls[[key]] <- union(all_ctrls, lasso_selected)
    
    cat(sprintf("\n  Double LASSO [%s / %s]:\n", o$var, samp))
    cat("    Y-equation selected: ",
        if (length(sel_y) > 0) paste(sel_y, collapse = ", ") else "(none)", "\n")
    cat("    D-equation selected: ",
        if (length(sel_d[[samp]]) > 0) paste(sel_d[[samp]], collapse = ", ") else "(pre-computed)", "\n")
    cat("    LASSO union (", length(lasso_selected), " vars): ",
        if (length(lasso_selected) > 0) paste(lasso_selected, collapse = ", ") else "(none)", "\n")
    cat("    Total controls:  ", length(lasso_ctrls[[key]]),
        " (", length(all_ctrls), " forced + ", length(lasso_selected), " LASSO-selected)\n")
  }
  
  # --- Tables ------------------------------------------------------------
  cat("\n  [2SLS table]\n")
  make_2sls_table(o$var, o$label, endog_var, o$file, lasso_ctrls)
  
  cat("  [CF table]\n")
  make_cf_table(o$var, o$label, endog_var, o$file, lasso_ctrls)
}

## Stop parallel cluster
stopCluster(cl)

## ════════════════════════════════════════════════════════════════════════════
## 6.  PREVIEW
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