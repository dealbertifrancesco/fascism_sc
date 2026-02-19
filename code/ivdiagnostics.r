rm(list = ls())

### Packages
library(lfe)
library(fixest)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(localIV)

### Load data
setwd(here())
clean_data_dir <- here("data", "processed")
tables_dir <- here("output", "tables")
gr_dir <- here("output", "figures")
PREVIEW_DIR <- here("writing", "preview")

df_iv <- read.csv(file.path(clean_data_dir, "df_iv.csv"))

## Define controls ####
geo_ctrls = c("lpop1911", "larea", "centre_alt", "max_alt") #
military_ctrls <- c("veterans")#, "veterans96_00", "ard_vol_pop6m", "cruent_d", "army_suppliers_d")
economic_ctrls <- c("ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy") #"shcrop" "ind_firms", , "elites", )

rep_experience2 = c("tcomrep", "tmarrep")
rep_experience4 = c("tcomrep", "tmarrep", "tcomrep_a", "tmarrep_s", "tmarrep_a")
allcontrols =  c("costal", "nearsea", "sea_distance100", "dist_river100", 
                 "alt_com100", "rug_med100", "CaloricSuitpre15nr100", 
                 "lnpop1000", "dist_romanrd100", "dist_bishop1000Km_GSZ100", 
                 "freecity", "comune_lega_lombarda_allnord")

## OUTCOME VARIABLE
o_var <- "dlf_1926"

## Helper functions
## Propscore IV
run_propensity_iv <- function(outcome_var, iv_var, data, cluster_var = ~provincia1921) {
  
  fml_logit <- as.formula(paste0(
    "ass1900s_d ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv + ", 
    iv_var, " | province_fe"
  ))
  
  # Run the Logit
  fs_logit <- feglm(fml_logit, data = data, family = binomial())
  pz_col_name <- paste0("pz_", iv_var)
  data[[pz_col_name]] <- predict(fs_logit, newdata = data, type = "response")
  
  # 3. Second Stage: IV (Outcome ~ Controls | Endogenous ~ Propensity_Score)
  fml_iv <- as.formula(paste0(
    outcome_var, " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv | province_fe | ass1900s_d ~ ", pz_col_name
  ))
  
  # Run the FEOLS
  if (is.null(cluster_var)) {
    res <- feols(fml_iv, data = data)
  } else {
    res <- feols(fml_iv, data = data, cluster = cluster_var)
  }
  
  return(res)
}

# Export grouped regressions
export_iv_table <- function(model1, model2, title, file_name) {
  etable(model1, model2, stage=1:2, fitstat=~n+ivf, coefstat="tstat",
         title = title, tex = TRUE, file = file_name,
         replace = TRUE)
}

get_complier_characteristics <- function(iv_model, char_vars, iv_var, data,
                                         endog_var = "ass1900s_d",
                                         fe_vars   = "province_fe",
                                         cluster   = ~provincia1921) {
  
  # Unwrap model if stored as a list (fixest sometimes wraps the object)
  if (is.list(iv_model) && !inherits(iv_model, "fixest")) iv_model <- iv_model[[1]]
  
  # ── 0. Setup ──────────────────────────────────────────────────────────────
  fe_formula <- paste(fe_vars, collapse = " + ")
  coef_name  <- paste0("fit_", endog_var)
  
  message("Endogenous var : ", endog_var)
  message("Fixed effects  : ", fe_formula)
  
  # ── 1. Get the exact sample used in the model ─────────────────────────────
  if (!is.null(iv_model$obs_selection) && length(iv_model$obs_selection) > 0) {
    data_sample <- data[unlist(iv_model$obs_selection), ]
  } else {
    data_sample <- data
  }
  message("Sample size    : ", nrow(data_sample))
  
  # ── 2. Re-generate the Propensity Score internally ────────────────────────
  fml_logit <- as.formula(paste0(
    endog_var, " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv + ",
    iv_var, " | ", fe_formula
  ))
  
  fs_logit                <- feglm(fml_logit, data = data_sample, family = binomial())
  data_sample$pz_internal <- predict(fs_logit, type = "response")
  
  # ── 3. Loop over characteristic variables ─────────────────────────────────
  # Abadie (2003): regress X*D on controls, using Pz as IV for D.
  # The coefficient on fit_D gives E[X | complier].
  ctrl_part <- ".[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv"
  iv_part   <- paste0(" | ", endog_var, " ~ pz_internal")
  
  results_list <- list()
  
  for (var in char_vars) {
    
    message("Processing: ", var)
    
    if (!var %in% names(data_sample) || all(is.na(data_sample[[var]]))) {
      warning(paste("Variable", var, "not found or all-NA in sample. Skipping."))
      next
    }
    
    data_sample$temp_XD <- data_sample[[var]] * data_sample[[endog_var]]
    
    fml_t <- as.formula(paste0("temp_XD ~ ", ctrl_part, " | ", fe_formula, iv_part))
    
    mod_t <- tryCatch(
      feols(fml_t, data = data_sample, cluster = cluster),
      error = function(e) { warning(paste("Error for", var, ":", e$message)); NULL }
    )
    
    if (is.null(mod_t)) next
    
    if (!coef_name %in% names(coef(mod_t))) {
      warning(paste("Coefficient", coef_name, "not found for", var,
                    "— available:", paste(names(coef(mod_t)), collapse = ", ")))
      next
    }
    
    results_list[[var]] <- data.frame(
      Variable         = var,
      Complier_Mean    = round(as.numeric(coef(mod_t)[coef_name]), 4),
      Full_Sample_Mean = round(mean(data_sample[[var]], na.rm = TRUE), 4)
    )
  }
  
  if (length(results_list) == 0) {
    warning("No variables were successfully processed.")
    return(data.frame())
  }
  
  return(do.call(rbind, results_list))
}

plot_exclusion_restriction <- function(iv_model, data,
                                       y_var,     
                                       n_groups  = 10,
                                       seed      = 123,
                                       iv_var    = "stat",
                                       d_var     = "ass1900s_d",
                                       fe_vars   = "province_fe",
                                       cluster   = ~provincia1921) {
  
  # Unwrap model if stored as a list (fixest sometimes wraps the object)
  if (is.list(iv_model) && !inherits(iv_model, "fixest")) iv_model <- iv_model[[1]]
  
  # ── 0. Setup ──────────────────────────────────────────────────────────────
  fe_formula <- paste(fe_vars, collapse = " + ")
  ctrl_part  <- ".[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv"
  
  message("Outcome var    : ", y_var)
  message("Endogenous var : ", d_var)
  message("Original IV    : ", iv_var)
  message("Fixed effects  : ", fe_formula)
  
  # ── 1. Get the exact sample used in the model ─────────────────────────────
  if (!is.null(iv_model$obs_selection) && length(iv_model$obs_selection) > 0) {
    data_sample <- data[unlist(iv_model$obs_selection), ]
  } else {
    data_sample <- data
  }
  message("Sample size    : ", nrow(data_sample))
  
  # ── 2. Regenerate propensity score ────────────────────────────────────────
  fml_logit <- as.formula(paste0(
    d_var, " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv + ",
    iv_var, " | ", fe_formula
  ))
  fs_logit                <- feglm(fml_logit, data = data_sample, family = binomial())
  data_sample$pz_internal <- predict(fs_logit, type = "response")
  z_var                   <- "pz_internal"
  
  # ── 3. Assign random groups ───────────────────────────────────────────────
  set.seed(seed)
  data_sample$random_group <- sample(1:n_groups, nrow(data_sample), replace = TRUE)
  
  plot_data <- data.frame(
    Group   = 1:n_groups,
    FS_Coef = NA_real_, RF_Coef = NA_real_,
    FS_SE   = NA_real_, RF_SE   = NA_real_
  )
  
  # ── 4. Loop over groups ───────────────────────────────────────────────────
  for (i in 1:n_groups) {
    sub_data <- data_sample[data_sample$random_group == i, ]
    
    if (nrow(sub_data) < 10) {
      warning(paste("Group", i, "has fewer than 10 obs — skipping."))
      next
    }
    
    fml_fs <- as.formula(paste0(d_var, " ~ ", z_var, " + ", ctrl_part, " | ", fe_formula))
    fml_rf <- as.formula(paste0(y_var, " ~ ", z_var, " + ", ctrl_part, " | ", fe_formula))
    
    m_fs <- tryCatch(
      feols(fml_fs, data = sub_data, cluster = cluster, warn = FALSE, notes = FALSE),
      error = function(e) { warning(paste("FS error in group", i, ":", e$message)); NULL }
    )
    m_rf <- tryCatch(
      feols(fml_rf, data = sub_data, cluster = cluster, warn = FALSE, notes = FALSE),
      error = function(e) { warning(paste("RF error in group", i, ":", e$message)); NULL }
    )
    
    if (is.null(m_fs) || is.null(m_rf)) next
    
    if (z_var %in% names(coef(m_fs))) {
      plot_data$FS_Coef[i] <- coef(m_fs)[z_var]
      plot_data$FS_SE[i]   <- se(m_fs)[z_var]
    }
    if (z_var %in% names(coef(m_rf))) {
      plot_data$RF_Coef[i] <- coef(m_rf)[z_var]
      plot_data$RF_SE[i]   <- se(m_rf)[z_var]
    }
  }
  
  # Drop failed groups
  plot_data <- plot_data[!is.na(plot_data$FS_Coef) & !is.na(plot_data$RF_Coef), ]
  if (nrow(plot_data) == 0) stop("No groups estimated successfully. Check your data and model.")
  
  # ── 5. Overall IV slope for reference line ────────────────────────────────
  coef_name     <- paste0("fit_", d_var)
  overall_slope <- tryCatch({
    s <- coef(iv_model)[coef_name]
    if (length(s) == 0) NA_real_ else as.numeric(s)
  }, error = function(e) NA_real_)
  if (is.na(overall_slope)) warning("Could not extract overall IV slope — reference line omitted.")
  
  # ── 6. Plot ───────────────────────────────────────────────────────────────
  p <- ggplot(plot_data, aes(x = FS_Coef, y = RF_Coef, label = Group)) +
    geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.5) +
    geom_errorbar(
      aes(ymin = RF_Coef - 1.96 * RF_SE, ymax = RF_Coef + 1.96 * RF_SE),
      width = 0, alpha = 0.3
    ) +
    geom_errorbarh(
      aes(xmin = FS_Coef - 1.96 * FS_SE, xmax = FS_Coef + 1.96 * FS_SE),
      height = 0, alpha = 0.3
    ) +
    geom_point(size = 3, color = "steelblue") +
    geom_text(vjust = -1.5, size = 3) +
    theme_minimal() +
    labs(
      title    = "Visual IV Test: Reduced Form vs First Stage",
      subtitle = paste0("Dashed line = overall IV estimate (β = ",
                        ifelse(!is.na(overall_slope), round(overall_slope, 3), "N/A"), ")"),
      x = paste0("First Stage: Propensity Score → ", d_var),
      y = paste0("Reduced Form: Propensity Score → ", y_var)
    )
  
  if (!is.na(overall_slope)) {
    p <- p + geom_abline(intercept = 0, slope = overall_slope,
                         color = "red", linetype = "dashed")
  }
  
  return(p)
}

# ----------------------------------------------------------
## ============================================================
## MTE Weights & Estimation — Heckman & Vytlacil (2005)
## ============================================================
## Usage:
##   iv_1    <- run_propensity_iv("fascist_branch", "stat", df_iv)
##   w_stat  <- compute_mte_weights("fascist_branch", "stat", df_iv)
##   mte_df  <- estimate_mte("fascist_branch", "stat", df_iv)
##   plot_mte_weights(w_stat, mte_df)
## ============================================================

# ----------------------------------------------------------
# INTERNAL HELPER: re-runs the same logit as run_propensity_iv
# ----------------------------------------------------------
.get_propensity <- function(iv_var, data) {
  fml_logit <- as.formula(paste0(
    "ass1900s_d ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv + ",
    iv_var, " | province_fe"
  ))
  fs_logit <- feglm(fml_logit, data = data, family = binomial())
  predict(fs_logit, newdata = data, type = "response")
}


# ----------------------------------------------------------
# compute_mte_weights()
# ----------------------------------------------------------
compute_mte_weights <- function(outcome_var,
                                iv_var,
                                data,
                                u_grid = seq(0.01, 0.99, by = 0.005)) {
  
  p_raw    <- .get_propensity(iv_var, data)
  p_scores <- p_raw[!is.na(p_raw)]
  
  Ep   <- mean(p_scores)
  E1mp <- mean(1 - p_scores)
  Varp <- var(p_scores)
  
  weights_df <- data.frame(u_D = u_grid) |>
    rowwise() |>
    mutate(
      h_ATE = 1,
      h_TT  = mean(p_scores > u_D) / Ep,
      h_TUT = mean(p_scores < u_D) / E1mp,
      h_IV  = mean((p_scores - Ep) * (p_scores > u_D)) / Varp
    ) |>
    ungroup()
  
  list(
    weights  = weights_df,
    p_scores = p_scores,
    moments  = c(Ep = Ep, E1mp = E1mp, Varp = Varp),
    labels   = list(outcome = outcome_var, iv_var = iv_var)
  )
}


# ----------------------------------------------------------
# estimate_mte()
#
# Returns a data.frame with $u_D, $MTE, $CI_low, $CI_high
# using the delta method on the polynomial derivative.
#
# The derivative of K(p) = b1*p + ... + bK*p^K is:
#   dK/dp = b1 + 2*b2*p + ... + K*bK*p^(K-1)
#
# In matrix form at a given u: g(u)' = [1, 2u, 3u^2, ..., K*u^(K-1)]
# so Var(MTE(u)) = g(u)' * Sigma_bb * g(u)
# where Sigma_bb is the sub-matrix of vcov for the poly coefficients
# ----------------------------------------------------------
estimate_mte <- function(outcome_var,
                         iv_var,
                         data,
                         degree    = 4,
                         u_grid    = seq(0.01, 0.99, by = 0.005),
                         conf      = 0.95,
                         cluster   = ~provincia1921) {
  
  # ── 1. Get P(Z) and polynomial columns ───────────────────
  data$pz   <- .get_propensity(iv_var, data)
  poly_cols <- paste0("pz_pow", seq_len(degree))
  for (k in seq_len(degree)) data[[poly_cols[k]]] <- data$pz^k
  
  # ── 2. Partially linear regression ───────────────────────
  fml_ols <- as.formula(paste0(
    outcome_var,
    " ~ .[geo_ctrls] + .[economic_ctrls] + .[military_ctrls] + psu1919_vv + ",
    paste(poly_cols, collapse = " + "),
    " | province_fe"
  ))
  
  fit_ols <- feols(fml_ols, data = data, cluster = cluster)
  
  # ── 3. Extract coefficients and vcov for poly terms ──────
  coefs   <- coef(fit_ols)
  b       <- coefs[poly_cols]
  Sigma   <- vcov(fit_ols)[poly_cols, poly_cols]
  z_crit  <- qnorm(1 - (1 - conf) / 2)
  
  # ── 4. MTE and delta-method SE at each u_D ───────────────
  #
  #  MTE(u)    = sum_k [ k * b_k * u^(k-1) ]
  #  grad g(u) = [ 1, 2u, 3u^2, ..., K*u^(K-1) ]  (length = degree)
  #  SE(u)     = sqrt( g(u)' Sigma g(u) )
  #
  results <- lapply(u_grid, function(u) {
    g   <- seq_len(degree) * u ^ (seq_len(degree) - 1)   # gradient
    mte <- sum(g * b)
    se  <- sqrt(as.numeric(t(g) %*% Sigma %*% g))
    data.frame(u_D     = u,
               MTE     = mte,
               CI_low  = mte - z_crit * se,
               CI_high = mte + z_crit * se)
  })
  
  do.call(rbind, results)
}


# ----------------------------------------------------------
# plot_mte_weights()
# ----------------------------------------------------------
plot_mte_weights <- function(wt_obj, mte_df = NULL) {
  
  pal <- c(
    "IV (P as instrument)" = "#ff7f00"
  )
  
  neg_share <- mean(wt_obj$weights$h_IV < 0)
  ann_label <- sprintf("IV weight < 0 for %.0f%% of u_D", 100 * neg_share)
  
  long_df <- wt_obj$weights |>
    pivot_longer(cols = c(h_IV),
                 names_to  = "estimand",
                 values_to = "weight") |>
    mutate(estimand = recode(estimand,
                             h_IV  = "IV (P as instrument)"))
  
  p <- ggplot(long_df, aes(x = u_D, y = weight,
                           colour = estimand, linetype = estimand)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_line(linewidth = 0.95) +
    annotate("text", x = 0.62, y = max(long_df$weight) * 0.93,
             label = ann_label, size = 3.2, colour = "#ff7f00", hjust = 0) +
    scale_colour_manual(values = pal) +
    scale_linetype_manual(values = c(
      "IV (P as instrument)" = "longdash"
    )) +
    labs(
      title    = sprintf("MTE Weights — outcome: %s, instrument: %s",
                         wt_obj$labels$outcome, wt_obj$labels$iv_var),
      x        = expression(u[D] ~ "(resistance to treatment)"),
      y        = "Weight  h(u_D)",
      colour   = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.caption     = element_text(size = 8, colour = "grey50")
    )
  
  if (!is.null(mte_df)) {
    w_range      <- range(long_df$weight)
    mte_range    <- range(mte_df$CI_low, mte_df$CI_high)  # include CI in scaling
    scale_factor <- diff(w_range) / diff(mte_range)
    shift        <- w_range[1] - mte_range[1] * scale_factor
    
    mte_df <- mte_df |>
      mutate(
        mte_scaled    = MTE     * scale_factor + shift,
        ci_low_scaled = CI_low  * scale_factor + shift,
        ci_hi_scaled  = CI_high * scale_factor + shift
      )
    
    p <- p +
      # Confidence band
      geom_ribbon(data = mte_df,
                  aes(x = u_D, ymin = ci_low_scaled, ymax = ci_hi_scaled),
                  fill = "grey30", alpha = 0.15,
                  inherit.aes = FALSE) +
      # MTE line
      geom_line(data = mte_df,
                aes(x = u_D, y = mte_scaled),
                colour = "black", linewidth = 1.1,
                linetype = "solid", inherit.aes = FALSE) +
      scale_y_continuous(
        sec.axis = sec_axis(
          transform = ~ (. - shift) / scale_factor,
          name      = "MTE  (right axis)"
        )
      ) +
      annotate("text", x = 0.02, y = max(mte_df$mte_scaled),
               label = "MTE", colour = "black", size = 3.5,
               fontface = "bold", hjust = -12)
  }
  
  print(p)
  invisible(p)
}

plot_mte_comparison <- function(wt_obj, mte_poly_df, mte_liv_df, title_iv) {
  p <- plot_mte_weights(wt_obj, mte_poly_df)
  
  w_range   <- range(wt_obj$weights[, c("h_IV")])
  mte_range <- range(c(mte_poly_df$CI_low, mte_poly_df$CI_high,
                       mte_liv_df$value), na.rm = TRUE)
  sf    <- diff(w_range) / diff(mte_range)
  shift <- w_range[1] - mte_range[1] * sf
  
  mte_liv_df$scaled <- mte_liv_df$value * sf + shift
  
  p +
    geom_line(data = mte_liv_df,
              aes(x = u_D, y = scaled),
              colour = "steelblue", linewidth = 1,
              linetype = "dashed", inherit.aes = FALSE) +
    annotate("text", x = 0.55, y = max(mte_liv_df$scaled, na.rm = TRUE),
             label = "MTE (localIV)", colour = "steelblue",
             size = 3, hjust = 0)
}


## ── Figure save + preview helpers ─────────────────────────────────────────

save_figure <- function(plot, filename, width = 8, height = 5) {
  path <- file.path(gr_dir, filename)
  ggsave(path, plot = plot, device = "pdf", width = width, height = height)
  cat("  → saved:", path, "\n")
  invisible(path)
}

preview_figures <- function(pdf_paths, out_name = "preview_figures.pdf") {
  includes <- paste(
    sapply(normalizePath(pdf_paths, winslash = "/"), function(f) {
      paste0(
        "\\begin{figure}[htbp]\n",
        "  \\centering\n",
        "  \\includegraphics[width=0.95\\textwidth]{", f, "}\n",
        "\\end{figure}\n"
      )
    }),
    collapse = "\n\\clearpage\n"
  )
  
  full_doc <- paste0(
    "\\documentclass{article}\n",
    "\\usepackage{graphicx}\n",
    "\\usepackage{geometry}\n",
    "\\geometry{margin=0.5in}\n",
    "\\begin{document}\n",
    includes,
    "\n\\end{document}"
  )
  
  tmp_tex <- file.path(PREVIEW_DIR, "preview_figures.tex")
  tmp_pdf <- file.path(PREVIEW_DIR, "preview_figures.pdf")
  writeLines(full_doc, tmp_tex)
  
  pdflatex <- local_tex_compiler
  old_wd <- getwd()
  setwd(PREVIEW_DIR)
  on.exit(setwd(old_wd))
  
  result <- system2(pdflatex,
                    args   = c("-interaction=nonstopmode", "preview_figures.tex"),
                    stdout = TRUE, stderr = TRUE)
  
  if (!file.exists(tmp_pdf)) {
    cat(paste(result, collapse = "\n"), "\n")
    stop("Compilation failed — see pdflatex output above.")
  }
  
  cat("Preview saved to:", tmp_pdf, "\n")
  shell.exec(normalizePath(tmp_pdf))
  invisible(tmp_pdf)
}

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

## ── Run regressions ────────────────────────────────────────────────────────
iv_1  <- run_propensity_iv(o_var, "stat",  df_iv)
iv_2  <- run_propensity_iv(o_var, "Monte", df_iv)

## ── MTE weights & estimates ────────────────────────────────────────────────
w_s      <- compute_mte_weights(o_var, "stat",        df_iv)
w_m      <- compute_mte_weights(o_var, "Monte",       df_iv)
w_sm     <- compute_mte_weights(o_var, "stat + Monte",df_iv)
mte_df_s  <- estimate_mte(o_var, "stat",        df_iv)
mte_df_m  <- estimate_mte(o_var, "Monte",       df_iv)
mte_df_sm <- estimate_mte(o_var, "stat + Monte",df_iv)

## ── localIV setup (unchanged from your code) ──────────────────────────────
prov_dummies <- model.matrix(~ factor(province_fe) - 1, data = df_iv)
prov_dummies <- prov_dummies[, -1]
colnames(prov_dummies) <- paste0("prov_", seq_len(ncol(prov_dummies)))

df_liv <- cbind(
  df_iv[, c(o_var, "ass1900s_d", "psu1919_vv",
            "lpop1911", "larea", "centre_alt", "max_alt", "veterans",
            "ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy",
            "stat", "Monte")],
  prov_dummies
)
df_liv    <- na.omit(df_liv)
prov_cols <- colnames(prov_dummies)
ctrl_rhs  <- paste(c(geo_ctrls, economic_ctrls, military_ctrls, "psu1919_vv", prov_cols),
                   collapse = " + ")

sel_stat      <- as.formula(paste("ass1900s_d ~", ctrl_rhs, "+ stat"))
sel_monte     <- as.formula(paste("ass1900s_d ~", ctrl_rhs, "+ Monte"))
sel_statmonte <- as.formula(paste("ass1900s_d ~", ctrl_rhs, "+ stat + Monte"))
out_fml       <- as.formula(paste(o_var, "~", ctrl_rhs))

liv_stat  <- mte(selection = sel_stat,      outcome = out_fml, data = df_liv,
                 method = "localIV", bw = 0.25)
liv_monte <- mte(selection = sel_monte,     outcome = out_fml, data = df_liv,
                 method = "localIV", bw = 0.25)
liv_sm    <- mte(selection = sel_statmonte, outcome = out_fml, data = df_liv,
                 method = "localIV", bw = 0.25)

u_grid        <- seq(0.02, 0.98, by = 0.01)
mte_liv_stat  <- mte_at(u = u_grid, model = liv_stat)
mte_liv_monte <- mte_at(u = u_grid, model = liv_monte)
mte_liv_sm    <- mte_at(u = u_grid, model = liv_sm)

mte_liv_stat$u_D   <- mte_liv_stat$u;  mte_liv_stat$instrument  <- "stat"
mte_liv_monte$u_D  <- mte_liv_monte$u; mte_liv_monte$instrument <- "Monte"
mte_liv_sm$u_D     <- mte_liv_sm$u;    mte_liv_sm$instrument    <- "stat + Monte"

## ── Produce & save all figures ────────────────────────────────────────────
saved_figs <- c()

# Exclusion restriction plots
p_excl_s <- plot_exclusion_restriction(iv_1, df_iv, y_var = o_var,
                                       n_groups = 8, iv_var = "stat")
p_excl_m <- plot_exclusion_restriction(iv_2, df_iv, y_var = o_var,
                                       n_groups = 8, iv_var = "Monte")
saved_figs <- c(saved_figs,
                save_figure(p_excl_s, "excl_restriction_stat.pdf"),
                save_figure(p_excl_m, "excl_restriction_monte.pdf")
)

# MTE weight plots (polynomial only)
p_mte_s  <- plot_mte_weights(w_s,  mte_df_s)
p_mte_m  <- plot_mte_weights(w_m,  mte_df_m)
p_mte_sm <- plot_mte_weights(w_sm, mte_df_sm)
saved_figs <- c(saved_figs,
                save_figure(p_mte_s,  "mte_weights_stat.pdf"),
                save_figure(p_mte_m,  "mte_weights_monte.pdf"),
                save_figure(p_mte_sm, "mte_weights_stat_monte.pdf")
)

# MTE comparison plots (poly vs localIV)
p_comp_s  <- plot_mte_comparison(w_s,  mte_df_s,  mte_liv_stat,  "stat")
p_comp_m  <- plot_mte_comparison(w_m,  mte_df_m,  mte_liv_monte, "Monte")
p_comp_sm <- plot_mte_comparison(w_sm, mte_df_sm, mte_liv_sm,    "stat + Monte")
saved_figs <- c(saved_figs,
                save_figure(p_comp_s,  "mte_comparison_stat.pdf"),
                save_figure(p_comp_m,  "mte_comparison_monte.pdf"),
                save_figure(p_comp_sm, "mte_comparison_stat_monte.pdf")
)

# Faceted poly vs localIV overlay
mte_df_s$instrument  <- "stat"
mte_df_m$instrument  <- "Monte"
mte_df_sm$instrument <- "stat + Monte"

poly_all <- rbind(
  mte_df_s[,  c("u_D","MTE","CI_low","CI_high","instrument")],
  mte_df_m[,  c("u_D","MTE","CI_low","CI_high","instrument")],
  mte_df_sm[, c("u_D","MTE","CI_low","CI_high","instrument")]
)
liv_all <- rbind(
  mte_liv_stat[,  c("u_D","value","instrument")],
  mte_liv_monte[, c("u_D","value","instrument")],
  mte_liv_sm[,    c("u_D","value","instrument")]
)
names(liv_all)[2] <- "MTE"

poly_all$instrument <- factor(poly_all$instrument, levels = c("stat","Monte","stat + Monte"))
liv_all$instrument  <- factor(liv_all$instrument,  levels = c("stat","Monte","stat + Monte"))

p_facet <- ggplot() +
  geom_ribbon(data = poly_all,
              aes(x = u_D, ymin = CI_low, ymax = CI_high),
              fill = "#e31a1c", alpha = 0.12) +
  geom_line(data = poly_all,
            aes(x = u_D, y = MTE),
            colour = "#e31a1c", linewidth = 0.9) +
  geom_line(data = liv_all,
            aes(x = u_D, y = MTE),
            colour = "#1f78b4", linewidth = 0.9, linetype = "dashed") +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.4) +
  facet_wrap(~ instrument, ncol = 3) +
  labs(
    title    = "MTE estimates — polynomial vs localIV",
    subtitle = "Red: polynomial (degree 4) with 95% CI  |  Blue dashed: localIV (bw = 0.25)",
    x        = expression(u[D] ~ "(resistance to treatment)"),
    y        = "MTE"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

saved_figs <- c(saved_figs,
                save_figure(p_facet, "mte_facet_poly_vs_localIV.pdf", width = 12, height = 5)
)

######################## NOTE: REQUIRES LOCAL LATEX COMPILER OR TINITEX #############
local_tex_compiler <- "C:/Users/dealb/AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"
### PREVIEW FILE
## ── Preview all ────────────────────────────────────────────────────────────
preview_figures(saved_figs)