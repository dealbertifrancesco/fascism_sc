rm(list = ls())

### Packages
library(fixest)
library(here)
library(dplyr)
library(ggplot2)

### Load data
setwd(here())
clean_data_dir <- here("data", "processed")
gr_dir         <- here("output", "figures")
PREVIEW_DIR    <- here("writing", "preview")

local_tex_compiler <- "C:/Users/dealb/AppData/Local/Programs/MiKTeX/miktex/bin/x64/pdflatex.exe"

## Suppress fixest notes
setFixest_notes(FALSE)

df_iv <- read.csv(file.path(clean_data_dir, "df_iv.csv"))

## ── Controls ───────────────────────────────────────────────────────────────
geo_ctrls      <- c("lpop1911", "larea", "centre_alt", "max_alt")
military_ctrls <- c("veterans")
economic_ctrls <- c("ind_workers", "dlab", "bourgeoisie", "landlord_ass", "literacy")
hist_ctrls     <- c("crime1874", "freecity", "lnpop1000")

ctrl_part <- paste(c(geo_ctrls, economic_ctrls, military_ctrls, hist_ctrls, "psu1919_vv"),
                   collapse = " + ")

## ── Variables ──────────────────────────────────────────────────────────────
endog_var <- "ass1900s_d"

outcomes <- list(
  list(var = "fascist_branch",     label = "Fascist Branch"),
  list(var = "fascist_violence",   label = "Fascist Violence"),
  list(var = "fascist1924_vv",     label = "Fascist Vote Share 1924"),
  list(var = "fascist1919_vv",     label = "Fascist Vote Share 1919"),
  list(var = "fascist1921_vv",     label = "Fascist Vote Share 1921"),
  list(var = "deportations_d",     label = "Deportations (Dummy)"),
  list(var = "depo_pop_cap1",      label = "Deportations (PC)"),
  list(var = "dlf_1926",           label = "DLF 1926"),
  list(var = "antifa_d",           label = "Antifascists (Dummy)"),
  list(var = "share_antifa_pop11", label = "Antifascists (PC)")
)

iv_specs <- list(
  list(raw = "stat",          label = "Statutes"),
  list(raw = "exposure_stat", label = "Exposure")
)

## ════════════════════════════════════════════════════════════════════════════
## EXCLUSION RESTRICTION PLOT
## ════════════════════════════════════════════════════════════════════════════
##
## Angrist–Pischke visual test:
##   - Split sample randomly into K groups
##   - In each group, estimate:
##       First stage:   D = γ·Z + X'δ + FE + ε
##       Reduced form:  Y = π·Z + X'δ + FE + ε
##   - Plot (γ̂_k, π̂_k) for each group k
##   - Under the exclusion restriction, points should line up on a line
##     through the origin with slope = LATE (the overall IV estimate)
##
## Uses the raw instrument Z directly — no propensity score needed.
## ════════════════════════════════════════════════════════════════════════════

plot_exclusion_restriction <- function(outcome_var, outcome_label,
                                       iv_var, iv_label, data,
                                       endog_var   = "ass1900s_d",
                                       n_groups    = 8,
                                       seed        = 123,
                                       cluster_var = ~provincia1921) {
  
  # --- Overall 2SLS estimate (reference slope) ---
  fml_iv <- as.formula(paste0(
    outcome_var, " ~ ", ctrl_part,
    " | province_fe",
    " | ", endog_var, " ~ ", iv_var
  ))
  iv_mod <- suppressWarnings(feols(fml_iv, data = data, cluster = cluster_var))
  
  coef_name    <- paste0("fit_", endog_var)
  overall_late <- tryCatch(as.numeric(coef(iv_mod)[coef_name]),
                           error = function(e) NA_real_)
  
  # --- Random split ---
  set.seed(seed)
  data$random_group <- sample(1:n_groups, nrow(data), replace = TRUE)
  
  plot_data <- data.frame(
    Group   = 1:n_groups,
    FS_Coef = NA_real_, RF_Coef = NA_real_,
    FS_SE   = NA_real_, RF_SE   = NA_real_
  )
  
  for (i in 1:n_groups) {
    sub <- data[data$random_group == i, ]
    if (nrow(sub) < 30) next
    
    fml_fs <- as.formula(paste0(endog_var, " ~ ", iv_var, " + ", ctrl_part, " | province_fe"))
    fml_rf <- as.formula(paste0(outcome_var, " ~ ", iv_var, " + ", ctrl_part, " | province_fe"))
    
    m_fs <- tryCatch(
      suppressWarnings(feols(fml_fs, data = sub, cluster = cluster_var)),
      error = function(e) NULL
    )
    m_rf <- tryCatch(
      suppressWarnings(feols(fml_rf, data = sub, cluster = cluster_var)),
      error = function(e) NULL
    )
    
    if (is.null(m_fs) || is.null(m_rf)) next
    
    if (iv_var %in% names(coef(m_fs))) {
      plot_data$FS_Coef[i] <- coef(m_fs)[iv_var]
      plot_data$FS_SE[i]   <- se(m_fs)[iv_var]
    }
    if (iv_var %in% names(coef(m_rf))) {
      plot_data$RF_Coef[i] <- coef(m_rf)[iv_var]
      plot_data$RF_SE[i]   <- se(m_rf)[iv_var]
    }
  }
  
  plot_data <- plot_data[!is.na(plot_data$FS_Coef) & !is.na(plot_data$RF_Coef), ]
  if (nrow(plot_data) == 0) stop("No groups estimated successfully.")
  
  # --- Plot ---
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
    theme_minimal(base_size = 12) +
    labs(
      title    = paste0("Exclusion Restriction Test: ", outcome_label),
      subtitle = paste0(
        "IV: ", iv_label,
        "  |  Dashed line = overall 2SLS estimate (",
        ifelse(!is.na(overall_late), sprintf("%.3f", overall_late), "N/A"),
        ")"
      ),
      x = paste0("First Stage coefficient on ", iv_label),
      y = paste0("Reduced Form coefficient on ", iv_label)
    )
  
  if (!is.na(overall_late)) {
    p <- p + geom_abline(intercept = 0, slope = overall_late,
                         color = "red", linetype = "dashed")
  }
  
  p
}

## ════════════════════════════════════════════════════════════════════════════
## FIGURE HELPERS
## ════════════════════════════════════════════════════════════════════════════

save_figure <- function(plot, filename, width = 8, height = 5) {
  path <- file.path(gr_dir, filename)
  ggsave(path, plot = plot, device = "pdf", width = width, height = height)
  cat("  → saved:", path, "\n")
  invisible(path)
}

preview_figures <- function(pdf_paths) {
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
  
  old_wd <- getwd()
  setwd(PREVIEW_DIR)
  on.exit(setwd(old_wd))
  
  result <- system2(local_tex_compiler,
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

## ════════════════════════════════════════════════════════════════════════════
## RUN
## ════════════════════════════════════════════════════════════════════════════

saved_figs <- c()

for (o in outcomes) {
  for (iv in iv_specs) {
    cat(sprintf("  Exclusion restriction: %s × %s\n", o$label, iv$label))
    
    p <- plot_exclusion_restriction(
      outcome_var  = o$var,
      outcome_label = o$label,
      iv_var       = iv$raw,
      iv_label     = iv$label,
      data         = df_iv,
      endog_var    = endog_var,
      n_groups     = 8
    )
    
    fname <- paste0("excl_restriction_", o$var, "_", iv$raw, ".pdf")
    saved_figs <- c(saved_figs, save_figure(p, fname))
  }
}

## ── Preview ────────────────────────────────────────────────────────────────
preview_figures(saved_figs)