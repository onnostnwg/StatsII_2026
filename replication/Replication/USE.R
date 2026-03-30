rm(list = ls())

setwd("C:/Users/onnos/Desktop/TRINITY/PhD/Quantitative Methods II/Replication Paper/Replication")

# packages
library(haven)
library(dplyr)
library(ggplot2)
library(patchwork)

dir.create("dump", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)

# --- helper: weighted column means for a matrix ---
wcolMeans <- function(X, w) {
  # X: matrix, w: numeric weights same length as nrow(X)
  as.numeric(crossprod(w, X) / sum(w))
}

# --- helper: Stata-like margins atmeans for one variable ---
margins_atmeans_curve <- function(model, data_used, var, by = 0.1) {
  # Build model matrix exactly as used by glm
  X <- model.matrix(model, data_used)
  w <- model$weights
  beta <- coef(model)
  
  # weighted mean of each column (this is the 'atmeans' covariate vector)
  xbar <- wcolMeans(X, w)
  names(xbar) <- colnames(X)
  
  # grid of var values
  grid <- seq(0, 1, by = by)
  
  # find the column that corresponds to the var
  # (works if var enters linearly as its own column)
  if (!var %in% names(xbar)) {
    stop(paste0("Variable column not found in model matrix: ", var,
                "\nAvailable columns include: ", paste(head(names(xbar), 10), collapse = ", "), " ..."))
  }
  
  # Create Xbar rows, varying var
  Xnew <- matrix(rep(xbar, each = length(grid)), nrow = length(grid), byrow = FALSE)
  colnames(Xnew) <- names(xbar)
  Xnew[, var] <- grid
  
  # predicted probability
  p <- plogis(Xnew %*% beta)
  
  data.frame(x = grid, p = as.numeric(p))
}

plot_margins_atmeans <- function(model, data_used, var, xlab, ylab = "") {
  curve <- margins_atmeans_curve(model, data_used, var, by = 0.1)
  ggplot(curve, aes(x = x, y = p)) +
    geom_line() +
    labs(x = xlab, y = ylab) +
    theme_bw()
}

# =========================================================
# PART 1: Vote switching (ww)  --- matches Stata section 1
# =========================================================

df1 <- read_dta("data/data_wahlwechsel.dta") %>%
  mutate(
    ww = case_when(
      Wahlwechsel == 1 ~ 0,
      Wahlwechsel == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    geschlecht = factor(geschlecht),
    bildung = factor(bildung),
    haushaltseinkommen = factor(haushaltseinkommen),
    region = factor(region)
  ) %>%
  # important: match Stata estimation sample (complete cases)
  filter(complete.cases(
    ww, OpennessMittelwert, ConscientiousnessMittelwert, ExtraversionMittelwert,
    AgreeablenessMittelwert, NeuroticismMittelwert, LiRe, alter,
    geschlecht, bildung, haushaltseinkommen, region, ipf_gewicht
  ))

m1 <- glm(
  ww ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert +
    AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter +
    geschlecht + bildung + haushaltseinkommen + region,
  data = df1,
  family = binomial(link = "logit"),
  weights = ipf_gewicht
)

p1 <- plot_margins_atmeans(m1, df1, "OpennessMittelwert",
                           xlab = "Openness",
                           ylab = "Predicted probability for vote switching")
p2 <- plot_margins_atmeans(m1, df1, "ConscientiousnessMittelwert", "Conscientiousness")
p3 <- plot_margins_atmeans(m1, df1, "ExtraversionMittelwert", "Extraversion")
p4 <- plot_margins_atmeans(m1, df1, "AgreeablenessMittelwert", "Agreeableness")
p5 <- plot_margins_atmeans(m1, df1, "NeuroticismMittelwert", "Neuroticism")

row_voteswitch <- (p1 | p2 | p3 | p4 | p5) +
  plot_annotation(subtitle = "Vote switching")

# =========================================================
# PART 2: Populism (pop)  --- matches Stata section 2
# =========================================================

df2 <- read_dta("data/data_populismus.dta") %>%
  mutate(
    pop = case_when(
      Populismusanfällige == 1 ~ 0,
      Populismusanfällige == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    geschlecht = factor(geschlecht),
    bildung = factor(bildung),
    haushaltseinkommen = factor(haushaltseinkommen),
    region = factor(region)
  ) %>%
  filter(complete.cases(
    pop, OpennessMittelwert, ConscientiousnessMittelwert, ExtraversionMittelwert,
    AgreeablenessMittelwert, NeuroticismMittelwert, LiRe, alter,
    geschlecht, bildung, haushaltseinkommen, region, ipf_gewicht
  ))

m2 <- glm(
  pop ~ OpennessMittelwert + ConscientiousnessMittelwert + ExtraversionMittelwert +
    AgreeablenessMittelwert + NeuroticismMittelwert + LiRe + alter +
    geschlecht + bildung + haushaltseinkommen + region,
  data = df2,
  family = binomial(link = "logit"),
  weights = ipf_gewicht
)





































q1 <- plot_margins_atmeans(m2, df2, "OpennessMittelwert",
                           xlab = "Openness",
                           ylab = "Predicted probability for vote switching to AfD")
q2 <- plot_margins_atmeans(m2, df2, "ConscientiousnessMittelwert", "Conscientiousness")
q3 <- plot_margins_atmeans(m2, df2, "ExtraversionMittelwert", "Extraversion")
q4 <- plot_margins_atmeans(m2, df2, "AgreeablenessMittelwert", "Agreeableness")
q5 <- plot_margins_atmeans(m2, df2, "NeuroticismMittelwert", "Neuroticism")

row_pop <- (q1 | q2 | q3 | q4 | q5) +
  plot_annotation(subtitle = "Vote switching to AfD = susceptibility to populism")

# =========================================================
# Combine like Stata graph combine ... , ycom row(2)
# =========================================================

# Force a common y-scale across ALL panels (like ycom)
all_curves <- bind_rows(
  margins_atmeans_curve(m1, df1, "OpennessMittelwert") %>% mutate(panel="a1"),
  margins_atmeans_curve(m1, df1, "ConscientiousnessMittelwert") %>% mutate(panel="a2"),
  margins_atmeans_curve(m1, df1, "ExtraversionMittelwert") %>% mutate(panel="a3"),
  margins_atmeans_curve(m1, df1, "AgreeablenessMittelwert") %>% mutate(panel="a4"),
  margins_atmeans_curve(m1, df1, "NeuroticismMittelwert") %>% mutate(panel="a5"),
  margins_atmeans_curve(m2, df2, "OpennessMittelwert") %>% mutate(panel="b1"),
  margins_atmeans_curve(m2, df2, "ConscientiousnessMittelwert") %>% mutate(panel="b2"),
  margins_atmeans_curve(m2, df2, "ExtraversionMittelwert") %>% mutate(panel="b3"),
  margins_atmeans_curve(m2, df2, "AgreeablenessMittelwert") %>% mutate(panel="b4"),
  margins_atmeans_curve(m2, df2, "NeuroticismMittelwert") %>% mutate(panel="b5")
)

ylim_all <- range(all_curves$p, na.rm = TRUE)

# apply common y-limits
row_voteswitch2 <- row_voteswitch & coord_cartesian(ylim = ylim_all)
row_pop2 <- row_pop & coord_cartesian(ylim = ylim_all)

fig4 <- row_voteswitch2 / row_pop2
fig4_labeled <- fig4 + plot_annotation(tag_levels = "a")

ggsave("figures/Figure 4.pdf", fig4_labeled, width = 12, height = 6)
ggsave("figures/Figure 4.png", fig4_labeled, width = 12, height = 6, dpi = 300)