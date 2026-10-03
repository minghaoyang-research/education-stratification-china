# 04 Models 1-7 and Tables 6, 7 and A1
#
# CGSS 2017, final analytic sample N = 1646.
# This reproduces the models and tables from my dissertation using the
# submitted unweighted specifications and classical standard errors.
# I check R against the final SPSS v4 output as printed: most values are
# shown to 3 decimals, while the Table 7 means are shown to 4.
source("R/03_original_isei.R")

# SPSS only gives the displayed decimals, so I compare within half of the
# last printed digit. The tiny extra term avoids a false failure from binary
# floating-point representation at a rounding boundary.
near_spss <- function(x, spss, digits = 3, label = "") {
  tol <- 0.5 * 10^-digits + sqrt(.Machine$double.eps)
  if (abs(x - spss) >= tol) stop(label, ": does not match SPSS")
}

# I calculate VIF directly so this script stays in base R. These models
# use single-column numeric predictors, so this is the usual 1 / (1 - R2) VIF.
calc_vif <- function(fit) {
  x <- model.matrix(fit)
  x <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  ans <- sapply(seq_len(ncol(x)), function(j) {
    e <- lm.fit(cbind(1, x[, -j, drop = FALSE]), x[, j])$residuals
    r2 <- 1 - sum(e^2) / sum((x[, j] - mean(x[, j]))^2)
    1 / (1 - r2)
  })
  setNames(ans, colnames(x))
}

common_terms <- c("edu_bachelor", "edu_postgrad", "female")
model_rhs <- list(
  Model1 = c(common_terms, "age"),
  Model2 = c(common_terms, "age", "par_edu_yrs"),
  Model3 = c(common_terms, "age", "par_edu_yrs", "rural_origin"),
  Model4 = c(common_terms, "age", "par_edu_yrs", "rural_origin",
             "bach_x_rural", "post_x_rural"),
  Model5 = c(common_terms, "par_edu_yrs", "rural_origin",
             "cohort2", "cohort3", "cohort4"),
  Model6 = c(common_terms, "par_edu_yrs", "rural_origin",
             "cohort2", "cohort3", "cohort4",
             "bach_x_c2", "bach_x_c3", "bach_x_c4",
             "post_x_c2", "post_x_c3", "post_x_c4"),
  Model7 = c(common_terms, "age", "par_edu_yrs", "rural_origin_f")
)

d <- analysis_original

# Interaction terms used in Models 4 and 6. The education and cohort dummies
# are already created in Stage 02; the interactions belong with the models.
d$bach_x_rural <- d$edu_bachelor * d$rural_origin
d$post_x_rural <- d$edu_postgrad * d$rural_origin

d$bach_x_c2 <- d$edu_bachelor * d$cohort2
d$bach_x_c3 <- d$edu_bachelor * d$cohort3
d$bach_x_c4 <- d$edu_bachelor * d$cohort4
d$post_x_c2 <- d$edu_postgrad * d$cohort2
d$post_x_c3 <- d$edu_postgrad * d$cohort3
d$post_x_c4 <- d$edu_postgrad * d$cohort4

models <- lapply(model_rhs, function(x) lm(reformulate(x, "isei"), data = d))
stopifnot(sapply(models, nobs) == 1646)


# 1. Coefficients and fit ----

# SPSS B and SE, in the same term order as model_rhs.
# M1 is the baseline; M2 adds parental education; M3 adds rural origin.
# M4 adds education x origin; M5 replaces age with cohorts; M6 adds
# education x cohort; M7 reruns M3 with the A27F residence measure.
#
# Two decisions here came out of the earlier modelling rather than being
# part of the first specification: I changed education from a single 1/2/3
# variable to separate degree dummies, and I split age from the cohort models
# because in a single 2017 cross-section age and birth cohort are almost
# mechanically linked, which made the combined specification hard to interpret.
spss_b <- list(
  Model1 = c(38.447, 6.274, 16.199, 0.943, 0.230),
  Model2 = c(36.637, 6.072, 15.761, 0.918, 0.243, 0.149),
  Model3 = c(32.802, 6.205, 15.899, 1.062, 0.272, 0.281, 3.025),
  Model4 = c(33.562, 5.159, 14.116, 1.049, 0.267, 0.294, 1.651, 2.205, 4.380),
  Model5 = c(48.859, 6.184, 15.971, 1.098, 0.260, 2.918, -3.542, -5.809, -7.885),
  Model6 = c(48.950, 5.873, 15.764, 1.167, 0.258, 2.956, -2.252, -6.878, -8.354,
             -2.655, 1.397, 1.228, -2.684, 4.479, -1.101),
  Model7 = c(32.884, 6.179, 15.859, 1.067, 0.269, 0.283, 3.089)
)
spss_se <- list(
  Model1 = c(1.814, 0.817, 1.494, 0.776, 0.041),
  Model2 = c(2.188, 0.828, 1.523, 0.775, 0.042, 0.101),
  Model3 = c(2.423, 0.826, 1.518, 0.774, 0.043, 0.107, 0.833),
  Model4 = c(2.470, 1.112, 1.920, 0.773, 0.043, 0.107, 1.206, 1.626, 3.090),
  Model5 = c(1.556, 0.829, 1.523, 0.777, 0.107, 0.834, 1.492, 1.433, 1.422),
  Model6 = c(1.761, 2.522, 11.163, 0.778, 0.107, 0.838, 1.928, 1.907, 1.872,
             3.080, 2.911, 2.852, 11.720, 11.462, 11.366),
  Model7 = c(2.403, 0.825, 1.517, 0.774, 0.043, 0.107, 0.832)
)

for (m in names(models)) {
  cf <- coef(summary(models[[m]]))
  stopifnot(nrow(cf) == length(spss_b[[m]]))
  for (j in seq_len(nrow(cf))) {
    near_spss(cf[j, 1], spss_b[[m]][j], 3, paste(m, rownames(cf)[j], "B"))
    near_spss(cf[j, 2], spss_se[[m]][j], 3, paste(m, rownames(cf)[j], "SE"))
  }
}

fit_all <- cbind(model = names(models), do.call(rbind, lapply(models, function(fit) {
  sm <- summary(fit)
  data.frame(
    N = nobs(fit),
    residual_df = df.residual(fit),
    R2 = sm$r.squared,
    adjusted_R2 = sm$adj.r.squared,
    residual_SE = sm$sigma,
    RSS = deviance(fit),
    F = sm$fstatistic[[1]],
    model_df = sm$fstatistic[[2]],
    denominator_df = sm$fstatistic[[3]]
  )
})))

spss_r2 <- c(0.085, 0.086, 0.093, 0.095, 0.091, 0.095, 0.093)
spss_adj_r2 <- c(0.082, 0.083, 0.090, 0.090, 0.087, 0.087, 0.090)
for (i in seq_along(models)) {
  near_spss(fit_all$R2[i], spss_r2[i], 3, paste(fit_all$model[i], "R2"))
  near_spss(fit_all$adjusted_R2[i], spss_adj_r2[i], 3,
            paste(fit_all$model[i], "adjusted R2"))
}
stopifnot(fit_all$residual_df == c(1641, 1640, 1639, 1637, 1637, 1631, 1639))

# residual sums of squares from the ANOVA tables
spss_rss <- c(Model1 = 402203.422, Model2 = 401669.114, Model3 = 398465.254,
              Model4 = 397735.913, Model5 = 399396.001, Model6 = 397692.944,
              Model7 = 398317.118)
for (m in names(spss_rss)) {
  near_spss(deviance(models[[m]]), spss_rss[[m]], 3, paste(m, "RSS"))
}

coefficients_all <- do.call(rbind, lapply(names(models), function(m) {
  cf <- coef(summary(models[[m]]))
  data.frame(model = m, term = rownames(cf), B = cf[, 1], SE = cf[, 2],
             t = cf[, 3], p = cf[, 4], row.names = NULL)
}))


# 2. VIF ----

vif_all <- do.call(rbind, lapply(names(models), function(m) {
  v <- calc_vif(models[[m]])
  data.frame(model = m, term = names(v), VIF = unname(v), tolerance = 1 / unname(v))
}))

# In Model 6 the reference cohort (1957-64) contains only 2 postgraduates;
# the postgraduate main effect is therefore estimated from that sparse cell.
spss_vif6 <- c(10.729, 62.480, 1.021, 1.258, 1.165, 4.057, 5.273, 5.585,
               4.951, 8.008, 8.801, 11.679, 22.563, 34.053)
vif6 <- calc_vif(models$Model6)
stopifnot(length(vif6) == length(spss_vif6))
for (j in seq_along(vif6)) {
  near_spss(vif6[[j]], spss_vif6[j], 3, paste("Model6 VIF", names(vif6)[j]))
}
# my dissertation reports a maximum VIF of 3.218 across the other six models
near_spss(max(vif_all$VIF[vif_all$model != "Model6"]), 3.218, 3,
          "max VIF, Models 1-5 and 7")


# 3. Table 6 and Appendix A1 ----

tab6 <- table(rural_origin = d$rural_origin, edu_lvl = d$edu_lvl)
tab_a1 <- table(cohort = d$cohort, edu_lvl = d$edu_lvl)
stopifnot(
  all(tab6 == matrix(c(361, 480, 87,
                       346, 326, 46), 2, byrow = TRUE)),
  all(tab_a1 == matrix(c(108, 60, 2,
                         174, 139, 21,
                         194, 277, 43,
                         231, 330, 67), 4, byrow = TRUE))
)

chi6 <- chisq.test(tab6)
chi_a1 <- chisq.test(tab_a1)
# The 1957-64 postgraduate cell has N = 2, but its expected count is 13.74.
near_spss(unname(chi6$statistic), 15.847, 3, "Table 6 chi-square")
near_spss(unname(chi_a1$statistic), 63.051, 3, "Appendix A1 chi-square")
stopifnot(chi6$parameter == 2, chi_a1$parameter == 6)


# 4. Table 7: ISEI by cohort and education ----

table7 <- data.frame(cohort = rep(1:4, each = 3), edu_lvl = rep(1:3, 4))
cell_isei <- function(i) {
  d$isei[d$cohort == table7$cohort[i] & d$edu_lvl == table7$edu_lvl[i]]
}
table7$n <- sapply(1:12, function(i) length(cell_isei(i)))
table7$mean_isei <- sapply(1:12, function(i) mean(cell_isei(i)))
table7$sd_isei <- sapply(1:12, function(i) sd(cell_isei(i)))

stopifnot(table7$n == as.vector(t(tab_a1)))
spss_t7 <- c(52.1204, 58.2667, 70.0000,
             50.7299, 54.0719, 64.5238,
             46.4742, 53.8953, 66.8605,
             45.4502, 52.3182, 59.9851)
for (i in 1:12) near_spss(table7$mean_isei[i], spss_t7[i], 4, paste("Table 7 row", i))

table7$cohort_label <- c("1957-64", "1965-74", "1975-84", "1985-92")[table7$cohort]
table7$education_label <- c("Junior college", "Bachelor", "Postgraduate")[table7$edu_lvl]


# 5. Joint F tests for the interaction blocks ----

# SPSS did not print these joint tests. The reference values below follow
# from the residual sums of squares in the final SPSS ANOVA tables.
a34 <- anova(models$Model3, models$Model4)
a56 <- anova(models$Model5, models$Model6)
nested_tests <- data.frame(
  comparison = c("Model 3 vs Model 4: education x rural-origin block",
                 "Model 5 vs Model 6: education x cohort block"),
  numerator_df = c(a34$Df[2], a56$Df[2]),
  denominator_df = c(a34$Res.Df[2], a56$Res.Df[2]),
  F = c(a34$F[2], a56$F[2]),
  p = c(a34$`Pr(>F)`[2], a56$`Pr(>F)`[2])
)
stopifnot(nested_tests$numerator_df == c(2, 6),
          nested_tests$denominator_df == c(1637, 1631))
near_spss(nested_tests$F[1], 1.501, 3, "Model 3 vs 4 F")
near_spss(nested_tests$p[1], 0.223, 3, "Model 3 vs 4 p")
near_spss(nested_tests$F[2], 1.164, 3, "Model 5 vs 6 F")
near_spss(nested_tests$p[2], 0.323, 3, "Model 5 vs 6 p")


# 6. Save ----

out_dir <- "output/original_replication"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
outputs <- list(
  original_models_coefficients = coefficients_all,
  original_models_fit_statistics = fit_all,
  original_models_vif = vif_all,
  table6_origin_by_education = as.data.frame(tab6, responseName = "n"),
  appendix_a1_cohort_by_education = as.data.frame(tab_a1, responseName = "n"),
  table7_isei_by_cohort_and_education = table7,
  original_joint_interaction_tests = nested_tests
)
for (nm in names(outputs)) {
  write.csv(outputs[[nm]], file.path(out_dir, paste0(nm, ".csv")), row.names = FALSE)
}

cat("04_original_models.R: ALL CHECKS PASSED\n")
