# 05 ISEI sensitivity check
#
# Compare the ISEI scores from my dissertation with the two mappings in
# ISCO08ConveRsions before comparing the models in Stage 06.
# isco08toisei08() is the main mapping, _2 is the older-table sensitivity check

library(ISCO08ConveRsions)
source("R/03_original_isei.R")   # brings in stage3_dat

out_dir <- "supplementary/output/isei_sensitivity"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# set up the data ----

dat <- stage3_dat

# keep ISCO as four digits: 0100/0110/0310 are missed if the leading zero is dropped
dat$isco4 <- sprintf("%04d", as.numeric(dat$isco_main))
dat$major_group <- substr(dat$isco4, 1, 1)
dat$isei_historical <- as.numeric(dat$isei)
dat$core <- !is.na(dat$rural_origin) & !is.na(dat$par_edu_yrs)

stopifnot(nrow(dat) == 1674,
          sum(dat$core) == 1650,
          sum(dat$core & !is.na(dat$isei_historical)) == 1646)

# quick check of the leading-zero problem
z <- c("100", "0100", "110", "0110", "310", "0310")
sapply(z, function(x) tryCatch(isco08toisei08(x), error = function(e) NA))
sapply(z, function(x) tryCatch(isco08toisei08_2(x), error = function(e) NA))


# one row for each occupation code ----

# the old ISEI score should not vary within the same ISCO code
n_hist <- tapply(dat$isei_historical, dat$isco4, function(x) length(unique(na.omit(x))))
stopifnot(all(n_hist <= 1))
hist_by_code <- tapply(dat$isei_historical, dat$isco4, function(x) x[!is.na(x)][1])

code_map <- data.frame(isco4 = sort(unique(dat$isco4)))
code_map$isco_numeric <- as.integer(code_map$isco4)
code_map$major_group <- substr(code_map$isco4, 1, 1)
code_map$isei_historical <- as.numeric(hist_by_code[code_map$isco4])
code_map$isei_syntax <- sapply(code_map$isco4, function(x) suppressWarnings(as.numeric(isco08toisei08(x))))
code_map$isei_overview <- sapply(code_map$isco4, function(x) suppressWarnings(as.numeric(isco08toisei08_2(x))))
code_map$n_stage3 <- as.integer(table(dat$isco4)[code_map$isco4])
code_map$n_core <- as.integer(table(factor(dat$isco4[dat$core], levels = code_map$isco4)))

code_map$syntax_minus_historical <- code_map$isei_syntax - code_map$isei_historical
code_map$overview_minus_syntax <- code_map$isei_overview - code_map$isei_syntax
code_map$abs_syntax_minus_historical <- abs(code_map$syntax_minus_historical)
code_map$abs_overview_minus_syntax <- abs(code_map$overview_minus_syntax)
code_map$person_weighted_abs_hist_syntax <- code_map$n_core * code_map$abs_syntax_minus_historical
code_map$person_weighted_abs_syntax_overview <- code_map$n_core * code_map$abs_overview_minus_syntax

# match the two alternative scores back to respondents
i <- match(dat$isco4, code_map$isco4)
dat$isei_syntax <- code_map$isei_syntax[i]
dat$isei_overview <- code_map$isei_overview[i]

core_dat <- dat[dat$core, ]
core_codes <- code_map[code_map$n_core > 0, ]


# basic coverage and score distributions ----
# 1674 cases, 1650 core, 248 codes

scores <- c("isei_historical", "isei_syntax", "isei_overview")

score_summary <- data.frame(
  score = scores,
  mapped_all = colSums(!is.na(dat[scores])),
  mapped_core = colSums(!is.na(core_dat[scores])),
  codes_mapped = colSums(!is.na(code_map[scores])),
  core_codes_mapped = colSums(!is.na(core_codes[scores])),
  mean = sapply(core_dat[scores], mean, na.rm = TRUE),
  sd = sapply(core_dat[scores], sd, na.rm = TRUE),
  min = sapply(core_dat[scores], min, na.rm = TRUE),
  max = sapply(core_dat[scores], max, na.rm = TRUE)
)
score_summary

with(core_dat, table(historical = !is.na(isei_historical), syntax = !is.na(isei_syntax)))
sapply(core_dat[scores], function(x) length(unique(na.omit(x))))   # number of distinct scores

core_codes[is.na(core_codes$isei_historical), c("isco4", "n_core")]   # only ISCO 1000
core_codes[is.na(core_codes$isei_syntax) | is.na(core_codes$isei_overview), c("isco4", "n_core")]   # no core codes missing


# compare the mappings ----

compare <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]
  b <- b[ok]
  d <- b - a
  data.frame(n = sum(ok), r = cor(a, b),
             mean_a = mean(a), mean_b = mean(b),
             mean_diff = mean(d), median_diff = median(d), sd_diff = sd(d),
             mean_abs_diff = mean(abs(d)), max_abs_diff = max(abs(d)),
             pct_same = 100 * mean(d == 0))
}

pairs <- list(c("isei_historical", "isei_syntax"),
              c("isei_historical", "isei_overview"),
              c("isei_syntax", "isei_overview"))

comparison <- NULL
for (p in pairs) {
  comparison <- rbind(comparison,
    data.frame(level = "person", a = p[1], b = p[2], compare(core_dat[[p[1]]], core_dat[[p[2]]])),
    data.frame(level = "code", a = p[1], b = p[2], compare(core_codes[[p[1]]], core_codes[[p[2]]])))
}
comparison

# largest historical -> syntax changes, weighted by the number of core respondents
top_changes <- core_codes[order(-core_codes$person_weighted_abs_hist_syntax,
                                -core_codes$abs_syntax_minus_historical), ]
top_changes <- head(top_changes, 20)

# unweighted code-level changes
head(core_codes[order(-core_codes$abs_syntax_minus_historical), c("isco4", "n_core", "isei_historical", "isei_syntax")], 10)
head(core_codes[order(-core_codes$abs_overview_minus_syntax), c("isco4", "n_core", "isei_syntax", "isei_overview")], 10)


# changes by education and rural origin ----

delta <- core_dat[!is.na(core_dat$isei_historical) & !is.na(core_dat$isei_syntax), ]
delta$change <- delta$isei_syntax - delta$isei_historical

change_stats <- function(x) {
  data.frame(n = nrow(x),
             mean_hist = mean(x$isei_historical),
             mean_syntax = mean(x$isei_syntax),
             mean_change = mean(x$change),
             sd_change = sd(x$change),
             mean_abs_change = mean(abs(x$change)))
}

by_edu <- do.call(rbind, lapply(split(delta, delta$edu_lvl), change_stats))
by_rural <- do.call(rbind, lapply(split(delta, delta$rural_origin), change_stats))
by_both <- do.call(rbind, lapply(split(delta, list(delta$edu_lvl, delta$rural_origin), drop = TRUE), change_stats))

change_by_group <- rbind(
  data.frame(group = paste("edu", rownames(by_edu)), by_edu),
  data.frame(group = paste("rural", rownames(by_rural)), by_rural),
  data.frame(group = paste("edu.rural", rownames(by_both)), by_both)
)
change_by_group

tapply(delta$change, delta$major_group, mean)   # mean change by ISCO major group


# effect on the analytic sample ----

new_cases <- dat$core & is.na(dat$isei_historical) & !is.na(dat$isei_syntax)
lost_cases <- dat$core & !is.na(dat$isei_historical) & is.na(dat$isei_syntax)
table(dat$isco4[new_cases])
table(edu = dat$edu_lvl[new_cases], rural = dat$rural_origin[new_cases])
# all four added cases are ISCO 1000, urban-origin and bachelor, so the Table 6 margins
# go 928 -> 932 (urban) and 806 -> 810 (bachelor)

sample_tab <- function(score) {
  s <- core_dat[!is.na(core_dat[[score]]), ]
  data.frame(score = score, as.data.frame(table(edu_lvl = s$edu_lvl, rural_origin = s$rural_origin)))
}
composition <- rbind(sample_tab("isei_historical"), sample_tab("isei_syntax"), sample_tab("isei_overview"))


# write the results ----

pkg_info <- data.frame(
  package = "ISCO08ConveRsions",
  version = as.character(packageVersion("ISCO08ConveRsions")),
  primary_function = "isco08toisei08",
  sensitivity_function = "isco08toisei08_2"
)

# 06 reads the first two files, the rest are for checking
write.csv(code_map, file.path(out_dir, "05_code_level_mapping.csv"), row.names = FALSE, na = "")
write.csv(pkg_info, file.path(out_dir, "05_package_provenance.csv"), row.names = FALSE, na = "")

write.csv(score_summary, file.path(out_dir, "05_score_summary.csv"), row.names = FALSE)
write.csv(comparison, file.path(out_dir, "05_mapping_comparison.csv"), row.names = FALSE)
write.csv(top_changes, file.path(out_dir, "05_top_changed_codes.csv"), row.names = FALSE)
write.csv(change_by_group, file.path(out_dir, "05_change_by_group.csv"), row.names = FALSE)
write.csv(composition, file.path(out_dir, "05_sample_composition.csv"), row.names = FALSE)

stopifnot(nrow(code_map) == 248,
          sum(new_cases) == 4,
          sum(lost_cases) == 0,
          !anyNA(core_dat$isei_syntax),
          !anyNA(core_dat$isei_overview))
