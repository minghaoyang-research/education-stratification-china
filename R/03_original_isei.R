# 03 Add the ISEI scores used in the dissertation

source("R/02_construct_variables.R")

# made from cgss2017_analysis_4.sps with tools/make_isei_map.R
isei_map <- read.csv("archive/isei_map_4.csv")

m <- match(stage3_dat$isco_main, isei_map$isco_main)
stage3_dat$isei <- as.numeric(isei_map$isei[m])

stopifnot(sum(!is.na(stage3_dat$isei)) == 1670)

stage3_dat$final_original_flag <- with(
  stage3_dat,
  !is.na(isei) & !is.na(rural_origin) & !is.na(par_edu_yrs)
)

analysis_original <- stage3_dat[stage3_dat$final_original_flag, , drop = FALSE]
stopifnot(nrow(analysis_original) == 1646)

# check against the 12 Apr SPSS output
stopifnot(
  round(mean(analysis_original$isei), 4) == 51.9842,
  round(sd(analysis_original$isei), 5) == 16.34346,
  min(analysis_original$isei) == 16,
  max(analysis_original$isei) == 88
)

dir.create("output/original_replication", recursive = TRUE, showWarnings = FALSE)

# education x origin check against the SPSS table
cell_n <- with(analysis_original, table(edu_lvl, rural_origin))
cell_mean <- with(
  analysis_original,
  tapply(isei, list(edu_lvl, rural_origin), mean)
)

cell_out <- data.frame(
  edu_lvl = c(1, 1, 2, 2, 3, 3),
  rural_origin = c(0, 1, 0, 1, 0, 1),
  expected_n = c(361, 346, 480, 326, 87, 46),
  expected_mean_isei = c(48.0028, 48.0983, 52.4937, 55.2423, 61.2874, 66.4565),
  n = c(cell_n[1, 1], cell_n[1, 2], cell_n[2, 1],
        cell_n[2, 2], cell_n[3, 1], cell_n[3, 2]),
  mean_isei = c(cell_mean[1, 1], cell_mean[1, 2], cell_mean[2, 1],
                cell_mean[2, 2], cell_mean[3, 1], cell_mean[3, 2])
)

write.csv(
  cell_out,
  "output/original_replication/education_origin_isei_check.csv",
  row.names = FALSE
)
