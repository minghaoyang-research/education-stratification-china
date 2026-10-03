# Build the ISEI lookup used in the dissertation from the archived SPSS syntax.

args <- commandArgs(trailingOnly = TRUE)
sps_file <- if (length(args) >= 1) args[1] else "archive/cgss2017_analysis_4.sps"
out_file <- if (length(args) >= 2) args[2] else "archive/isei_map_4.csv"

sps <- trimws(readLines(sps_file, warn = FALSE, encoding = "UTF-8"))

rule_re <- paste0(
  "^IF\\s*(.*?)\\s+isei\\s*=\\s*",
  "(\\$SYSMIS|-?[0-9]+(?:\\.[0-9]+)?)\\s*\\.\\s*$"
)

rule_lines <- sps[grepl(rule_re, sps, ignore.case = TRUE, perl = TRUE)]
stopifnot(length(rule_lines) == 257)

parts <- regmatches(
  rule_lines,
  regexec(rule_re, rule_lines, ignore.case = TRUE, perl = TRUE)
)

cond_spss <- trimws(vapply(parts, `[`, "", 2))
value_txt <- vapply(parts, `[`, "", 3)
value <- ifelse(
  toupper(value_txt) == "$SYSMIS",
  NA_real_,
  suppressWarnings(as.numeric(value_txt))
)

spss_to_r <- function(x) {
  x <- gsub(
    "NOT\\s+MISSING\\s*\\(([^)]+)\\)",
    "!is.na(\\1)", x,
    ignore.case = TRUE, perl = TRUE
  )
  x <- gsub(
    "MISSING\\s*\\(([^)]+)\\)",
    "is.na(\\1)", x,
    ignore.case = TRUE, perl = TRUE
  )

  ops <- c(
    AND = "&", OR = "|", GE = ">=", LE = "<=",
    GT = ">", LT = "<", NE = "!=", EQ = "=="
  )

  for (op in names(ops)) {
    x <- gsub(
      paste0("\\b", op, "\\b"),
      ops[[op]], x,
      ignore.case = TRUE, perl = TRUE
    )
  }

  x <- gsub("(?<![><!=])=(?!=)", "==", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}

cond_r <- vapply(cond_spss, spss_to_r, "", USE.NAMES = FALSE)

env <- new.env(parent = baseenv())
env$isco_main <- 1:9999
env$isei <- rep(NA_real_, 9999)

for (i in seq_along(cond_r)) {
  hit <- eval(parse(text = cond_r[i]), envir = env)
  hit[is.na(hit)] <- FALSE
  env$isei[hit] <- value[i]
}

isei_map <- data.frame(
  isco_main = env$isco_main,
  isei = env$isei
)

stopifnot(
  nrow(isei_map) == 9999,
  sum(!is.na(isei_map$isei)) == 5600,
  isei_map$isei[isei_map$isco_main == 100] == 45,
  isei_map$isei[isei_map$isco_main == 310] == 45
)

write.csv(isei_map, out_file, row.names = FALSE)

cat(
  "Wrote ", nrow(isei_map), " rows to ", out_file,
  " (", sum(!is.na(isei_map$isei)), " mapped)\n",
  sep = ""
)
