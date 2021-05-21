
require(data.table)

.debug <- "."
.args <- if (interactive()) c(
  file.path(.debug, "data", "cog_metadata.csv"),
  file.path(.debug, "data", "data_proportion.rds")
) else commandArgs(trailingOnly = TRUE)

cog <- fread(.args[1])[
  country == "UK" & pillar_2 == TRUE
][, .(
    .N,
    B.1.617.1 = sum(lineage %like% "B\\.1\\.617\\.1"),
    B.1.617.2 = sum(lineage %like% "B\\.1\\.617\\.2"),
    B.1.617.3 = sum(lineage %like% "B\\.1\\.617\\.3")
), keyby = sample_date][
  1:(.N-2) # leave off last two days
]

#' TODO precalculate values of interest
cog[,
  `p.1.617.2` :=  `B.1.617.2`/N
][,
  `non.1.617.2` :=  N-`B.1.617.2`
]

#' @example 
#' cog[, which(as.integer(diff(sample_date)) != 1) ]
#' lots of missing days initially, then a few small patches

saveRDS(cog, tail(.args, 1))