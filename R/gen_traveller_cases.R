
require(data.table)

.debug <- "."
.args <- if (interactive()) c(
  file.path(.debug, "data", "voc_imports_2021_05_13.csv"),
  file.path(.debug, "data", "traveller_cases.rds")
) else commandArgs(trailingOnly = TRUE)

traveller_cases <- dcast(
  fread(.args[1]),
  Date ~ `Travel Indicator`,
  value.var = "Number_B_1_617_2",
  fill = 0
)
names(traveller_cases)[which(names(traveller_cases)=="Traveller")] <- "Number_B_1_617_2"
names(traveller_cases)[which(names(traveller_cases)=="Date")] <- "date"

saveRDS(traveller_cases, tail(.args, 1))