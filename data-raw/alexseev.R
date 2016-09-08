#' Clean alexseev data
library("foreign")
library("dplyr")
alexseev <- read.dta("raw-data/alexseev.dta") %>%
  na.omit() %>%
  as_data_frame() %>%
  mutate(brdcont = as.logical(brdcont))

for (i in c("datalabel", "time.stamp", "formats", "types",
            "val.labels", "var.labels", "version")) {
 attr(alexseev, i) <- NULL
}

save(alexseev, file = "data/alexseev.rda")