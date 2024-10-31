library(dplyr)
library(styler)
c(
  "~/projects/atlasshinyexport/apps/cohortCharacterization/",
  "~/projects/atlasshinyexport/apps/IncidenceRate/",
  "~/projects/atlasshinyexport/apps/cohortPathways/",
  "~/projects/atlasshinyexport/apps/cohortCounts/") %>%
  lapply(function(dir) styler::style_dir(dir, exclude_dirs = c("packrat", "renv", "data", "data2", "data3")))