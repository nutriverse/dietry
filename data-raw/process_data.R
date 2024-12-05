# Process datasets -------------------------------------------------------------

## FCS sample dataset from WFP-VAM Resource Centre ----

fcs01 <- read.csv("https://raw.githubusercontent.com/WFP-VAM/RAMResourcesScripts/refs/heads/main/Static/FCS_Sample_Survey.csv")

usethis::use_data(fcs01, overwrite = FALSE, compress = "xz")
