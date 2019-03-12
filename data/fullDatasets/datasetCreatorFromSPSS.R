library(LOGAN)
file.path <- "../../CEMO/LOGAN (aux)/CBA_cp025q01_logs12_SPSS.sav"
cp025q01 <- m0$ImportSPSS(filename = file.path)
countries <- c("NOR", "SWE", "DNK")
cp025q01 <- cp025q01[cp025q01$cnt %in% countries, ]
save(cp025q01, file = "data/cp025q01.rda", compress = "xz")