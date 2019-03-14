# Loading package and files -----------------------------------------------
library(LOGAN)
path <- "../../CEMO/LOGAN (aux)/"  # this changes from PC to PC
cp025q01 <- m0$ImportSPSS(filename = paste0(path, "CBA_cp025q01_logs12_SPSS.sav"))
load(paste0(path, "PISA2012_CBA2.RData"))

# Filtering out countries -------------------------------------------------
countries <- c("NOR", "SWE", "DNK")
cols <-   c("CNT", "SUBNATIO", "STRATUM", "OECD", "NC", "SCHOOLID", "StIDStd",
            "BOOKID", "FORMID", "ST04Q01", "CP025Q01","PV1CPRO", "PV2CPRO",
            "PV3CPRO", "PV4CPRO", "PV5CPRO", "W_FSTUWT")
cp025q01 <- cp025q01[cp025q01$cnt %in% countries, ]
pisa <- PISA2012_CBA[PISA2012_CBA$CNT %in% countries, cols]
save(cp025q01, file = "data/cp025q01.rda", compress = "xz")
save(pisa, file = "data/pisa.rda", compress = "xz")
