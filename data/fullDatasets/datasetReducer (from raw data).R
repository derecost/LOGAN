# Loading package and files -----------------------------------------------
library(LOGAN)
path <- "../../CEMO/LOGAN (aux)/"  # this changes from PC to PC
cp025q01 <- m0$ImportSPSS(filename = paste0(path, 
                          "CBA_cp025q01_logs12_SPSS.sav"))
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

# Working out cp025q01 for some function examples -------------------------
df <- cp025q01
df$NewID <- paste0(df$cnt, "-", df$schoolid, "-", df$StIDStd)
trim.vars <- c("event", "event_type", "top_setting", "central_setting",
               "bottom_setting", "diag_state")
df.trimmed <- m0$TrimVar(df, trim.vars)
library(rlang)
concat.events <- c(quo(event), quo(event_type), quo(top_setting),
                   quo(central_setting), quo(bottom_setting), quo(diag_state))
df.conc <- m0$ConcatActions(df.trimmed, concat.events)
clear.events <- c("ACER_EVENT_" = "", "_NULL" = "")
df.clean <- m0$CleanActions(df.conc, event.type, clear.events)
time.vars <- c("cnt", "schoolid", "StIDStd", "NewID")
df.start <- m1$VarTimebyID(df.clean, NewID, time, new.event.type,
                           "START_ITEM",
                           "CP025Q01.START")[c(time.vars, "CP025Q01.START")]
df.end   <- m1$VarTimebyID(df.clean, NewID, time, new.event.type,
                           "END_ITEM",
                           "CP025Q01.END")[c(time.vars, "CP025Q01.END")]
df.dataAct <- m0$DataActionsbyID(df.clean, NewID, new.event.type,
                                 "CP025Q01.ACTIONS")[c(time.vars, "CP025Q01.ACTIONS")]
df.time <- dplyr::left_join(df.start, df.end, by = time.vars)
df.timeActions <- dplyr::left_join(df.time, df.dataAct, by = time.vars)
df.dataAct <- m1$TOTVar(df.timeActions, "CP025Q01.START", "CP025Q01.END",
                        divBy = 60, tot.var = "CP025Q01.TOT")
actions <- c("apply_1_0_0", "apply_-1_0_0", "apply_2_0_0", "apply_-2_0_0",
             "apply_0_1_0", "apply_0_-1_0", "apply_0_2_0", "apply_0_-2_0",
              "apply_0_0_1", "apply_0_0_-1", "apply_0_0_2", "apply_0_0_-2")
df.dataAct <- m2$VarActionSearch(df.dataAct, "CP025Q01.ACTIONS", actions)
df.dataAct <- m1$NumericTimeVar(df.dataAct, "CP025Q01.TOT")
df.dataAct$top <- as.numeric(apply(df.dataAct[, 8:11], 1, sum) > 0)
df.dataAct$bot <- as.numeric(apply(df.dataAct[, 12:15], 1, sum) > 0)
df.dataAct$mid <- as.numeric(apply(df.dataAct[, 16:19], 1, sum) > 0)
df.dataAct$votat  <- as.numeric(df.dataAct$top > 0 & df.dataAct$bot > 0 &
                                 df.dataAct$mid > 0)
names(df.dataAct)[1:2] <- c("CNT", "SCHOOLID")
df.complete <- dplyr::left_join(df.dataAct, pisa,
                                by = c("CNT", "SCHOOLID", "StIDStd"))
df.complete <- df.complete[!is.na(df.complete$CP025Q01), ]
df.complete$CP025Q01 <- as.numeric(df.complete$CP025Q01 == 2)
cp025q01.treated <- df.complete
save(cp025q01.treated, file = "data/cp025q01.treated.rda", compress = "xz")