context("Function outputs are as expected")

# Data preparation
df <- cp025q01
df$id <- paste(df[, 1], df[, 2], df[, 3], sep = "-")
trim_events <- c("event", "event_type", "diag_state")
quo_events <- c(rlang::quo(event), rlang::quo(event_type))
clean_events <- c("ACER_EVENT_" = "")

# Sequential function calls
df_trim <- m0$TrimVar(df, trim_events)
df_conc <- m0$ConcatActions(df_trim, quo_events)
df_clean <- m0$CleanActions(df_conc, event_type, clean_events)
df_act <- m0$DataActionsbyID(df_clean, id, new.event.type, "actions")
df_var <- m1$VarTimebyID(df_clean, id, time, new.event.type, "START_ITEM", "start")

# Independent function calls
df_treat <- cp025q01.treated

df_treat$categ <- cut(df_treat$PV1CPRO, c(0, 423, 488, 553, 900))
df_dataplot <- df_treat[, c("top", "categ")]
df_dataplot[,1] <- as.factor(df_dataplot[,1])
df_dataplot[,2] <- as.factor(df_dataplot[,2])

descr <- m2$DescriptiveStrategy(df_treat, "votat",
                                "CP025Q01", "PV1CPRO", print = FALSE)
numer <- m1$NumericTimeVar(df_treat, "NC")
range_var <- m0$RangeNumberActionsbyVar(df_treat, NewID, CNT,
                                        save.table =TRUE)
summ <- m1$SummaryTOTbyVar(df_treat, "CP025Q01.TOT", "CP025Q01", FALSE)

tot_var <- m1$TOTVar(df_treat, "CP025Q01.START", "CP025Q01.END", 
                     divBy = 60, tot.var = "CP025Q01.TOT")
var_search <- m2$VarActionSearch(df_treat, "CP025Q01.ACTIONS", "1_apply")

# Checks
test_that("Classes are as expected", {
    expect_s3_class(df_trim,  "data.frame")
    expect_s3_class(df_conc,  "data.frame")
    expect_s3_class(df_clean, "data.frame")
    expect_s3_class(df_act,   "data.frame")
    expect_s3_class(df_var,   "data.frame")
    expect_type(descr, "list")
    expect_s3_class(numer, "data.frame")
    expect_type(numer$NC, "double")
    expect_s3_class(range_var, "data.frame")
    expect_s3_class(summ, "data.frame")
    expect_s3_class(tot_var, "data.frame")
    expect_s3_class(var_search, "data.frame")
    expect_s3_class(m1$PlotTimeonTaskbyVar(cp025q01.treated,
                                            "CP025Q01.TOT", "CP025Q01"), 
                    "ggplot")
    expect_s3_class(m2$PlotStrategybyCatPerformance(df_dataplot, top,
                                                    categ, "Proficiency levels","Percentage"),
                    "ggplot")
})