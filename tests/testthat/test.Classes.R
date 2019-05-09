context("Function outputs are as expected")

# Data preparation
df <- cp025q01
df$id <- paste(df[, 1], df[, 2], df[, 3], sep = "-")
trim_events <- c("event", "event_type", "diag_state")
quo_events <- c(rlang::quo(event), rlang::quo(event_type))
clean_events <- c("ACER_EVENT_" = "")

# Function applications
df_trim <- m0$TrimVar(df, trim_events)
df_conc <- m0$ConcatActions(df_trim, quo_events)
df_clean <- m0$CleanActions(df_conc, event_type, clean_events)
df_act <- m0$DataActionsbyID(df_clean, id, new.event.type, "actions")
descr <- m2$DescriptiveStrategy(cp025q01.treated, "votat",
                                "CP025Q01", "PV1CPRO", print = FALSE)
test_that("Classes are as expected", {
    expect_s3_class(df_trim,  "data.frame")
    expect_s3_class(df_conc,  "data.frame")
    expect_s3_class(df_clean, "data.frame")
    expect_s3_class(df_act,   "data.frame")
    expect_s3_class(descr, "list")
})