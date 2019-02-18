load("data/fullDatasets/cp025q01.complete.rda")
load("data/fullDatasets/cp025q01.data.rda")

cp025q01.complete <- cp025q01.complete[cp025q01.complete$CNT == "NOR", ]
save(cp025q01.complete, file = "data/cp025q01.complete.rda", compress = "xz")

cp025q01.data <- cp025q01.data[cp025q01.data$cnt == "NOR", ]
save(cp025q01.data, file = "data/cp025q01.data.rda", compress = "xz")
