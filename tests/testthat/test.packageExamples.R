# context("Examples from LOGAN 0.0.0 yield the expected results")
# # TODO: update expected results with actual data
#
# test_that("CleanActions yields no errors", {
#     expect_output(m2$DescriptiveStrategy(data = cp025q01.complete,
#                                       strategy.var = "VOTAT1",
#                                       performance.item = "CP025Q01",
#                                       performance.test = "PV1CPRO",
#                                       PartialCredit = FALSE))
#
#     # TODO: Fix outputs from the functions below. Tests should not be expecting
#     # errors! WTF are "interactive.actions"?
#     expect_error(m1$RangeNumberActionsbyVar(data = cp025q01.data,
#                                          id.var = rlang::quo(NewID),
#                                          var.group = rlang::quo(cnt)))
#     expect_error(m0$ConcatActions(cp025q01.data, concat.events))
#     expect_error(m2$VarActionPosition(data = cm015q01.TimeActions,
#                                    action.var = "ITEM.ACTIONS",
#                                    actions.search = interactive.actions))
#     expect_error(m2$VarActionSearch(data = cm015q01.TimeActions,
#                                  action.var = "ITEM.ACTIONS",
#                                  actions.search = interactive.actions))
# })
