test <- structure(list(m = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 1L),
                                     .Label = c("h", "i"), class = "factor"),
                       n = structure(c(1L, 1L, 2L, 1L, 2L, 2L, 1L, 1L, 1L, 1L),
                                     .Label = c("j", "k"), class = "factor"),
                       p = structure(c(3L, 1L, 3L, 3L, 1L, NA, 4L, NA, 2L, 2L),
                                     .Label = c("A", "B", "C", "D"),
                                     class = "factor"),
                       q = structure(c(4L, 1L, 3L, 2L, 1L, 2L, 2L, 4L, 3L, 1L),
                                     .Label = c("A", "B", "C", "D"),
                                     class = "factor"),
                       w = c(20.2999565302214, 36.8041590078653,
                             13.7695959682599, 5.38440876211972,
                             31.0731246638721, -3.57099712187315,
                             32.0847635703307, 49.8147747609407,
                             22.6512119755884, 45.1290207630026),
                       x = structure(c(4L, 4L, 3L, 1L, 2L, 3L, NA, 4L, 4L, 4L),
                                     .Label = c("a", "b", "c", "d"),
                                     class = "factor"),
                       y = structure(c(1L, 2L, 1L, 1L, 1L, 2L, 1L, 2L, 2L, 2L),
                                     .Label = c("u", "v"), class = "factor"),
                       z = structure(c(10.2801362538247, 36.1270889290522,
                                       27.5510419322898,  1.24615409369401,
                                       15.8048454896255, 29.7147992625034,
                                       11.8445634965342, 28.7079226871819,
                                       12.5149351845833, 14.8212199870413),
                                     class = c("MedIQR", 'numeric'))),
                  row.names = c(NA,  10L), class = "data.frame")



table1(test, c(m, z, y, w, x), 'n')
table1(c(m, z, y, 5, x), 'n', test)
table1(c(m, z, y, 5, x), n, test)
table1(c(m, z, y, 5, x), 2, test)

