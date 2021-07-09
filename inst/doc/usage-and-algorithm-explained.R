## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = FALSE
)

## ----setup--------------------------------------------------------------------
library(dedupewider)
library(kableExtra)
library(magrittr)
library(data.table)

## ----display-table------------------------------------------------------------
display_tbl <- function(x) {
  kbl(x) %>% 
  kable_paper(full_width = FALSE)
}

## -----------------------------------------------------------------------------
example_table <- data.frame(Tel_1 = c(111, 222, 444, 555),
                            Tel_2 = c(222, 666, 666, 555),
                            Name = paste0("name", 1:4))

display_tbl(example_table)

## ----initial-table-usage------------------------------------------------------
initial_table <- data.frame(tel_1 = c(111, 222, 444, 555),
                            tel_2 = c(222, 666, 666, 555),
                            tel_3 = c(NA, NA, NA, 555),
                            tel_4 = c(NA, NA, NA, 555),
                            tel_5 = c(NA, NA, NA, 555),
                            name = paste0("name", 1:4),
                            nace = c("01.19", "01.64", "55.90", "09.10"))
display_tbl(initial_table)

## ----usage-default, echo=TRUE, results='hide'---------------------------------
library(dedupewider)

initial_table <- data.frame(tel_1 = c(111, 222, 444, 555),
                            tel_2 = c(222, 666, 666, 555),
                            tel_3 = c(NA, NA, NA, 555),
                            tel_4 = c(NA, NA, NA, 555),
                            tel_5 = c(NA, NA, NA, 555),
                            name = paste0("name", 1:4),
                            nace = c("01.19", "01.64", "55.90", "09.10"))

table_deduplicated <- dedupe_wide(initial_table, cols_dedupe = paste0("tel_", 1:5),
                                  cols_expand = "name")
table_deduplicated

## ----usage-default-display----------------------------------------------------
display_tbl(table_deduplicated)

## ----usage-multiple-expand, echo=TRUE, results='hide'-------------------------
table_deduplicated <- dedupe_wide(initial_table, cols_dedupe = paste0("tel_", 1:5),
                                  cols_expand = c("name", "nace"))
table_deduplicated


## ----usage-multiple-expand-display--------------------------------------------
display_tbl(table_deduplicated)

## ----usage-max-new-cols, echo=TRUE, results='hide'----------------------------
table_deduplicated <- dedupe_wide(initial_table, cols_dedupe = paste0("tel_", 1:5),
                                  cols_expand = c("name", "nace"),
                                  max_new_cols = 2)
table_deduplicated

## ----usage-max-new-cols-display-----------------------------------------------
display_tbl(table_deduplicated)

## ----duplicated-unique-na, echo=TRUE, results='hide'--------------------------
duplicated(c(NA, NA)) # returns FALSE TRUE
unique(c(NA, NA)) # returns NA (vector of length 1)

## ----dedupe_wide-na-initial-table---------------------------------------------
table_na <- data.frame(tel_1 = c(111, 222, NA, NA),
                tel_2 = c(222, 111, NA, NA),
                name = paste0("name", 5:8))
display_tbl(table_na)

## ----dedupe_wide-na-----------------------------------------------------------
table_na_dedupe <- dedupe_wide(table_na, cols_dedupe = c("tel_1", "tel_2"), cols_expand = "name")
display_tbl(table_na_dedupe)

## ----usage-parallel, echo=TRUE, results='hide'--------------------------------
data.table::setDTthreads()
dedupe_wide(initial_table, cols_dedupe = paste0("tel_", 1:5),
                                  cols_expand = "name")

## ----initial-table-algorithm--------------------------------------------------
initial_table <- data.frame(tel_1 = c(111, 222, 333, 444, 333, 333,
                                      666, 777, 333, 333, 333, 333),
                            tel_2 = c(777, 666, NA, 666, NA, NA, NA, NA, NA, 777,
                                      888, 777),
                            tel_3 = c(999, NA, NA, 333, NA, NA, NA, NA, NA, 101, 102,
                                      777),
                            tel_4 = c(102, NA, NA, NA, NA, NA, NA, NA, NA, 103, NA, 103),
                            name = paste0("name", 1:12))
display_tbl(initial_table)

## ----gather-cols--------------------------------------------------------------
x <- data.frame(tel_1 = c(111, 222, 333, 444, 333, 333,
                                      666, 777, 333, 333, 333, 333),
                            tel_2 = c(777, 666, NA, 666, NA, NA, NA, NA, NA, 777,
                                      888, 777),
                            tel_3 = c(999, NA, NA, 333, NA, NA, NA, NA, NA, 101, 102,
                                      777),
                            tel_4 = c(102, NA, NA, NA, NA, NA, NA, NA, NA, 103, NA, 103),
                            name = paste0("name", 1:12))
cols_dedupe <- paste0("tel_", 1:4)
setDT(x)
x[, ....idx := .I]
x_tmp <- suppressWarnings(melt.data.table(x, id.vars = "....idx", measure.vars = cols_dedupe, na.rm = TRUE))
x_tmp[, variable := NULL]
display_tbl(head(x_tmp))

## ----group-by-value-----------------------------------------------------------
x_tmp[, V1 := list(list(....idx)), by = "value"][, ....idx := NULL]
x_tmp <- unique(x_tmp, by = "value")
x_tmp[, `:=`(filter_col = lengths(V1),
                 value = NULL)]
x_tmp <- x_tmp[filter_col > 1L]
x_tmp[, V1 := lapply(V1, unique)][, `:=`(main_index = unlist(lapply(V1, min), use.names = FALSE),
                                                    filter_col = lengths(V1))]
display_tbl(x_tmp[, c("V1", "main_index")])

## ----make-pairs-indexes-------------------------------------------------------
indexes_more_than_one_occurrence <- x_tmp
indexes_more_than_one_occurrence_vector <- indexes_more_than_one_occurrence[, list(indexes = unique(sort(unlist(V1, use.names = FALSE), decreasing = TRUE)))][["indexes"]]
indexes_more_than_one_occurrence[, V1 := lapply(V1, function(x) x[-which.min(x)])]
indexes_more_than_one_occurrence <- indexes_more_than_one_occurrence[, list(rest_indexes = unlist(V1, use.names = FALSE)), by = main_index]
setorder(indexes_more_than_one_occurrence, -rest_indexes, -main_index)
indexes_more_than_one_occurrence <- unique(indexes_more_than_one_occurrence)
display_tbl(indexes_more_than_one_occurrence)

## ----example-12---------------------------------------------------------------
display_tbl(data.frame(main_index = c(10, 3, 1),
                       rest_indexes = c(12, 10, 3)))

## -----------------------------------------------------------------------------
cascade_indexes <- function(indexes_more_than_one_occurrence_vector, indexes_more_than_one_occurrence) {
  rest_indexes <- main_index <- filter <- NULL
  for (i in indexes_more_than_one_occurrence_vector) {
    which <- which(indexes_more_than_one_occurrence[["rest_indexes"]] == i)
    value <- shift(indexes_more_than_one_occurrence[which][["main_index"]], fill = i)
    set(indexes_more_than_one_occurrence, which, "rest_indexes", value)
    setorder(indexes_more_than_one_occurrence, -rest_indexes, -main_index)
  }
  indexes_more_than_one_occurrence[, filter := fifelse(main_index == rest_indexes, FALSE, TRUE)]
  indexes_more_than_one_occurrence <- indexes_more_than_one_occurrence[filter == TRUE][, filter := NULL]
  indexes_more_than_one_occurrence <- unique(indexes_more_than_one_occurrence, by = "rest_indexes")
  indexes_more_than_one_occurrence
}

indexes_more_than_one_occurrence <- cascade_indexes(indexes_more_than_one_occurrence_vector, indexes_more_than_one_occurrence)
display_tbl(indexes_more_than_one_occurrence)

