# End-to-end: the numbers shown in the app's tables are the package's
# values (rounded as displayed). Needs a browser (chromote), so it only runs
# locally.

skip_on_cran()
skip_on_ci()
skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")
skip_if(is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
        "Chrome is not available")

# Visible rows of a DT table as a data frame of strings
visible_table <- function(app, id) {
  # DT loads the rows by ajax: wait until they are in the page
  app$wait_for_js(sprintf(
    "document.querySelectorAll('#%s table.dataTable tbody tr td').length > 1 &&
     !document.querySelector('#%s .dataTables_empty')", id, id),
    timeout = 30000)
  js <- sprintf("(() => {
    // With scrollX, DT draws the header and the body in separate tables
    const root = document.getElementById('%s');
    const head = Array.from(root.querySelector('thead').querySelectorAll('th'))
      .map(h => h.textContent.trim());
    const rows = Array.from(root.querySelectorAll('tbody tr'))
      .filter(r => r.querySelectorAll('td').length === head.length)
      .map(r => Array.from(r.querySelectorAll('td')).map(c => c.textContent.trim()));
    return {head: head, rows: rows};
  })()", id)
  x <- app$get_js(js)
  m <- do.call(rbind, lapply(x$rows, unlist))
  df <- as.data.frame(m, stringsAsFactors = FALSE)
  names(df) <- unlist(x$head)
  df
}

# Number as DT's formatRound() shows it ("" for NA)
shown <- function(x, digits) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits,
                               big.mark = ","))
}

test_that("results and detail tables show the package values", {
  app <- shinytest2::AppDriver$new(
    system.file("shiny", "acwr_app", package = "ACWR"),
    name = "values", load_timeout = 60000, timeout = 30000
  )
  withr::defer(app$stop())
  set <- function(...) {
    app$set_inputs(..., wait_ = FALSE)
    app$wait_for_idle(1000, timeout = 60000)
  }

  set(`data-data_source` = "preloaded")
  set(`data-preloaded_choice` = "Equipo con descansos (3 atletas, fechas)")
  set(`data-athlete` = "2")
  set(main_nav = "ACWR")
  set(`acwr-acwr_tabs` = "Tabla de resultados")
  Sys.sleep(1)

  # Package output on the raw sessions of athlete 2, one row per date
  db <- training_load_rest[training_load_rest$ID == 2, ]
  ref <- ACWR(db, "ID", "TL", training_dates = "Training_Date")
  ref <- ref[!duplicated(ref$Training_Date), ]
  ref_date <- format(as.Date(ref$Training_Date))

  tab <- visible_table(app, "acwr-results_table")
  expect_gt(nrow(tab), 20)
  i <- match(tab$Fecha, ref_date)
  on_session <- !is.na(i)
  expect_gt(sum(on_session), 10)
  expect_equal(tab$ACWR_COUPLED[on_session], shown(ref$RAC_ACWR[i[on_session]], 2))
  expect_equal(tab$ACWR_UNCOUPLED[on_session], shown(ref$RAU_ACWR[i[on_session]], 2))
  expect_equal(tab$ACWR_EWMA[on_session], shown(ref$EWMA_ACWR[i[on_session]], 2))
  # Rest days are in the table, with load 0
  expect_true(any(!on_session))
  expect_true(all(tab$Load[!on_session] == "0.00"))

  # Detailed calculation, uncoupled method (4 decimals)
  set(`acwr-acwr_tabs` = "C\u00e1lculo Detallado")
  set(`acwr-detail_method` = "uncoupled")
  Sys.sleep(1)
  det <- visible_table(app, "acwr-detail_table")
  j <- match(det$Fecha, ref_date)
  ok <- !is.na(j)
  expect_gt(sum(ok), 10)
  expect_equal(det$ACWR[ok], shown(ref$RAU_ACWR[j[ok]], 4))
})
