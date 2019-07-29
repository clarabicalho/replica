#' Format table
#'
#' Wrapper for \code{xtable} with additional formatting options.
#' @param tab
#' @param add.note
#' @param caption
#' @param type
#' @param column.sep.width
#' @param header
#' @param table.placement
#' @param omit.table.layout
#' @param include.colnames
#' @param include.rownames
#' @param comment
#' @param ...
#'
format_table <- function(tab, add.note = "",
                         caption = "", print.sign = FALSE,
                         type = "latex",
                         column.sep.width = "1pt",
                         header = F, table.placement = "htb",
                         omit.table.layout = "n",
                         include.colnames = FALSE,
                         include.rownames = FALSE,
                         comment = FALSE, ...){
  to_print <- capture.output(print(
    xtable(tab, caption = caption, ...),
    type = type,
    column.sep.width = column.sep.width,
    header = header, table.placement = table.placement,
    omit.table.layout = omit.table.layout,
    include.colnames = include.colnames,
    include.rownames = include.rownames,
    comment = comment, ...))

  if(type == "latex"){
    ptext <- ifelse(print.sign, "$^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$.", "")
    notes <- paste("\\begin{flushleft}\\textit{Note:}", add.note, ptext, "\\end{flushleft}")
    if(add.note == "" && !print.sign) notes <- ""
    to_print <- c(to_print[1:(length(to_print)-1)], notes, to_print[length(to_print)])
  }

  to_print
}

write_table <- function(to_export, output_file){
  writeLines(to_export, file(output_file))
  close.connection(file(output_file))
}

render_table <- function(table_file, output_file = "test_table.pdf"){
  Rmd <- gsub("pdf","Rmd", output_file, fixed = TRUE)
  writeLines(paste0("\\input{", table_file, "}"), file(Rmd))
  close.connection(file(Rmd))
  rmarkdown::render(Rmd, output_format = "pdf_document")
  file.remove(Rmd)
  closeAllConnections()
}

# format_output <- function(..., notes = ""){
#   t <- capture.output(stargazer(..., df = FALSE, column.sep.width = "1pt",
#                                 omit = c("Constant", "m"),
#                                 omit.stat = c("adj.rsq", "ser"),
#                                 header = F, table.placement = "htb",
#                                 omit.table.layout = "n"))
#   notes <- paste("\\begin{flushleft}\\textit{Note:}", notes, "$^*$ $p<0.05$; $^{**}$ $p<0.01$; $^{***}$ $p<0.001$. \\end{flushleft}")
#   c(t[1:(length(t)-1)], notes, t[length(t)])
# }