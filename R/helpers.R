#' Wrap string by other strings
#' @param vector A character vector. Elements to be wrapped.
#' @param left A string. String pasted to the left of each element.
#' @param right A string. String pasted to the right of each vector.
#' @export
#' @example
#' se  <- round(runif(3), 3)
#' wrap_str(se)
wrap_str <- function(vector, left = "(", right = ")"){
  sapply(vector, function(v)
    ifelse(is.na(v) || v == "", "", paste0(left, v , right)))
}


#' Look for string pattern in files and print file name on consule
#'
#' @param pattern A string. What to lookup. A regex pattern or exact string (by setting \code{value = TRUE}, for example).
#' @param file_pattern A regex. Pattern used in \code{list.files()}. By default it is set to search files ending in `.R` (R script)
#' @param dir A string. Directory to search within.
#' @param full_path Logical. Whether to print full path of file.
#' @param print_lines Logical. By default prints file name in which \code{pattern} was found.
#' @param ... Options passed onto \code{grepl()}.
#' @return NULL
#' @export
look_in_files <- function(pattern, file_pattern = ".R$", dir = "",
                          full_path = FALSE, print_lines = FALSE, ...){
  files <- list.files(dir, pattern = file_pattern, full.names = full_path)
  found <- c()
  lines <- c()
  for(file in files){
    fname <- paste0(dir, "/", file)
    script <- readLines(file(fname))
    where <- grepl(pattern, script, ...)
    found[file] <- any(where)
    if(found[file]) {
      ifelse(print_lines,
             cat(paste0("`", pattern, "' found in ", file, " in lines ", paste(which(where), collapse = ", "))),
             cat(paste0("`", pattern, "' found in ", file)))
    }
    close(file(fname))
  }
  if(!any(found)) cat(paste0("`", pattern, "' not found in any files."))
}
