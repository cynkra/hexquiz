# Taken from https://www.r-bloggers.com/2012/07/validating-email-adresses-in-r/
is_email <- function(x) {
  grepl(
    "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
    as.character(x),
    ignore.case = TRUE
  )
}
