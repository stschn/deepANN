eval <- NULL

.onLoad <- function(libname, pkgname) {
  if (requireNamespace("rlang", quietly = TRUE))
    eval <<- rlang::eval_bare
  else
    eval <<- base::eval
}
