.BasicClasses = c(
  "String"  = "character",
  "Factor"  = "factor",
  "Integer" = "integer",
  "Numeric" = "numeric",
  "Complex" = "complex",
  "Raw"     = "raw",
  "Boolean" = "logical"
)

.getBasicClasses <- function(...) {
  out <- tryCatch(
    {
      return(unlist(lapply(list(...), function(class_name) { .BasicClasses[[class_name]] })))
    },
    error = function(cond) {
      return(NA)
    },
    finally = {
    }
  )
  return(out)
}

.CategoricalClasses <- .getBasicClasses("String", "Factor")
.ContinuousClasses <- .getBasicClasses("Integer", "Numeric", "Complex", "Raw")

.deepANNClasses <-
  c("marray"                       = "marray",
    "Tensor"                       = "tensor",
    "Feedforward Network"          = "mlp",
    "Long Short-Term Memory"       = "lstm",
    "Convolutional Neural Network" = "cnn",
    "Naive Bayes"                  = "naivebayes")

.ProbabilityDistribution <-
  c("Categorical" = "categorical",
    "Gaussian"    = "gaussian")
