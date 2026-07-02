skip_if_lavaan_runtime_unavailable <- function() {
  skip_if_not_installed("lavaan")
  skip_if_not(
    mcsimr:::lavaan_runtime_available(),
    mcsimr:::lavaan_runtime_error()
  )
}
