#!/usr/bin/env Rscript

args <- commandArgs(FALSE)
file_arg <- args[grepl("^--file=", args)]
script_file <- if (length(file_arg)) {
  sub("^--file=", "", file_arg[[1L]])
} else {
  "dev/check-publication-readiness.R"
}
root <- normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE)
if (!file.exists(file.path(root, "DESCRIPTION"))) {
  root <- normalizePath(getwd(), mustWork = FALSE)
}
setwd(root)

status <- list()
run_step <- function(name, expr) {
  cat("\n==>", name, "\n")
  started <- Sys.time()
  result <- tryCatch(
    {
      force(expr)
      TRUE
    },
    error = function(e) {
      message("FAILED: ", conditionMessage(e))
      FALSE
    }
  )
  status[[length(status) + 1L]] <<- data.frame(
    step = name,
    passed = result,
    seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2),
    stringsAsFactors = FALSE
  )
  invisible(result)
}

run_command <- function(command, args = character(), env = character()) {
  out <- system2(command, args = args, env = env)
  if (!identical(out, 0L)) {
    stop(command, " failed with status ", out, call. = FALSE)
  }
}

pandoc_dir <- "/Applications/quarto/bin/tools/aarch64"
path_env <- if (dir.exists(pandoc_dir)) {
  paste0("PATH=", pandoc_dir, .Platform$path.sep, Sys.getenv("PATH"))
} else {
  paste0("PATH=", Sys.getenv("PATH"))
}

run_step("App syntax", {
  invisible(parse("inst/shiny/app.R"))
  invisible(parse("app/app.R"))
})

run_step("Unit tests", {
  testthat::test_local()
})

run_step("Source build", {
  run_command("R", c("CMD", "build", "."), env = path_env)
})

tarball <- list.files(pattern = "^mcsimr_[0-9].*[.]tar[.]gz$")
tarball <- tarball[order(file.info(tarball)$mtime, decreasing = TRUE)][[1L]]

run_step("CRAN-style check", {
  run_command("R", c("CMD", "check", "--as-cran", "--no-manual", tarball), env = path_env)
})

run_step("Spelling", {
  if (requireNamespace("spelling", quietly = TRUE)) {
    words <- spelling::spell_check_package()
    if (NROW(words)) {
      print(words)
      stop("Spelling check found possible issues.", call. = FALSE)
    }
  } else {
    message("spelling is not installed; skipping.")
  }
})

run_step("URLs", {
  if (requireNamespace("urlchecker", quietly = TRUE)) {
    old_path <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = old_path), add = TRUE)
    Sys.setenv(PATH = sub("^PATH=", "", path_env))
    urls <- urlchecker::url_check()
    if (NROW(urls)) {
      print(urls)
      stop("URL check found possible issues.", call. = FALSE)
    }
  } else {
    message("urlchecker is not installed; skipping.")
  }
})

summary <- do.call(rbind, status)
cat("\nPublication readiness summary\n")
print(summary, row.names = FALSE)
if (!all(summary$passed)) {
  quit(status = 1L)
}

cat("\nAll requested publication-readiness checks passed.\n")
