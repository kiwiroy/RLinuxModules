.rlinuxmodules       <- list()
.rlinuxmodules.names <- c(
  "LOADEDMODULES",
  "LOADEDMODULES_modshare",
  "MODULEPATH",
  "MODULEPATH_modshare",
  "MODULESHOME",
  "MODULES_CMD",
  "MODULES_COLORS",
  "MODULES_LMCONFLICT",
  "MODULES_LMCONFLICT_modshare",
  "MODULES_LMPREREQ",
  "MODULES_LMPREREQ_modshare",
  "MODULES_LMTAG",
  "MODULES_LMTAG_modshare",
  "MODULES_USE_COMPAT_VERSION",
  "MODULE_VERSION",
  "MODULE_VERSION_STACK",
  "_LMFILES_",
  "_LMFILES__modshare"
)

.onLoad <- function(libname, pkgname) {
  .rlinuxmodules <<-
    as.list(Sys.getenv(x = .rlinuxmodules.names, unset = NA))
  op <- options()
  op.rlinuxmodules <- list(rlinuxmodules.onunload.reset = TRUE, rlinuxmodules.filter.cat = FALSE)
  toset <- !(names(op.rlinuxmodules) %in% names(op))
  if (any(toset))
    options(op.rlinuxmodules[toset])

  invisible()
}

.onUnload <- function(libpath) {
  if (res <- getOption("rlinuxmodules.onunload.reset", TRUE)) {
    res <- .rlinuxmodules[which(!is.na(.rlinuxmodules))]
    if (length(res) > 0) {
      do.call(what = Sys.setenv, args = res)
    }
    if (length(.rlinuxmodules[which(is.na(.rlinuxmodules))]) > 0) {
      do.call(what = Sys.unsetenv, args = list(names(.rlinuxmodules[which(is.na(.rlinuxmodules))])))
    }
  }

  invisible()
}
