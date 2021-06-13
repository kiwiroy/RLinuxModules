#' Initialize Environment Modules interface
#'
#' Initialize linux Environment Modules. Must be called before using \code{\link{module}}
#'
#' @param version The version of the module system that is installed
#' @param modulesHome Path to where the module system is installed
#'
#'
#' @export
moduleInit <- function( version = '3.2.10',
                        modulesHome = '/local/genome/Modules/3.2.10'){

  if (getOption("rlinuxmodules.use_lmod", FALSE) == TRUE) {
    lmod_init(version, modulesHome)
  } else {
    # Check if modulecmd exists in the bin directory of modulesHome
    module_cmd <- file.path(modulesHome, "bin", "modulecmd")
    if(!file.exists(module_cmd)){
      stop(module_cmd, " missing!\n",
           "  Module environment init failed!" )
    }

    if(compareVersion("3.2.10", version) >= 0) {
      # version 3x
      version_three_modules(version, modulesHome)
    } else {
      version_four_modules(version, modulesHome)
    }
  }
}

maybe_load_shell_function <- function (modulesCommand) {
  if( shell_has_bash() ) {
    module_function <- bash_func_name()
    if( is.na(Sys.getenv(module_function, unset = NA)) ) {
      env_var <- list(f = paste("() {  eval `", modulesCommand, " bash ${1+\"$@\"}`; }",
                                sep = ""))
      names(env_var) <- module_function
      do.call(Sys.setenv, env_var)
    }
  }
}

lmod_init <- function (version, modulesHome) {
  lmod_dir <- file.path(modulesHome, "libexec")
  lmod_cmd <- file.path(lmod_dir, "lmod")
  if(!file.exists(lmod_cmd)){
    stop(lmod_cmd, " missing!\n",
         "  Module environment init failed!" )
  }

  Sys.setenv(
    LMOD_CMD     = lmod_cmd,
    LMOD_DIR     = lmod_dir,
    LMOD_VERSION = version,
    MODULESHOME  = modulesHome
  )
  invisible()
}

shell_has_bash <- function() {
  ## $SHELL is login shell bash - set by all shells
  ## $0 is process name
  ## $BASH is set by bash even when sh is symlinked to bash (/bin/sh rather than /bin/bash)
  ## This is not enough, dash is sanitizing https://stackoverflow.com/a/38125775
  bash <-
    system("{ which bash >/dev/null && bash -c 'basename ${SHELL:-unset}; basename $0; basename ${BASH:-unset}'; } || echo unset",
           intern = TRUE)
  any(bash == "bash") & any(bash == "unset") == FALSE
}

# probe the system to discover naming scheme BASH_FUNC_module%% vs BASH_FUNC_module()
# Section 1.3 https://dwheeler.com/essays/shellshock.html (https://archive.is/odZ4v)
bash_func_name <- function() {
  ## system() uses sh which is often not linked/copy of bash, but another shell e.g. dash
  name_scheme <- system("bash -c '__r() { : ;}; export -f __r; env | grep ^BASH_FUNC___r'",
                        intern = TRUE)
  name_scheme <- gsub(pattern = "(BASH_FUNC___r|=.*)", replacement = "", x = name_scheme)
  func_name   <- paste("BASH_FUNC_module", name_scheme, sep = "")
  func_name
}


version_four_modules <- function (version, modulesHome) {
  if( is.na(Sys.getenv('MODULE_VERSION', unset = NA)) ){
    Sys.setenv(MODULE_VERSION_STACK = version,
               MODULE_VERSION = version)
  } else {
    Sys.setenv(MODULE_VERSION_STACK = Sys.getenv('MODULE_VERSION'))
  }

  Sys.setenv(MODULESHOME=modulesHome)

  if( is.na(Sys.getenv('MODULEPATH', unset = NA)) ){
    dot_modulespath <- file.path(modulesHome, "init", ".modulespath")
    path <- ""
    if (file.exists(dot_modulespath)) {
      txt <- readLines(con = dot_modulespath)

      # remove commented lines and trim leading and trailing whitespace
      txt <- gsub("^\\s+|\\s+$", "", sub("#.*$","", txt))
      # paste together
      path <- paste(txt[txt != ""],collapse=":")
    } else {
      # use default
      path <- file.path(modulesHome, 'modulefiles')
    }
    Sys.setenv( MODULEPATH = path )
  }

  if( is.na(Sys.getenv('LOADEDMODULES', unset = NA)) ){
    Sys.setenv( LOADEDMODULES = "" )
  }
  Sys.setenv(MODULES_COLORS = "")
  # for {r, engine="bash"} in Rmarkdown/other bash subprocesses to use 'module' function
  maybe_load_shell_function(file.path(modulesHome, "bin/modulecmd"))
}

version_three_modules <- function (version, modulesHome) {
  if( is.na(Sys.getenv('MODULE_VERSION', unset = NA)) ){
    Sys.setenv(MODULE_VERSION_STACK = version,
               MODULE_VERSION = version)
  } else {
    Sys.setenv(MODULE_VERSION_STACK = Sys.getenv('MODULE_VERSION'))
  }

  Sys.setenv(MODULESHOME=modulesHome)

  if( is.na(Sys.getenv('MODULEPATH', unset = NA)) ){
    txt <- readLines(file.path(Sys.getenv('MODULESHOME'),"init/.modulespath"))

    # remove commented lines and trim leading and trailing whitespace
    txt <- gsub("^\\s+|\\s+$", "", sub("#.*$","",txt))
    # paste together
    path <- paste(txt[txt != ""],collapse=":")
    Sys.setenv( MODULEPATH = path )
  }

  if( is.na(Sys.getenv('LOADEDMODULES', unset = NA)) ){
    Sys.setenv( LOADEDMODULES = "" )
  }
  Sys.setenv(MODULES_COLORS = "")
  # for {r, engine="bash"} in Rmarkdown/other bash subprocesses to use 'module' function
  maybe_load_shell_function(file.path(modulesHome, "bin/modulecmd"))
}
