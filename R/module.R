#' Linux Environment Modules interface
#'
#' Access the linux Environment Modules which provides for the dynamic modification of a user's
#' environment via modulefiles.
#'
#' Each modulefile contains the information needed to configure the shell for an application.
#' Once the RLinuxModules package is initialized (with \code{\link{moduleInit}}), the environment can
#' be modified on a per-module basis using the module command which interprets modulefiles. Typically
#' modulefiles instruct the module command to alter or set shell environment variables such as PATH,
#' MANPATH, etc. modulefiles may be shared by many users on a system and users may have their own
#' collection to supplement or replace the shared modulefiles.
#'
#' @param Arguments "[ switches ] [ subcommand ] [ subcommand-args ]" See examples.
#' @return Output messages will be sent to stderr
#'
#' @examples
#' \dontrun{
#' module("avail") # shows available modules
#'
#' module("--help") # show available sub-commands and switches
#'
#' module("load samtools") # loads the module "samtools"
#' system("which samtools") # check that samtools is loaded in the environment
#'
#' module("list") # list loaded modules
#'
#' module("unload samtools") # unload the samtools module
#'
#' module("load samtools/1.0") # loads a specific version of the module "samtools"
#' system("which samtools") # check that the correct samtools is loaded in the environment
#' }
#' @aliases ml
#' @export
module <- function( Arguments ){

  # check if arguments are corrext type
  if( !(class(Arguments) == "character" && length(Arguments)==1)){
    stop("Arguments must be a character vector of length 1")
  }

  if(!grepl(pattern = "\\w+", x = Arguments[1])) {
    stop("Arguments must be a character vector of non-space")
  }

  # check if module environment has been initialized
  if( is.na(Sys.getenv('MODULESHOME', unset = NA)) ){
    stop("Environment variable MODULESHOME missing!\n",
         "  Run moduleInit() to initialize module envionment" )
  }

  if (getOption("rlinuxmodules.use_lmod", FALSE) == TRUE) {
    invisible(lmod_module(moduleCmd, Arguments))
  } else {
    moduleCmd <- file.path(Sys.getenv('MODULESHOME'), "bin/modulecmd")
    # check if modulecmd exists
    if (!file.exists(moduleCmd)) {
      stop(moduleCmd,
           " missing!\n",
           "  Module environment not properly set up!")
    }
    moduleVersion <- Sys.getenv("MODULE_VERSION", unset = NA)
    if(compareVersion("3.2.10", moduleVersion) >= 0) {
      # 3.2.10
      # TODO: actually be backwards compatible
      invisible(tcl_four_module(moduleCmd, Arguments))
    } else {
      invisible(tcl_four_module(moduleCmd, Arguments))
    }
  }
}

lmod_module <- function (moduleCmd, Arguments) {
  # TODO: check this implementation - cribbed from https://github.com/TACC/Lmod/blob/master/init/R.in
  cmd <- paste(moduleCmd, "r", Arguments, sep = ' ')

  hndl <- pipe(cmd)
  eval(expr = parse(file = hndl))
  close(hndl)

  invisible(0)
}

tcl_three_module <- function (moduleCmd, Arguments) {
  # use the python interface
  pythonCmds <- system(paste(moduleCmd,"python",Arguments),intern=T)


  # Check if all python commands are recognizable
  validPythonCmd <- grepl("os\\.chdir\\('([^']*)'\\)",pythonCmds) |
    grepl("os\\.environ\\['([^']*)'] = '([^']*)'",pythonCmds) |
    grepl("del os\\.environ\\['([^']*)'\\]",pythonCmds)
  if( !all(validPythonCmd) ){
    stop("modulecmd returned unknown command(s):\n", paste(pythonCmds[!validPythonCmd],collapse = "\n"))
  }

  # convert python commands to R commands
  RCmds <- sub("os\\.chdir\\('([^']*)'\\)","setwd(dir = '\\1')",pythonCmds,perl=T)
  RCmds <- sub("os\\.environ\\['([^']*)'] = '([^']*)'","Sys.setenv('\\1' = '\\2')",RCmds,perl=T)
  RCmds <- sub("del os\\.environ\\['([^']*)'\\]","Sys.unsetenv('\\1')",RCmds,perl=T)

  # execute R commands
  invisible( eval( parse(text = RCmds) ) )
}

tcl_four_module <- function (moduleCmd, Arguments) {
  # determine subcommand
  args              <- gsub(pattern = "(\\-+[^\\s]+\\s|^\\s+)", replacement = "", x = Arguments[1], perl = TRUE)
  moduleoperation   <- regmatches(x = args, regexpr("^([^\\s]+)", args, perl = TRUE))
  cmds_needing_eval <- c("add", "load", "rm", "unload", "purge", "reload", "switch", "swap", "use", "unuse")

  # use the shiny interface
  rCmds <- system2(moduleCmd, args = c("r", trimws(Arguments, which = "both")), stdout = TRUE, stderr = TRUE, timeout = 10)

  # execute R commands
  mlstatus <- FALSE
  if (any(match(x = moduleoperation, table = cmds_needing_eval), na.rm = TRUE)) {
    moduleCode <- grepl(pattern = "^(Sys\\.|mlstatus)", x = rCmds)
    invisible( eval( parse(text = rCmds[moduleCode]) ) )
    if(any(moduleCode == FALSE)) {
      return(paste(rCmds[-which(moduleCode)], collapse = "\n"))
    }
  } else {
    return(paste(rCmds, collapse = "\n"))
  }

  if (length(rCmds) & !mlstatus){ stop("modulecmd was not successful, mlstatus != TRUE") }
  invisible(mlstatus)
}

#' @export
ml <- function(...) module(...)
