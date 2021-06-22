context("RLinuxModules command")

modulesHome <- file.path(getwd(), "mock")
empty_env <- c(MODULE_VERSION = NA,
               MODULEPATH     = NA,
               LOADEDMODULES  = NA,
               MODULEHOME     = NA,
               'BASH_FUNC_module%%' = NA,
               'BASH_FUNC_module()' = NA)

test_that("module commands", {
  withr::with_envvar(empty_env,
    {
      moduleInit(modulesHome = modulesHome)
      expect_error(module(), "argument \"Arguments\" is missing, with no default")
      expect_error(module(logical(0)), "Arguments must be a character vector of length 1")
      expect_error(module(numeric(0)), "Arguments must be a character vector of length 1")
      expect_error(module(single(0)), "Arguments must be a character vector of length 1")
      expect_error(module(double(0)), "Arguments must be a character vector of length 1")
      expect_error(module(character(0)), "Arguments must be a character vector of length 1")
      expect_error(module(complex(0)), "Arguments must be a character vector of length 1")
      expect_error(module(x=1), "unused argument")
      expect_error(module(""), "Arguments must be a character vector of non-space")

      ## Test alias ml() => module()
      expect_equal(names(formals("ml")), "...")
      expect_match(deparse(body("ml")), "module\\(\\.\\.\\.\\)")

      module("load samtools")
      expect_equivalent(Sys.getenv("PATH"),
                        "/genome/samtools/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
      expect_equivalent(Sys.getenv("SAMTOOLS_VERSION"), "1.7")

      module("unload samtools")
      expect_equivalent(Sys.getenv("PATH"),
                        "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
      expect_true(is.na(Sys.getenv("SAMTOOLS_VERSION", unset = NA)))

      # list works
      listing <- module("list")
      expect_equivalent(listing, "Currently Loaded Modulefiles:\n 1) R/default")

      # option parsing too
      listing <- module("--no-pager list")
      expect_equivalent(listing, "Currently Loaded Modulefiles:\n 1) R/default")

      # option parsing too with leading space
      listing <- module(" --no-pager list")
      expect_equivalent(listing, "Currently Loaded Modulefiles:\n 1) R/default")

      # help
      listing <- module("-h")
      expect_equivalent(listing, "Modules Release 9.9.9-mock (2035-01-01)\nUsage: module [options] [command] [args ...]")
      listing <- module("--help")
      expect_equivalent(listing, "Modules Release 9.9.9-mock (2035-01-01)\nUsage: module [options] [command] [args ...]")
      })

  # leading space and around commands
  withr::with_envvar(empty_env, {
    moduleInit(modulesHome = modulesHome)

    module(" load samtools")
    expect_equivalent(Sys.getenv("PATH"),
                      "/genome/samtools/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
    expect_equivalent(Sys.getenv("SAMTOOLS_VERSION"), "1.7")

    module("unload  samtools ")
    expect_equivalent(Sys.getenv("PATH"),
                      "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
    expect_true(is.na(Sys.getenv("SAMTOOLS_VERSION", unset = NA)))
  })

  # further space tests - load / unload pairs
  withr::with_envvar(empty_env, {
    moduleInit(modulesHome = modulesHome)
    expect_error(module("            "), "Arguments must be a character vector of non-space")

    module("
           load samtools   ")
    expect_equivalent(Sys.getenv("PATH"),
                      "/genome/samtools/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
    expect_equivalent(Sys.getenv("SAMTOOLS_VERSION"), "1.7")

    module("   unload                                 samtools ")
    expect_equivalent(Sys.getenv("PATH"),
                      "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
    expect_true(is.na(Sys.getenv("SAMTOOLS_VERSION", unset = NA)))
  })

  # system2 stdout and stderr
  withr::with_envvar(empty_env, {
    moduleInit(modulesHome = modulesHome)

    module("load samtools")
    expect_equivalent(Sys.getenv("PATH"),
                      "/genome/samtools/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
    expect_equivalent(Sys.getenv("SAMTOOLS_VERSION"), "1.7")

    stderr_captured <- module("purge")
    expect_equivalent(stderr_captured, "purged")
    expect_equivalent(Sys.getenv("PATH"),
                      "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin")
    expect_true(is.na(Sys.getenv("SAMTOOLS_VERSION", unset = NA)))
  })

  withr::with_envvar(empty_env, {
    moduleInit(modulesHome = modulesHome)

    withr::with_options(c(rlinuxmodules.filter.cat = TRUE), {
      captured <- module("list")
      expect_equivalent(captured, "Currently Loaded Modulefiles:\n 1) R/default")
    })

  })
})

test_that("module commands in system", {
  skip_if_not(shell_has_bash(), message = "shell is not bash")
  withr::with_envvar(empty_env,
    {
      moduleInit(modulesHome = modulesHome)
      expect_equal(system("bash -c 'type -t module'", intern = TRUE), "function",
                   label = "'module' is a function in shell")

      expect_equivalent(system("bash -c 'module load samtools; echo $PATH'", intern = TRUE),
                        "/genome/samtools/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin",
                        label = "PATH variable")
    })
})
