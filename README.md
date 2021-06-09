![](https://www.r-pkg.org/badges/version/LOGAN)
![](https://cranlogs.r-pkg.org/badges/grand-total/LOGAN)

# LOGAN

An R package for log file analysis. It can be used as a standalone R package or as a backend for the accompanying Shiny application.

# Licence

In the spirit of the [open science](https://openscience.com) movement, LOGAN is free and open source software, licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html). Contributions to the package are welcome, and we recommend you contact the package creator if you're interested in it. Please read our style and git guidelines if you intend to contribute with code; the document is available in the package's `inst` directory.

# Installation

In order to use LOGAN, you can install it directly in R. An illustration of the analyses can be found on the [Shiny app](https://loganpackage.shinyapps.io/shiny/) on your browser.

## Official release

The latest official, stable version of LOGAN is available on [CRAN](https://cran.r-project.org) and can be installed like any other R package, i.e., by issuing the following in your R prompt:

```r
install.packages("LOGAN")
```

## Development and past versions

[LOGAN's GitHub repository](https://github.com/derecost/LOGAN/) is the central hub for development of the package. Previous and test versions of LOGAN can be built from there, but they are to be considered unstable and are not guaranteed to work properly. The development team has several coding standards in place to avoid issues at any branch, especially the master one, but unless you know what you are doing, we recommend you stick with the CRAN version of our package.

To install LOGAN from GitHub, first make sure you have an up-to-date version of the `devtools` package installed in your machine. Then, run the following command in you R terminal:

```r
devtools::install_github("derecost/LOGAN")
```

Alternatively, package releases can be downloaded as compressed files from https://github.com/derecost/LOGAN/releases. These files can be installed in R using the `install.packages()` command with the downloaded file path as argument.

LOGAN is currently under active development, so it is always a good idea to check which version you have just installed. To do so, run `packageVersion("LOGAN")` in R.

# Usage

After installing LOGAN, either through CRAN or GitHub, the package can be used after issuing `library(LOGAN)` in R.

LOGAN is organized in modules, so each function must be called as a subset of
their modules, e.g. `m0$CleanActions()` instead of `CleanActions()`. However, 
calls to function documentation and examples must be made directly, e.g.
`?CleanActions` and `example(CleanActions)`.

# Planned features

- Improved documentation (e.g. wiki, vignettes)
- Speed improvements (change loops into calls to "*apply", remove unnecessary statements, etc.)
- Reduction in the number of package dependencies
- Improved validation of function input

# Reporting bugs and requesting features

Bugs should be reported by creating a new issue on https://github.com/derecost/LOGAN/issues. Please send feature requests by e-mail to the package maintainer. If you would rather implement those changes yourself, please issue a [pull request](https://github.com/derecost/LOGAN/pulls) when you are ready to have us review your contribution.
