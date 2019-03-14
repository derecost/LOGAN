# LOGAN

An R package for log file analysis. It can be used as a standalone R package or as a backend for the accompanying Shiny application.

# Licence

In the spirit of the [open science](https://openscience.com) movement, LOGAN is licensed under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html). Contributions to the package are welcome, and we recommend you contact the package creator if you're interested in it. Please read our style and git guidelines if you intend to contribute with code; the document is available in the package's `inst` directory.

# Usage

In order to use LOGAN, you can install it directly in R or access the [Shiny app](https://loganpackage.shinyapps.io/shiny/) on your browser. To install the latest version of LOGAN, first make sure you have an up-to-date version of the `devtools` package installed in your machine. Then, run the following command in you R terminal:

```
devtools::install_github("derecost/LOGAN")
```

Afterwards, the package can be used after issuing `library(LOGAN)` in R.

LOGAN is organized in modules, so each function must be called as a subset of
their modules, e.g. `m0$CleanActions()` instead of `CleanActions()`. However, 
calls to function documentation and examples must be made directly, e.g.
`?CleanActions` and `example(CleanActions)`.

# Planned features

- Inclusion in CRAN
- Improved documentation (e.g. wiki, vignettes)
- Speed improvements (change loops into calls to "*apply")
- Reduction of number of package dependencies