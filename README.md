# brandbuilder

A leprechaun-style Shiny app-package for building Bootswatch and `_brand.yml` combinations.

## Run during development

```r
install.packages(c("devtools", "leprechaun", "shiny", "bslib", "htmltools", "yaml", "pkgload"))
devtools::load_all()
run_app()
```

Leprechaun is a development-time code generator, not a runtime dependency. The app follows its package structure with `R/ui.R`, `R/server.R`, `R/run.R`, packaged assets under `inst/assets`, and a deployment entry point under `inst/run/app.R`.

```r
devtools::document()
devtools::test()
devtools::check()
```
