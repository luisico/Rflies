Rflies
======
Survival analysis for flies research.

Install
-------
First clone the repository or download a release from https://github.com/luisico/Rflies/releases and unpack it.

For RStudio:
1. Install RStudio Desktop from ...
2. Open the project `flies.Rproj` in RStudio.
3. Install the following package dependencies if not already present:
   - knitr
   - devtools
   - roxygen2

There are two ways of installing packages in Rstudio:
* Go to `Tools | Install Packages...` and input packages in the box, ie `knitr, devtools, roxygen2`.
* Go to the Console (in the lower-left corner by default), or press `Ctrl+2` and type
``` R
install.packages(c("knitr", "devtools", "roxygen2"))
```
Installing packages is only need when installing RStudio for the first time or upgrading to a new version of Rstudio.

4. Install or upgrade package `flies` from the download by switching to the console (`Ctrl+2`) and typing:
``` R
devtools::install("flies")
```

Licence
-------
Released under the [MIT license](https://opensource.org/licenses/MIT).

Author Information
------------------
Luis Gracia <lgraval [at] gmail.com>:
- GitHub at [luisico](https://github.com/luisico)

Victoria Hewitt
