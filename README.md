# openVA
[![R-CMD-check](https://github.com/verbal-autopsy-software/openVA/workflows/R-CMD-check/badge.svg)](https://github.com/verbal-autopsy-software/openVA/actions) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/openVA)](https://cran.r-project.org/package=openVA)  [![](https://cranlogs.r-pkg.org/badges/openVA)](https://cran.r-project.org/package=openVA) [![](https://cranlogs.r-pkg.org/badges/grand-total/openVA?color=orange)](https://cran.r-project.org/package=openVA)
[![codecov](https://codecov.io/gh/verbal-autopsy-software/openVA/branch/master/graph/badge.svg?token=P049IXFSVH)](https://app.codecov.io/gh/verbal-autopsy-software/openVA)

The openVA package implements multiple existing open-source algorithms for coding cause of death from verbal autopsies. It also provides tools for data manipulation tasks commonly used in Verbal Autopsy analysis and implements easy graphical visualization of individual and population level statistics. 
 
The VA methods implemented in the package include:
+ InterVA4 by [Byass et al (2012)](https://doi.org/10.3402/gha.v5i0.19281), implemented using the R package [InterVA4](https://CRAN.R-project.org/package=InterVA4)
+ InterVA5 by [Byass at al (2019)](https://doi.org/10.1186/s12916-019-1333-6), implemented using the R package [InterVA5](https://CRAN.R-project.org/package=InterVA5)
+ InSilicoVA by [McCormick et al (2016)](https://doi.org/10.1080/01621459.2016.1152191), implemented using the R package [InSilicoVA](https://CRAN.R-project.org/package=InSilicoVA)
+ NBC by [Miasnikof et al (2015)](https://doi.org/10.1186/s12916-015-0521-2), implemented by the R package [nbc4va](https://github.com/rrwen/nbc4va) on Github.  
+ A replication of Tariff method implemented using the R package [Tariff](https://CRAN.R-project.org/package=Tariff). The Tariff method is described in [James et al (2011)](https://doi.org/10.1186/1478-7954-9-31) and [Serina, et al. (2015)](https://doi.org/10.1186/s12916-015-0527-9). Please note this package was not developed by authors affiliated with the Institute for Health Metrics and Evaluation (IHME) and thus unintentional discrepancies may exist in the implementation of the Tariff method. It also does not fully replicate the full implementations of the SmartVA-Analyze application distributed by IHME.


For more information, check out the package and team website for more details and documentations: [openVA.net](https://openva.net/).


## Examples

To get started using the openVA package, try the following example running InSilicoVA on a small synthetic WHO2016 dataset

```
library(openVA)
data(RandomVA5)
fit <- codeVA(RandomVA5, data.type = "WHO2016", model = "InSilicoVA",
                    Nsim = 1000, auto.length = FALSE)
summary(fit)
plotVA(fit)
```

For more examples, see the [package vignette](inst/doc/openVA-vignette.html).


## Installation instructions

This guide presents an overview for installing **openVA** package, with some common error reports and solutions at the end.

### 1. Overview of openVA package structure
Whenever the **openVA** package is loaded to R, it also requires four other core packages on CRAN for each of the VA coding methods, namely, **InSilicoVA**, **InterVA4**, **Tariff**, and **nbc4va**, and **ggplot2** for visualization. Additionally, the **InSilicoVA** package further requires the dependency of **coda** and **rJava** for its computation. 

Users typically need to take no specific action regarding the dependencies, since R takes care of them automatically. However, sometimes issues with installing **openVA** package can arise because of some of the dependencies, which may require additional configurations and re-installing the package. I find that most of the times, the errors stem from loading **rJava**, and thus is the main focus of this guide.

### 2. Pre-requisites
As the name suggests, to properly load the package **rJava**, you will need two key ingredients: _R_, and _Java_. Here is how you can make sure you have the right combination of the two:
 
#### Check R environment
1. If you do not already have it, install from [CRAN](https://cran.r-project.org/). Follow the instructions at the link to choose a mirror that will take you to the download page. After download, double click the file to install.
2. Open R. On the welcome message, there is a line starting with “Platform” and ending with “(32-bit)” or “(64-bit)”. It is very important to know which version (32-bit or 64-bit) of R you use, since the Java JDK should have the same version. 
3. Sometimes multiple versions of R could be installed on the same machine, so you should check the version you wish to use for data analysis. For example, if you prefer using RStudio to run the codes, you should check the default R version of RStudio by reading the welcome message for RStudio, instead of, say, the R version when opening from command line.

#### Check Java installation
1. To check if Java in installed on your **Mac** or **Linux** machine, open terminal if you use Mac or Linux and type in ```java -version```. If Java is installed, it will show the Java version number. Version number at least 1.7.x should be sufficient.
2. To check if Java is installed on your **Windows** machine, open **Control Panel**, select **Programs**, then select **Programs and Features**, and see if Java is listed. 
3. If no Java is installed (on Mac OSX, you will see a pop-up notice of installing Java JDK), or version too low, you should download and install a newer Java. The required download is JDK (Java Development Kit). Click on download link for JDK and choose the appropriate version. You should choose “x86” version if your R version is 32-bit, and “x64” if R is 64-bit. Then follow the instructions to finish download and install Java.
4. After successfully installing Java, try again typing ```java -version``` on terminal or Command Prompt. It should show the correct version number just installed.
