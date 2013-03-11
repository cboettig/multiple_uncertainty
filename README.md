Multiple Uncertainty
====================

* Carl Boettiger, Mike Springborn, Jim Sanchirico


Abstract
--------

This project is an exploration into measuring and comparing the value of information in the problem of optimal control/management of a natural resource under multiple sources of uncertainty.  Uncertainty about model dynamics, quality of information, and the accuracy of management implementation pose a challenge both in theory and practice of managing natural systems.  Different forms of uncertainty may interact, making it impossible to study each in isolation, even in theory and simulation.  While managers can often gather additional information to decease uncertainty, not all information is equally valuable.  The goal of this project is to help illuminate what kinds of information are most valuable (or kinds of uncertainty are most hazardous) in a way that can be quantified directly in the context of the management optimization problem.  


Install notes
-------------

This repository is structured as an R package and can be installed directly using 

```r
library(devtools)
install.guthub("multiple_uncertainty", "cboettig")
```

[Research scripts](https://github.com/cboettig/multiple_uncertainty/tree/master/inst/examples)
--------------------

Research scripts are [knitr](http://yihui.name/knitr) files containing various investigations that are part of this project.  These scripts contain notes and descriptions of the investigation, code to carry out the simulations and analyses, and graphs and tables of the results as part of a single dynamic document.  Version history of these scripts tracks the evolution of an analysis.  Methods common to all scripts that allow a more general toolbox for exploring optimal control and multiple uncertainty are provided as functions of the R package.  Scripts can be linked to their most recent version or to version-stable SHA1 hashes, ensuring that they can be reproduced.  Make sure all files from the repo match the hash to ensure that functions external to the script also match the original run.  

[Issues tracker](https://github.com/cboettig/multiple_uncertainty/issues)
------------------

Completed and pending tasks can be seen in the issues tracker, and may be organized by type and milestone.  

Lab notebook
------------

Any research notes connected to this project appear under the [#multiple-uncertainty](http://carlboettiger.info/tags.html#multiple-uncertainty) tag in my online [lab notebook](http://carlboettiger.info/lab-notebook)

