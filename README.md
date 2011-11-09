Pretty Darn Good Control: Applied Optimal Control Tools & Examples 
==================================================================

This repository contains a collection of scripts and functions being
actively used and developing in research in optimal control as 
part of a NIMBioS working group, PDG Control.  


Installation for R
------------------

Install R.  See http://www.r-project.org
Install required packages:
Launch R and do:

```R 
install.packages(c("bvpSolve", "ggplot2")) 
```

Matlab installation
-------------------------------------
Collaborators work in Matlab, and some scripts here are written in matlab.
I've tried to ensure that my adapted matlab scripts will run in the open-source
clone "octave," which can be freely installed on most platforms.  


What's here so far?
------------------
* R code for a collocation example of Optimal Control by Pontryagin's method
* R code for a dynamic programming solution (Bellman Equation) of optimal control
* R code and matlab code for a stochastic dynamic programming solution


Progress & what need's doing
============================
Information about progress milestones and what's up next can be followed through the Issues Tracker, see: https://github.com/cboettig/pdg_control/issues?sort=created&direction=desc&state=open
More details about the project are avialble in my research lab notebook here: http://www.carlboettiger.info/archives/tag/PDG-Control

