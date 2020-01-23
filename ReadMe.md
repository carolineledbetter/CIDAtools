# CIDAtools  
The primary purpose of this package is to house RProject Templates that automatically setup folder
structures, ReadMe, and .git according to the preferred workflow at CIDA. 

<img src="inst/figures/CIDAtoolshex.png" alt="CIDAtools" width="259" height="300"/>

`devtools::install_github('ledbettc/CIDAtools')`

This package allows the Project Name, PI, and Project Analyst to be stored in a ProjData.dcf file to be 
easily retrieved and used in headers and reports. 

Example of header snippet using Project Data:
```
snippet header
	###########################################
	# Project: `r CIDAtools::ProjectName()`
	# Author: `r CIDAtools::ProjectAnalyst()`
	# Date: `r paste(format(Sys.Date(), '%m/%d/%Y'))`
	# #########################################
```
This package contains numerous templates and tools to make life at CIDA (part 
of the Colorado School of Public Health) happier and more efficient!

There is a CIDA project template (requires RStudio version 1.1) that sets up the
project folder using the CIDA template with ReadMe files in each folder and an 
option to set up a git repository. You can also save project metadata. 

This package also contains functions for reading excel files with colour columns
and the Table1 function. 

Please feel free to file an issue request if you encounter errors or would like 
to request new features.  

## News/Notes
Currently tibbles do not work with `Table1()`. To use `Table1()` with a tibble please use `as.data.frame()`. A new function 
(`desc_table()`)
utilizing tidy principles and compatible with the tidyverse is under construction. A preliminary version (lacking some of the 
ultimate functionality) is available/working. 
If you would like to use it while it is in development it is under development in the desc_table branch. 
The commit messages/code comments 
contain details on where in the process I am at. 
