# visualFields
## Statistical Methods for Visual Fields

This repository contains the source code for the R package "visualFields". This package is a collection of tools for analyzing the field of vision. It provides a framework for the development and use of innovative methods for visualization, statistical analysis, and clinical interpretation of visual-field loss and its change over time. visualFields is intended to be a tool for collaborative research.

More detailed information can be found here:
http://jov.arvojournals.org/article.aspx?articleid=2121355

## How can I install the visualFields package?

The stable version can be downloaded from CRAN ("The Comprehensive R Archive Network").

```
install.packages("visualFields")
```

If you want to test new features, you can also directly download, build and install from this repository.
+ install the devtools package
+ use the install_github command as shown below
+ use the `ref` parameter to select a branch

```
install.packages("devtools")

library(devtools)
install_github("imarinfr/visualFields")
```

## How can I contribute to this project?

If you want to contribute code, please note that your code should fulfil the requirements set by CRAN. You can test that by running `R CMD check --as.cran source`. If this test throws errors, warnings or even notes, the package will be rejected by CRAN. This will cause problems with code that relies on packages from the tidyverse or on data.table.

### Test code

If you have an Octopus perimeter by Haag-Streit, you can also contribute by downloading the newest version of the package as shown above using install_github and testing the loadvfEyesuite function.

Export your visual field data from the Eyesuite software. In the menu, click `Perimetry -> Export examination data`. In window that comes up, you can input criteria to select visual fields. Currently, binocular visual fields will be ignored. When you are done, click the `Export examination` button at the bottom of the window. 

The data will be saved into a file that is pre-specified in the preferences menu. To change this file, click `Tools -> Preferences`, select `Perimetry -> Export` on the left and change the path and/or the name of  `Report file` on the right.