# visualFields
## Statistical Methods for Visual Fields

This is my fork of the official repository imarinfr/visualFields. Follow these steps to install my version of the package:

+ install the devtools package
+ use the install_github command as shown below
+ use the `ref` parameter to select a branch

```
install.packages("devtools")

library(devtools)
install_github("huchzi/visualFields/source")
```

## Testing the import function 'loadvfEyesuite'

Export your visual field data from the Eyesuite software. In the menu, click `Perimetry -> Export examination data`. In window that comes up, you can input criteria to select visual fields. Currently, binocular visual fields will be ignored. When you are done, click the `Export examination` button at the bottom of the window. 

The data will be saved into a file that is pre-specified in the preferences menu. To change this file, click `Tools -> Preferences`, select `Perimetry -> Export` on the left and change the path and/or the name of  `Report file` on the right.