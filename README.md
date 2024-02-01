# siriemashapes

siriemashapes is a package designed to facilitate data manipulation of Hotspot analysis from Siriema software (http://www.ufrgs.br/siriema/).

To install the package:

```
devtools::install_github('rdornas/siriemashapes')
```


**WARNING**: For optimal and accurate results, it is strongly recommended to perform the Hotspot analysis by setting the number of divisions to double the radius. Please note that there might be discrepancies between the initial and final number of events, as the function only includes events within 250 m of the road axis.

The siriemashapes function requires the original files used in Siriema’s Hotspot Analysis (road and events txt files), as well as the original result file obtained from Siriema (txt or dat file). Additionally, the coordinate system information must be provided in the crs argument. This package is compatible only with projected coordinate systems. **It is highly recommended to input the EPSG number in the `siriemashapes` function, which can be found at https://epsg.io/.**

The event file **must** include the default columns, X, Y, Z, followed by the species column.

Due to the need for geographical accuracy, the analysis can be time-consuming, especially for very long roads.

The final output is a list containing a spatial data frame from Siriema’s Hotspot Analysis and a data frame summarizing the number of events for each species in each sample unit.





