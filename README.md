# siriemashapes

siriemashapes is a package designed to facilitate the manipulation of data from Hotspot analysis from Siriema software (http://www.ufrgs.br/siriema/).

To install the package:

```
devtools::install_github('rdornas/siriemashapes')
```


**WARNING**: For best (and accurate) results, it is strongly recommended that the Hotspot analysis be performed taking into account the number of divisions set to double the radius.


siriemashapes has a function of the same name that requires the input of the original files used in the Siriema's Hotspot Analysis (road and events text files) as well as the original result file obtained in Siriema (txt or dat file). Last but not least is the coordinate system information in the crs argument. **This package only works with projected coordinate system. It is highly recommend that in `siriemashapes` function you input the EPSG number, which can be searched and found at https://epsg.io/.**

The event file **must** have the default columns, X, Y, Z, and also the species column, right after column Z.

For the sake of geographical accuracy, analysis can be time consuming, especially on very long highways.

The final result consists in a list containing a spatial data frame from Siriema's Hotspot Analysis as well as a data frame summarising the number of events of each species in each sample unit
