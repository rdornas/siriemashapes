# siriemashapes

siriemashapes is a package designed to facilitate the manipulation of data from Hotspot analysis from Siriema software (http://www.ufrgs.br/siriema/).

The package generates a spatial data frame with Hotspot analysis results, as well as a data frame listing the number of events for each taxon in each sample unit.

To install the package:

```
devtools::install_github('rdornas/siriemashapes')
```

**WARNING**: For best (and accurate) results, it is strongly recommended that the Hotspot analysis be performed taking into account the number of divisions set to double the radius.


siriemashapes has a function of the same name that requires the input of the original files used in the Hotspot analysis (road and events text files) as well as the original result file obtained in Siriema (txt or dat file). Last but not least is the coordinate system information in the crs argument. **It is highly recommend entering the EPSG number which can be searched and found at https://epsg.io/.**

The event file must have the default columns, X, Y, Z, and also the species column, right after column Z.
