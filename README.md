# harpCore
*Core functions and methods for the harp ecosystem*

This package underlies everything in harp. It defines all of the classes, 
generics and some methods used in the other harp packages, as well as some functions 
that are needed in more than one of the harp packages. The most important classes 
are:

* **geolist** - A list of 2d georeferenced fields.
* **harp_df** (and subclasses) - Data frames containing harp data.
* **harp_list** - A list of harp_df data frames.

Methods are defined for *most* [dplyr](https://dplyr.tidyverse.org/) verbs. 

In addition, functions for handling date-times, geographic transformations of 
gridded data, data frame manipulations specific to harp, meteorological 
formulae and smoothing 2d fields are included. 

This package is automatically attached by all other harp packages. 
