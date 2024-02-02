# harpCore 0.2.2

* Hotfix release that improves `define_domain()` and adds `geo_reproject()`
  * `define_domain()` has been improved so that it can take a _proj_ string, 
  e.g. `"+proj=lcc +lon_0=15 +lat_0=63.3 +lat_1=63.3 +lat_2=63.3 +R=6371000"` as
  the `proj` argument.
  * `geo_reproject()` has been added to enable data frames with latitude and 
  longitude columns to be expressed in projection coordinates.
  
# harpCore 0.2.1

* Hotfix release that adds functionality to generate time sequences with 
`seq_secs()`, `seq_mins()`, `seq_hours()` and `seq_days()`

# harpCore 0.2.0

* Initial version. Tagged v0.2.0 for consistency with versioning of existing
harp packages
