### Map controls

  1. `Variable`: Select the variable to visualize.

  1. `Date`: Select the date to visualize.  
    - Dates are limited to the last 365 days since the most recent date available.
    - The most recent date available is usually 5 days ago from the actual date, due to limitations imposed by the data sources (meteorological daily data is only available after 5 days).
  
  1. `Aggregation`: Select the level of aggregation to visualize.  
    - `None` shows a raster image (500m resolution)
    - `Municipalities`, `Counties` and `Provinces` show aggregated data by the selected level.

### Map exploration

Using the mouse you can modify the map view:

  - Scrolling will increase/reduce the map zoom.  
  - Dragging will move the map.  
  - Dragging while pressing `Ctrl` allow to change map aspect and azimuth. This is especially useful when visualizing aggregated data polygons.

### Map download

Download of 500m resolution maps is available in the public EMF data repository, as `gpkg` files containing points at the center of each cell.