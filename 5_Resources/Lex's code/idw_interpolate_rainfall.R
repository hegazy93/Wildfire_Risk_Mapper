## packages
require(sf)
require(tidyverse)
require(raster)
library(terra)
library(gstat)
library(tidyterra)

# 1. read in land cover data to define an extent and a clip box grid
gr = raster("landcover_MRSDNM.tif", band = 1)
gr
# OK this is in OSGB projection (EPSG 27700)
plot(gr)
# now define a clip extent + 20km
# and make a polygon to clip the data
ex = extent(gr)
fac = 20000
xmin = ex[1] - fac
xmax = ex[2] + fac
ymin = ex[3] - fac
ymax = ex[4] + fac
# define a closed box
my.box <- data.frame(
  ID = c(rep("1",5)),
  X = c(xmin, xmax, xmax, xmin, xmin),
  Y = c(ymin, ymin, ymax, ymax, ymin))
# my.poly
# then make polygon 
my.poly <- sfheaders::sf_polygon(
  obj = my.box, x = "X", y = "Y", polygon_id = "ID"
)
st_crs(my.poly) = st_crs(27700)
plot(my.poly)
my.poly$ID <- as.numeric(as.character(my.poly$ID))
# make grid
rp <- rasterize(my.poly, gr, "ID")

# 2. read in rainfall, make into a spoatial object and create a surface 
r = st_read("api_d.gpkg")
# reads it as data.frame - this was my mistake in exporting in from your computer! 
class(r)
head(r)
# make spatial and convert to OSGB projection
r = r |> 
	st_as_sf(coords = c("long", "lat"), crs = st_crs(4326)) |>
	st_transform(27700)
r = r[my.poly,]
ggplot(my.poly) + geom_sf() +
	geom_sf(data = r, aes(col = daily_rainfall) )

# 3. make the interpolated model
fit_drain <- gstat(formula = daily_rainfall ~ 1, data = r,  set = list(idp = 2))
# gstat same as spdep::idw
drain_int <- interpolate(rp, model=fit_drain, ext=rp)
plot(drain_int)

# check	
drain_int_terra = rast(drain_int)
ggplot(my.poly) + geom_sf() +
	tidyterra::geom_spatraster(data = drain_int_terra, aes(fill = var1.pred)) + 
	scale_fill_viridis_c() +
	geom_sf(data = r)

