# Geography and mappong convenience functions

# Useful packages:
# rgdal -- Loading shapefiles, etc.
# maps -- loading simple maps
# mapdata -- Manipulating maps
# mapproj -- Manipulating maps
# maptools -- Manipulating maps
# GISTools -- more mapping tools
# rgeos
# geosphere
# PBSmapping
# OpenStreetMap
# plotGoogleMaps
# raster
# shapefiles
# fields
# SpatialEpi

# Combine a spatial polygon dataframe with another dataframe
merge.shpdf.df <- function(shpdf, df, by.shpdf, by.df) {	
	shpdf@data <- data.frame(shpdf@data, df[match(shpdf@data[, by.shpdf], df[, by.df]), ])
	return(shpdf)
}

# Combine a spatial polygon with a dataframe
# Note that the row names of shp must match with the row names of df
merge.shp.df <- function(shp, df, by.df) {
	row.names(df) <- df[, by.df]
	df <- df[row.names(df) %in% row.names(shp), ]	
	return(SpatialPolygonsDataFrame(shp, data = df))
}

earth.dist <- function (long1, lat1, long2, lat2, units = "mi") {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  # If not miles, assume kilometers
  if (units == "mi") {
    R <- 3963.1676
  } else {
    R <- 6378.1
  }
  d <- R * c
  return(d)
}

# Ripped from http://r-sig-geo.2731867.n2.nabble.com/compute-buffer-from-point-shapefile-to-have-shapefile-td4574666.html
# Radius is specified in meters
# Key function is http://127.0.0.1:18224/library/geosphere/html/destPoint.html
buffer <- function(pts, radius, angle = 5, proj = "+proj=longlat") {  
  angles <- seq(1, 360, by = angle)
  crds = list()
  if (!is.null(nrow(pts))) {
    for(i in 1:nrow(pts)) {
      d <- destPoint(pts[i, ], angles, radius)
      crds[[i]] <- rbind(d, d[1,])
    }
  } else {
    d <- destPoint(pts, angles, radius)
    crds[[1]] <- rbind(d, d[1, ])
  }
  p <- lapply(crds, Polygon)
  pp <- list()
  for(i in 1:length(p)) pp[i] = Polygons(p[i], i)
  #spdf <- SpatialPolygonsDataFrame(SpatialPolygons(pp, proj4string = CRS("+init=epsg:4326")), data.frame(id = 1:length(p)))
  spdf <- SpatialPolygonsDataFrame(SpatialPolygons(pp, proj4string = CRS(proj)), data.frame(id = 1:length(p)))
  gUnionCascaded(spdf)  
}

# For finding intersections of polygons:
#
# First, use the "over" function to trim down search space --
# int <- group.of.polygons[!is.na(over(group.of.polygons, big.polygon)), ]  # Get the intersecting polygons
#
# Then:
# output <-
#   do.call(
#     rbind,
#     lapply(
#       1:nrow(int),
#       function(i) {
#         data.frame(
#           id = i,
#           origarea = gArea(SpatialPolygons(int@polygons[i])),
#           newarea = gArea(gIntersection(SpatialPolygons(int@polygons[i]), SpatialPolygons(bufTr@polygons))),
#           population = int$fakepop[i]
#         )      
#       }
#     )
#   )

# Simplify a shapefile (with resolution specified by res)
# Ripped mostly from http://stackoverflow.com/questions/20976449/r-simplify-shapefile
SimplifyShp <- function(shapefile, res = 0.01) {
  for(i in 1:length(shapefile@polygons)) {
    for(j in 1:length(shapefile@polygons[[i]]@Polygons)) {
      temp <- as.data.frame(shapefile@polygons[[i]]@Polygons[[j]]@coords)
      names(temp) <- c("x", "y")
      temp2 <- dp(temp, res)
      shapefile@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
    }
  }
  return(shapefile)
}

# Get a map file as a polygon
GetMapPoly <- function(region, type = "state") {
  if (region != "usa") {
    m <- map(type, region, fill=TRUE, col="transparent", plot=FALSE)
  } else {
    m <- map(region, fill = T, col = "transparent", plot = F)
  }
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  map2SpatialPolygons(m, IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))  
}

# Find points underlaying a map
# Region: the region to fetch for the map
# Shp: the shapefile to match against the region
# Width: The gBuffer radius to add
GetMapOverlap <- function(region, shp, type = "state", width = 0.1) {
  # Because when you're working with the entire US, things get whacky...  
  regionmap <- gBuffer(GetMapPoly(region, type), width = width)
  statezips <- shp[!is.na(over(shp, regionmap)), ]
  statezips@data[is.na(statezips$brokers), c("brokers", "br1pd", "br3pd", "total")] <- rep(0, 4)
  return (statezips)
}

# An example of plotting a line on a google map:
# usePackage("ggmap")
# usePackage("RgoogleMaps")
# usePackage("geosphere")

# usa <- GetMapPoly("usa")
# center <- data.frame(t(apply(usa@bbox, 1, mean)))
# colnames(center) <- c("lon", "lat")
# us.map <- get_map(usa@bbox, zoom = 4)

# p1 <- c(-71.166548, 42.335137)
# p2 <- c(-118.288162, 34.014126)
# test <- data.frame(gcIntermediate(p1, p2, 50))
# ggmap(us.map) + geom_line(aes(x = lon, y = lat), data = test, size = 1, alpha = .4, color = "red")