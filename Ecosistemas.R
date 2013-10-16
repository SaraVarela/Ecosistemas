library (raster)
library (rgdal)
library (dismo)

# load the species range 
setwd ("C:\\Users\\sara\\Documents\\_CIENCIAS\\sampling")
sp<- raster ("POTENTIAL_D.rst")
mask<- raster ("mask.rst")
map_sp=sp+mask
plot (map_sp, axes=F, box=F, legend=F, 
      col=c("lightblue", "white", "darkblue"))
title ("Potential distribution")

# load the map of distances to the roads
road<- raster ("dist_carretera.rst")
col_blue<- colorRampPalette(c("darkblue","blue","lightblue","white"), 100)
plot (road, col=col_blue (100), zlim=c(0, 0.5))
title ("Distance to roads")


# sample the distribution of the species with bias
spp<- reclassify (sp, c(-Inf,0, NA))
mapa_spp<- sp+mask
na_mask<- reclassify (mask, c(-Inf, 0.5, NA))
road_m=road*mask
m_road2<- road_m*na_mask
muestreo<- m_road2*sp
sumar<- reclassify (sp, c(-0.5, 0.5, 1, 0.5, 1.5, 0))
muestreo2<- muestreo + sumar
plot (muestreo2, axes=F, box=F, legend=F, 
      col=colores, colNA="lightblue")

samp<- sampleRandom (spp, 10000, na.rm=TRUE, xy=TRUE)
puntos<- data.frame (samp[,1], samp[,2])
points (puntos)
peso_road<- extract (road, puntos)

road_25_<- list ()
for (i in 1:100)
{
  road_25<- sample (orden, size=25, 
                     replace=F, 
                     prob= abs(peso_road - max(peso_road)))
  road_25_[[i]]<- road_25
}

road_50_<- list ()
for (i in 1:100)
{
  road_50<- sample (orden, size=50, 
                    replace=F, 
                    prob= abs(peso_road - max(peso_road)))
  road_50_[[i]]<- road_50
}

road_100_<- list ()
for (i in 1:100)
{
  road_100<- sample (orden, size=100, 
                     replace=F, 
                     prob= abs(peso_road - max(peso_road)))
  road_100_[[i]]<- road_100
}

road_250_<- list ()
for (i in 1:100)
{
  road_250<- sample (orden, size=250, 
                    replace=F, 
                    prob= abs(peso_road - max(peso_road)))
  road_250_[[i]]<- road_250
}

# load variables
setwd ("C:\\Users\\sara\\Documents\\_CIENCIAS\\sampling")
tmin<- raster ("TMIN.rst")
tmax<- raster ("TMAX.rst")
prec<- raster ("prec.rst")
variables<- stack (tmin, tmax, prec)


# run the models, bioclim for all the data sets (25, 50, 100 and 250 points)
res_road25<- NULL

for (i in 1:100)
{ 
  coord<- puntos [road_25_ [[i]], ]
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road25<- rbind (res_road25, res)
}

res_road50<- NULL
for (i in 1:100)
{ 
  coord<- puntos [road_50_ [[i]], ]
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road50<- rbind (res_road50, res)
}

res_road100<- NULL
for (i in 1:100)
{ 
  coord<- puntos [road_100_ [[i]], ]
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road100<- rbind (res_road100, res)
}

res_road250<- NULL
for (i in 1:100)
{ 
  coord<- puntos [road_250_ [[i]], ]
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road250<- rbind (res_road250, res)
}

# run the models, maxent. 

res_road_maxent25_thresh<- NULL
for (i in 39:50)
{ 
  coord<- puntos [road_25_ [[i]], ]
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road_maxent25_thresh<- rbind (res_road_maxent25_thresh, res)
}

res_road_maxent50_thresh<- NULL
for (i in 1:50)
{ 
  coord<- puntos [road_50_ [[i]], ]
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road_maxent50_thresh<- rbind (res_road_maxent50_thresh, res)
}

res_road_maxent100_thresh<- NULL
for (i in 1:50)
{ 
  coord<- puntos [road_100_ [[i]], ]
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road_maxent100_thresh<- rbind (res_road_maxent100_thresh, res)
}
res_road_maxent250_thresh<- NULL
for (i in 1:50)
{ 
  coord<- puntos [road_250_ [[i]], ]
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  res_road_maxent250_thresh<- rbind (res_road_maxent250_thresh, res)
}



# models calibrated with biased data and 3 mistaken occurrences

bio_error_25<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_25_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F) 
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  bio_error_25<- rbind (bio_error_25, res)
} 

bio_error_50<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_50_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  bio_error_50<- rbind (bio_error_50, res)
} 

bio_error_100<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_100_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F)   
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  bio_error_100<- rbind (bio_error_100, res)
} 

bio_error_250<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_250_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- bioclim (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  pred<- reclassify (prediction, c(-Inf, min (predicho), 
                                   0, min (predicho), 1.1, 2), right=F)
  
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  bio_error_250<- rbind (bio_error_250, res)
} 


max_error_25<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_25_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  max_error_25<- rbind (max_error_25, res)
} 

max_error_50<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_50_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  max_error_50<- rbind (max_error_50, res)
} 

max_error_100<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_100_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  max_error_100<- rbind (max_error_100, res)
} 

max_error_250<- NULL
for (i in 1:3)
{ 
  coord<- puntos [road_250_ [[1]], ]
  names (coord)<- c("x", "y")
  coord<- rbind (coord, errores[1:i,])
  modelo<- maxent (variables, coord)
  prediction<- predict (modelo, variables)
  predicho<- extract (prediction, coord)
  t<- modelo@results[40]
  pred<- reclassify (prediction, c(-Inf, t, 
                                   0, t, 1.1, 2), right=F)
  test<- pred+sp
  res<- c(length (which (test@data@values==1)),
          length (which (test@data@values==2)),
          length (which (test@data@values==3)))
  max_error_250<- rbind (max_error_250, res)
} 


##### for more information, please contact Sara Varela, svarela@paleobiogeography.org
