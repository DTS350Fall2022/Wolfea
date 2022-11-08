---
title: "Task18"
author: "Austin"
date: "11/8/2022"
output: 
  html_document:
    keep_md: TRUE
    code_folding: 'hide'
editor_options: 
  chunk_output_type: console
---





```r
library(knitr)
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
## ✔ tibble  3.1.8     ✔ dplyr   1.0.8
## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
## ✔ readr   2.1.2     ✔ forcats 0.5.1
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(downloader)
library(corrplot)
```

```
## corrplot 0.92 loaded
```

```r
library(stringi)
library(stringr)
library(dplyr)
library(ggplot2)
library(haven)
library(readxl)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
## 
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(readr) 
library(haven)
library(riem)
library(tidyquant)
```

```
## Loading required package: PerformanceAnalytics
## Loading required package: xts
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## 
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
## 
## 
## Attaching package: 'xts'
## 
## The following objects are masked from 'package:dplyr':
## 
##     first, last
## 
## 
## Attaching package: 'PerformanceAnalytics'
## 
## The following object is masked from 'package:graphics':
## 
##     legend
## 
## Loading required package: quantmod
## Loading required package: TTR
## Registered S3 method overwritten by 'quantmod':
##   method            from
##   as.zoo.data.frame zoo
```

```r
library(timetk)
library(DT) 
library(dygraphs)
library(maps)
```

```
## 
## Attaching package: 'maps'
## 
## The following object is masked from 'package:purrr':
## 
##     map
```

```r
library(USAboundaries)
library(USAboundariesData)
library(sf)
```

```
## Linking to GEOS 3.10.2, GDAL 3.4.2, PROJ 8.2.1; sf_use_s2() is TRUE
```

```r
library(ggsflabel)
```

```
## 
## Attaching package: 'ggsflabel'
## 
## The following objects are masked from 'package:ggplot2':
## 
##     geom_sf_label, geom_sf_text, StatSfCoordinates
```

```r
library(USAboundariesData)
library(ggsflabel)
```


```r
cities <- us_cities() 
```

```
## City populations for contemporary data come from the 2010 census.
```

```r
head(cities)
```

```
## Simple feature collection with 6 features and 12 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: -161.7934 ymin: 58.3727 xmax: -134.1788 ymax: 71.25408
## Geodetic CRS:  WGS 84
## # A tibble: 6 × 13
##   city    state…¹ state…² county count…³ stplf…⁴ name_…⁵ city_…⁶ popul…⁷ place…⁸
##   <chr>   <chr>   <chr>   <chr>  <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
## 1 Anchor… Alaska  AK      THIRD… Third … 0203000 Anchor… US Cen… US Cen… Incorp…
## 2 Barrow  Alaska  AK      North… North … 0205200 Barrow… US Cen… US Cen… Incorp…
## 3 Bethel  Alaska  AK      Bethe… Bethel… 0206520 Bethel… US Cen… US Cen… Incorp…
## 4 Fairba… Alaska  AK      FOURT… Fourth… 0224230 Fairba… US Cen… US Cen… Incorp…
## 5 Homer   Alaska  AK      Kenai… Kenai … 0233140 Homer … US Cen… US Cen… Incorp…
## 6 Juneau  Alaska  AK      FIRST… First … 0236400 Juneau… US Cen… US Cen… Incorp…
## # … with 3 more variables: year <int>, population <int>, geometry <POINT [°]>,
## #   and abbreviated variable names ¹​state_name, ²​state_abbr, ³​county_name,
## #   ⁴​stplfips_2010, ⁵​name_2010, ⁶​city_source, ⁷​population_source, ⁸​place_type
```

```r
states <- us_states()
counties <- us_counties()
```


```r
idaho <- us_counties(states = "ID")
usmap <- sf::st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(usmap)
```

```
## Simple feature collection with 6 features and 1 field
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -124.3834 ymin: 30.24071 xmax: -71.78015 ymax: 42.04937
## Geodetic CRS:  WGS 84
##            ID                           geom
## 1     alabama MULTIPOLYGON (((-87.46201 3...
## 2     arizona MULTIPOLYGON (((-114.6374 3...
## 3    arkansas MULTIPOLYGON (((-94.05103 3...
## 4  california MULTIPOLYGON (((-120.006 42...
## 5    colorado MULTIPOLYGON (((-102.0552 4...
## 6 connecticut MULTIPOLYGON (((-73.49902 4...
```

```r
alaskahawaii <- cities %>%
  filter(state_name != 'Alaska', state_name != 'Hawaii')
```


```r
orderedcities <- alaskahawaii %>%
  group_by(state_name) %>%
  arrange(desc(population))
head(orderedcities)
```

```
## Simple feature collection with 6 features and 12 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: -118.4108 ymin: 29.78047 xmax: -73.9385 ymax: 41.83755
## Geodetic CRS:  WGS 84
## # A tibble: 6 × 13
## # Groups:   state_name [6]
##   city    state…¹ state…² county count…³ stplf…⁴ name_…⁵ city_…⁶ popul…⁷ place…⁸
##   <chr>   <chr>   <chr>   <chr>  <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
## 1 New Yo… New Yo… NY      NEW Y… New Yo… 3651000 New Yo… US Cen… US Cen… Incorp…
## 2 Los An… Califo… CA      LOS A… Los An… 0644000 Los An… US Cen… US Cen… Incorp…
## 3 Chicago Illino… IL      COOK   Cook    1714000 Chicag… US Cen… US Cen… Incorp…
## 4 Houston Texas   TX      HARRIS Harris  4835000 Housto… US Cen… US Cen… Incorp…
## 5 Philad… Pennsy… PA      PHILA… Philad… 4260000 Philad… US Cen… US Cen… Incorp…
## 6 Phoenix Arizona AZ      MARIC… Marico… 0455000 Phoeni… US Cen… US Cen… Incorp…
## # … with 3 more variables: year <int>, population <int>, geometry <POINT [°]>,
## #   and abbreviated variable names ¹​state_name, ²​state_abbr, ³​county_name,
## #   ⁴​stplfips_2010, ⁵​name_2010, ⁶​city_source, ⁷​population_source, ⁸​place_type
```

```r
Biggestcities <- orderedcities %>%
  slice(1:3)
first <- Biggestcities %>%
  slice(1:1)
second <- Biggestcities %>%
  slice(2:2)
third <- Biggestcities %>%
  slice(3:3)
head(third)
```

```
## Simple feature collection with 6 features and 12 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: -121.8193 ymin: 30.66843 xmax: -72.68334 ymax: 41.76605
## Geodetic CRS:  WGS 84
## # A tibble: 6 × 13
## # Groups:   state_name [6]
##   city    state…¹ state…² county count…³ stplf…⁴ name_…⁵ city_…⁶ popul…⁷ place…⁸
##   <chr>   <chr>   <chr>   <chr>  <chr>   <chr>   <chr>   <chr>   <chr>   <chr>  
## 1 Mobile  Alabama AL      MOBILE Mobile  0150000 Mobile… US Cen… US Cen… Incorp…
## 2 Mesa    Arizona AZ      MARIC… Marico… 0446000 Mesa c… US Cen… US Cen… Incorp…
## 3 Fayett… Arkans… AR      WASHI… Washin… 0523290 Fayett… US Cen… US Cen… Incorp…
## 4 San Jo… Califo… CA      SANTA… Santa … 0668000 San Jo… US Cen… US Cen… Incorp…
## 5 Aurora  Colora… CO      multi… Multip… 0804000 Aurora… US Cen… US Cen… Incorp…
## 6 Hartfo… Connec… CT      HARTF… Hartfo… 0937000 Hartfo… US Cen… US Cen… Incorp…
## # … with 3 more variables: year <int>, population <int>, geometry <POINT [°]>,
## #   and abbreviated variable names ¹​state_name, ²​state_abbr, ³​county_name,
## #   ⁴​stplfips_2010, ⁵​name_2010, ⁶​city_source, ⁷​population_source, ⁸​place_type
```


```r
USmap <- ggplot() +
  geom_sf(data = usmap, fill = NA) +
  geom_sf(data = idaho, fill = NA) +
  geom_sf(data = first, aes(size = population/1000), color = "lightblue") +
  geom_sf(data = second, aes(size = population/1000), color = "royalblue") +
  geom_sf(data = third, aes(size = population/1000), color = "darkblue") +
  geom_sf_label(data = first, aes(label = city), color = "black", nudge_x = 1.5, nudge_y = 1, size = 2) +
  scale_size_continuous(name = 'Population\n(1,000)') +
  labs(title="Largest cities is the United State", x="Latitude",y="Longitude") +
  theme_bw() 
USmap
```

```
## Warning in st_point_on_surface.sfc(data$geometry): st_point_on_surface may not
## give correct results for longitude/latitude data
```

![](Task18_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

