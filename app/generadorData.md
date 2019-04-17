---
title: "Generacion de datos"
output: html_notebook
---

```r
library(wakefield)
library(markdown)
```

#Generador de datos

```r
meses <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
dias <- c("sun","mon","tue","wed","thu","fri","sat")
mesesS<- sample(meses,50,replace = TRUE)
diasS<- sample(dias,50,replace = TRUE)
dataset <- r_data_frame(
  n = 50,
  height(min = 1,max = 9,digits=0,name = "X"),
  height(min = 1,max = 9,digits=0,name = "Y"),
  height(min = 18.7,max = 96.20,digits=1,name = "FFMC"),
  height(min = 1.1,max = 291.3,digits=1,name = "DMC"),
  height(min = 7.9,max = 860.3,digits=1,name = "DC"),
  height(min = 0.0,max = 56.10,digits=1,name = "ISI"),
  height(min = 2.2,max = 33.30,digits=1,name = "temp"),
  height(min = 15.0,max = 100,digits=1,name = "RH"),
  height(min = 0.40,max = 9.40,digits=1,name = "wind"),
  height(min = 0.0,max = 6.40,digits=1,name = "rain"),
  height(min = 0.0,max = 1090.84,digits=1,name = "area")
)

dataset$month = mesesS
dataset$day = diasS
if(file.exists("/home/rstudio/ProyectoFinal/dataset/dataGenerada.csv")){
  datafile <- read.csv("/home/rstudio/ProyectoFinal/dataset/dataGenerada.csv", stringsAsFactors = FALSE,check.names = FALSE)
  dataset<-rbind(dataset,datafile)
}
write.csv(dataset,"/home/rstudio/ProyectoFinal/dataset/dataGenerada.csv",row.names = FALSE)
dataset
```

```
## # A tibble: 4,650 x 13
##        X     Y  FFMC   DMC    DC   ISI  temp    RH  wind  rain  area month
##  * <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>
##  1     9     9  73.3  65.7  72.3  56.1  33.3  63.6   9.4   6.4  69.3 feb  
##  2     9     9  69.2  71.7  72.3  56.1  33.3  65.4   9.4   6.4  64   jul  
##  3     9     9  72    61.7  69    56.1  33.3  62.2   9.4   6.4  63   oct  
##  4     9     9  72.6  71.2  61.5  56.1  33.3  61.2   9.4   6.4  67.8 sep  
##  5     9     9  66.4  64.5  68.3  56.1  33.3  66.7   9.4   6.4  68.8 sep  
##  6     9     9  68.4  71.4  63.7  56.1  33.3  72.4   9.4   6.4  65.6 sep  
##  7     9     9  73.9  65.8  64.7  56.1  33.3  73.5   9.4   6.4  75.4 feb  
##  8     9     9  69.5  69.6  68.3  56.1  33.3  69     9.4   6.4  67.1 apr  
##  9     9     9  77    75.4  66.3  56.1  33.3  71.6   9.4   6.4  73.6 feb  
## 10     9     9  67.6  70.3  72    56.1  33.3  69.6   9.4   6.4  70   jun  
## # ... with 4,640 more rows, and 1 more variable: day <chr>
```
