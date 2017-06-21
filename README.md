# Feature_engineering-Variable_generation

# estimate area of coverage of BTS by vorronoi tensellation
# Parckage required

library(mapproj)
library(deldir)

# Figure 3
# Area of coverage of BTS Using voronoi polygons

Latlong_projected <- mapproject(LatLong$Long, LatLong$Lat, "albers", param = c(39, 45))
par(mar=c(0,0,0,0))
plot(Latlong_projected, asp=1, type="n", bty="n", xlab="", axes=FALSE)
points(Latlong_projected, pch=16, cex=0.1, col="red")
vtess <- deldir(Latlong_projected$x, Latlong_projected$y)
Vtess1 <- plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)




## PART 2 Demographic and household survey dataset (DHS)


## Reading the datasets 
cord <- read.csv("Coordinates.csv") # DHS
cord$Cluster <- c(1:492) # introducing an ID
rw <- read.csv("RWDHSVI.csv") # GPS location of the clusters

#Merging Cluster lat-long with DHS by "cluster ids"

df_dhs <- merge(cord, rw, by = "Cluster", sort =FALSE)

# Remove unnecessary columns

df_dhs <- select(df_dhs,-5,-6,-8,-10,-11,-12)


# add unique numbers on df_dhs and df_merged1
df_dhs$id <- c(1:length(df_dhs$Cluster))
df_merged1$id <- c(1:length(df_merged$btsid))

# To merge the CDR dataset and DHS dataset we base on GPS cordinates

# Checking for cordinate points at a unique id
df_merged1$Lat[df_merged$id==200]   #lat  -1.94148
df_merged1$Long[df_merged$id==200]  #long 30.05189

df_dhs$latnum[df_dhs$id==200]  # lat -1.799653
df_dhs$longnum[df_dhs$id==200] # long 30.04251

#Rwanda NE and SW Extreme Cordinates
# NE: lat -1.047268, Long 30.474205
# SW: lat -2.18308, long 29.026362

# creating smaller data frames of df_merged1 and df_dhs
df_bts_ll <- unique(select(df_merged1, Lat,Long))

df_cluster_ll <- unique(select(df_dhs, latnum, longnum))

# Merging CDRS with DHS using BTS cordinates and cluster cordinates
# A for loop to iterate looking for < 0.005 as equal cluster and bts cordinates
# LONGITUDE 
k <- 1
cluster_bts_longs <- as.data.frame(matrix(ncol=2, nrow=0))

for(i in sort(df_cluster_ll$longnum)){
    for(j in sort(df_bts_ll$Long)){
        if (i - j  <  0.010){
            cluster_bts_longs[k,1] = i
            cluster_bts_longs[k,2] = j
        }else 
            next
        k = k + 1
    }
}
cluster_bts_longs_uni = cluster_bts_longs[!duplicated(cluster_bts_longs$V1),]
colnames(cluster_bts_longs_uni) <- c("longnum","Long")
#LATITUDE
k <- 1
cluster_bts_lats <- as.data.frame(matrix(ncol=2, nrow=0))

for(i in sort(df_cluster_ll$latnum)){
    for(j in sort(df_bts_ll$Lat)){
        if (abs(i - j)  <  0.010){
            cluster_bts_lats[k,1] = i
            cluster_bts_lats[k,2] = j
        }else 
            next
        k = k + 1
    }
}
cluster_bts_lats_uni = cluster_bts_lats[!duplicated(cluster_bts_lats$V1),]
colnames(cluster_bts_lats_uni) <- c("latnum","Lat")


# Merging, cluster and bts basing on their Latitude and longitude
data_cluster_latnum <- merge(df_dhs, cluster_bts_lats_uni, by ="latnum",all = TRUE, sort = FALSE)
data_cluster_latnum_longnum <- merge(data_cluster_latnum, cluster_bts_longs_uni, by ="longnum",all = TRUE, sort = FALSE)



# Merge, DHS and CDR datasets basing on "latnum,longnum":"Lat,Long"
data_cluster_bts <- merge(data_cluster_latnum_longnum, df_final, by =c("Lat","Long"),all = TRUE, sort = FALSE)
df_cluster_bts <- na.omit(data_cluster_bts)

# Figure 4
# Associating cluster cordinates to BTS polygons they belong to 

Latlong_projected <- mapproject(LatLong$Long, LatLong$Lat, "albers", param = c(39, 45))
CordLL_proj <- mapproject(cord$longnum, cord$latnum, "albers", param = c(39, 45))

par(mar=c(0,0,0,0))
plot(Latlong_projected, asp=1, type="n", bty="n", xlab="", axes=FALSE)

points(Latlong_projected, pch=16, cex=0.1, col="red")
points(CordLL_proj, pch=19, cex=0.1, col="blue")
vtess <- deldir(Latlong_projected$x, Latlong_projected$y)
Vtess1 <- plot(vtess, wlines="tess", wpoints="none", number=FALSE, add=TRUE, lty=1)



# Extraction of variables
# Cell phone usage Independent and DHS dependent variables
# 1. Mobility, Sites used per caller



#Load Data
# First Indepent variable: Consumption Variable
df_Consu <- read.csv("Usage.csv")
df_Consu1 <- select(df_Consu, -1, -2, -4)

#2. Demographic
df_Demo <- read.csv("Demographic_location.csv")
df_Demo1 <- select(df_Demo, -1, -2, -4)

#3. Cordinates
df_cor <- read.csv("Cluster_bts.csv")
df_cor1 <- select(df_cor, -1)

# Merge cordinate with Usage using Lat
Consu_Cor <- merge(df_Consu1, df_cor1, by = "Lat", all = TRUE, sort = FALSE)

# Merge Consumption and Demographic by latnum
Consu_Demo <- merge(Consu_Cor, df_Demo1, by = "latnum", all = TRUE, sort = FALSE)

