library(tidyverse)
library(tmap)
library(tmaptools)
library(RSQLite)
library(dplyr)
library(here)
library(plotly)
library(maptools)
library(RColorBrewer)
library(classInt)
library(sp)
library(rgeos)
library(sf)
library(rgdal)
library(geojsonio)
library(grid)
library(spatstat)
library(rgeos)
library(maptools)
library(GISTools)
library(geojson)
library(stringr)
library(janitor)
library(broom)
library(mapview)
library(crosstalk)
library(spdep)
library(car)
library(fs)
library(raster)
library(fpc)
library(tidypredict)
library(broom)
library(rsample)

#1.data input
#1.1 sat result
sat_result <- read.csv("sat.csv", 
                       header = TRUE, sep = ",", encoding = "latin1")%>%
  clean_names()


#1.2 school survey
school_survey <- read.csv("sat-survey.csv", 
                          header = TRUE, sep = ",", encoding = "latin1")%>%
  clean_names()


#1.3 join school survey and sat result---n=416
result_survey <- sat_result %>%
  merge(.,
        school_survey, 
        by.x="dbn", 
        by.y="x_ef_bb_bf_dbn",
        no.dups = TRUE)%>%
  distinct(.,dbn, 
           .keep_all = TRUE)%>%
  clean_names()

#1.4 school dictionary -- have coordinate
school_dictionary <- read.csv("directory.csv", 
                            header = TRUE, sep = ",", encoding = "latin1")%>%
  clean_names()


#1.5 full infor--high school joined SAT result --n = 332
sat_info_1 <- school_dictionary %>%
  merge(.,
        result_survey, 
        by.x="dbn", 
        by.y="dbn",
        no.dups = TRUE)%>%
  distinct(.,dbn, 
           .keep_all = TRUE)%>%
  clean_names()

#1.6 mutate - higher and lower than average SAT level

sat_info_2 <- sat_info_1 %>%
  mutate(avg_sat_sum = (sat_critical_reading_avg_core + sat_math_avg_core + sat_writing_avg_core ))%>%
  mutate(total_average_sat= sum(avg_sat_sum)/332)

#1.7 nyc-outline #3857
nyc_outline <- st_read(here::here("zipcode", 
                                  "ZIP_CODE_040114.shp")) %>%
  st_transform(., 3857)

nyc_school_map <- nyc_outline %>%
  clean_names()%>%
  merge(.,
        sat_info_2, 
        by.x="zipcode", 
        by.y="x_ef_bb_bf_zipcode",
        no.dups = TRUE)

#1.8 plot the sat map
tmap_mode("view")
qtm(nyc_school_map, 
    fill = "avg_sat_sum", 
    borders = NULL,  
    fill.palette = "Blues")

tmap_mode("plot")
nyc_school_map %>%
  qtm(.,fill = "avg_sat_sum")

#1.8 respect and safe score 
tmap_mode("view")
qtm(nyc_school_map, 
    fill = "total_safety_and_respect_score", 
    borders = NULL,  
    fill.palette = "Blues")

#1.9 choose above average SAT result ,n=119
sat_good_performance <- sat_info_2 %>%
  filter(`avg_sat_sum`>1222.491)%>%
  clean_names()#n=119

#1.10 how much-zipcode
sat_good_performance_group <- sat_good_performance%>%
  group_by(x_ef_bb_bf_zipcode)%>%
  summarise( count=n())

sat_good_performance_group_2 <-  sat_good_performance %>%
  clean_names()%>%
  merge(.,
        sat_good_performance_group, 
        by.x="x_ef_bb_bf_zipcode", 
        by.y="x_ef_bb_bf_zipcode",
        no.dups = TRUE)%>%
  distinct()

#2.Spatial
# change to coordinate --3857-NYC
sat_good_performeance_coor <-  sat_good_performance_group_2 %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 3857)

tmap_mode("view")
tm_shape(nyc_outline) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(sat_good_performeance_coor) +
  tm_dots(col = "blue")

window <- as.owin(nyc_outline)
plot(window)

## change soatial data
sat_good_performeance_spaces<- sat_good_performeance_coor %>%
  as(., 'Spatial')

sat_good_performeance_spaces_2.ppp <- ppp(x=sat_good_performeance_spaces@coords[,1],
                          y=sat_good_performeance_spaces@coords[,2],
                          window=window)

sat_good_performeance_spaces_2.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Excellent high school in New York City")

#Kernel Density Estimation
sat_good_performeance_spaces_2.ppp %>%
  density(., sigma=1000) %>%
  plot()

#平方分析
plot(sat_good_performeance_spaces_2.ppp,
     pch=16,
     cex=0.5, 
     main="Excellent high school in New York City")

sat_good_performeance_spaces_2.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")

Qcount <- sat_good_performeance_spaces_2.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>%
  dplyr::count(Var1=Freq)%>%
  dplyr::rename(Freqquadratcount=n)

sums <- Qcount %>%
  mutate(total = Var1 * Freqquadratcount) %>%
  dplyr::summarise(across(everything(), sum))%>%
  dplyr::select(-Var1) 

lambda<- Qcount%>%
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>%
  dplyr::select(lambda)%>%
  pull(lambda)

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

plot(c(1,5),c(0,14), type="n",
     xlab="Number of excellent school (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

####above know any kinds of spatial patterning associated with the 
###excellent higher school in the area of NYC

###The formula for calculating expected probabilities based on the Poisson distribution
###oppositive，
###cluster or dispeared
###Test

teststats <- quadrat.test(sat_good_performeance_spaces_2.ppp, nx = 6, ny = 6)

plot(sat_good_performeance_spaces_2.ppp,pch=16,cex=0.5, main="Excellent high school in New York City")
plot(teststats, add=T, col = "red")

#Ripley’s K----
K <- sat_good_performeance_spaces_2.ppp %>%
  Kest(., correction="border") %>%
  plot()
##


##DBSCAN
st_geometry(nyc_outline)
##take 2500m
##4

sat_good_performeance_spaces_2.points <- sat_good_performeance_spaces %>%
  coordinates(.)%>%
  as.data.frame()

db <- sat_good_performeance_spaces_2.points %>%
  fpc::dbscan(.,eps = 2500, MinPts = 5)

plot(db, sat_good_performeance_spaces_2.points, main = "DBSCAN Output", frame = F)
plot(nyc_outline$geometry, add=T)

##eps----

library(dbscan)

sat_good_performeance_spaces_2.points%>%
  dbscan::kNNdistplot(.,k=5) 

###The distance where you have a sharp change in curve is your epsilon
##five cluster

library(ggplot2)
db
db$cluster

sat_good_performeance_spaces_2.points<- sat_good_performeance_spaces_2.points %>%
  mutate(dbcluster=db$cluster)

chulls <- sat_good_performeance_spaces_2.points %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

#delect zero
chulls <- chulls %>%
  filter(dbcluster >=1)

#
dbplot <- ggplot(data=sat_good_performeance_spaces_2.points, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 

dbplot <- dbplot + geom_point()

dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 

dbplot + theme_bw() + coord_equal()

#3.1spatial autocorrelation
# change coordinate
sat_good_performeance_spaces_3 <- sat_good_performeance_coor %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 3857)

sat_good_performeance_spaces_4 <- sat_good_performeance_spaces_3%>%
  rename(zipcode = `x_ef_bb_bf_zipcode`)
  
tmap_mode("view")
tm_shape(nyc_outline) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(sat_good_performeance_spaces_4) +
  tm_dots(col = "blue")

#3.3 count how many plot in each zip areas
library(sf)
points_sf_joined <- nyc_outline %>%
  st_join(sat_good_performeance_spaces_4)%>%
  janitor::clean_names()%>%
  mutate(area=st_area(.))%>%
  mutate(density= count/area)%>%
  dplyr::select(density, zipcode, count)

points_sf_joined_2 <- points_sf_joined %>%                    
  group_by(zipcode) %>%         
  summarise(density = first(density),
            plaquecount= first(count))

points_sf_joined_d1_3 <- points_sf_joined_2%>%
  na.omit(points_sf_joined_d1)

#3.4 development --- with missing value and without 
tm_shape(points_sf_joined_2) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("density"),
              title="excellent School Density")


tm_shape(points_sf_joined_d1_3) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("density"),
              title="excellent School Density")

#3.5test -Moran'I
library(spdep)

#3.5.1calculate the centroids of all excellent high school in newyor
coordsW <- points_sf_joined_d1_3%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#3.6generate a spatial weights matrix 
NYC_nb <- points_sf_joined_d1_3 %>%
  poly2nb(., queen=T)

plot(NYC_nb, st_geometry(coordsW), col="red")
plot(points_sf_joined_d1_3$geometry, add=T)

#3.7
NYC.lw <- NYC_nb %>%
  nb2listw(., style="C",zero.policy = TRUE)
##this means that at least on site has no neighbours

#3.8M‘I 
I_Global_Density_ <- points_sf_joined_d1_3 %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(.,NYC.lw, zero.policy=TRUE)

I_Global_Density_

##

#3.9 Geary'C----C = 0.116
C_Global_Density <- points_sf_joined_d1_3 %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., NYC.lw,zero.policy=TRUE)

C_Global_Density

#3.10 Getis Ord General G 1.8343e-0.2 < Expectation=1.562500e-0.2 , low values clustering
G_Global_Density <-   points_sf_joined_d1_3 %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., NYC.lw,zero.policy=TRUE)

G_Global_Density


#4.regression
#4.
#
sat_info_2_regression <-  sat_info_2 %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 3857)
###
###
hyp_1 <- qplot(x = `total_safety_and_respect_score`, 
               y = `avg_sat_sum`, 
               data=sat_info_2_regression)

hyp_1 + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter()

#4.1regression model

Regressiondata<- sat_info_2_regression%>%
  dplyr::select(avg_sat_sum, 
                total_safety_and_respect_score)

model1 <- Regressiondata %>%
  lm(avg_sat_sum ~
       total_safety_and_respect_score,
     data=.)

summary(model1)
library(broom)
tidy(model1)
glance(model1)
##

#4.2 Bootstrap resampling
Bootstrapdata<- sat_info_2_regression%>%
  clean_names()%>%
  dplyr::select(avg_sat_sum, total_safety_and_respect_score)

set.seed(99)

sat_info_2_boot <- st_drop_geometry(Bootstrapdata)%>%
  bootstraps(times = 1000, apparent = TRUE)

slice_tail(sat_info_2_boot, n=5)

SAT_models <- sat_info_2_boot %>%
  mutate(
    model = map(splits, ~ lm(avg_sat_sum ~ total_safety_and_respect_score, 
                             data = .)))

SAT_models$model[[1]]

SAT_models_tidy <- SAT_models %>%
  mutate(
    coef_info = map(model, tidy))

SAT_coef <- SAT_models_tidy %>%
  unnest(coef_info)
SAT_coef

coef <- SAT_coef %>% 
  filter(term == "total_safety_and_respect_score")
coef

#hist of coefficient
coef %>%
  ggplot(aes(x=estimate)) +
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 bins=15, 
                 fill="lightblue2", col="lightblue3")+
  geom_vline(aes(xintercept=mean(estimate)),
             color="blue",
             linetype="dashed")+
  labs(title="Bootstrap resample estimates",
       x="Coefficient estimates",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

library(rsample)
int_pctl(SAT_models_tidy, coef_info, alpha = 0.05)

#
SAT_aug <- SAT_models_tidy %>%
  mutate(augmented = map(model, augment))%>%
  unnest(augmented)

length(sat_info_2_regression$`avg_sat_sum`)

firstboot<-filter(SAT_aug,id=="Bootstrap0001")

firstbootlength <- firstboot %>%
  dplyr::select(avg_sat_sum)%>%
  pull()%>%
  length()

firstboot$coef_info

uniquecoefficent <- firstboot %>%
  dplyr::select(coef_info)%>%
  unnest(coef_info)%>%
  distinct()

uniquecoefficent

#show the possible variance.
ggplot(SAT_aug, aes(total_safety_and_respect_score,
                    avg_sat_sum))+
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "cyan3") +  
  geom_point(data=filter(SAT_aug,id=="Apparent"))+
  labs(x="total_safety_and_respect_score",
       y="avg_sat_sum")

# 
ggplot(sat_info_2_regression, aes(x=total_safety_and_respect_score)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.3) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(sat_info_2_regression, aes(x=avg_sat_sum)) +
  geom_histogram(aes(y = ..density..),
                 binwidth = 70) + 
  geom_density(colour="red",
               size=1, 
               adjust=1)

#5.2 residual
model_data <- model1 %>%
  augment(., Regressiondata)

sat_info_2_residual <- model_data %>%
  mutate(model1resids = residuals(model1))

########################
sat_info_2_residual_no_geo <- sat_info_2_residual %>%
  dplyr::select(`avg_sat_sum`,`total_safety_and_respect_score`,`model1resids`)

sat_info_2_residual <- read.csv("residual2.csv", 
                       header = TRUE, sep = ",", encoding = "latin1")%>%
  clean_names()
###

sat_info_3_residual <- nyc_outline %>%
  merge(.,
        sat_info_2_residual, 
        by.x="ZIPCODE", 
        by.y="x_ef_bb_bf_zipcode",
        no.dups = TRUE)



nyc_school_map_2 <- nyc_school_map %>%
  na.omit(nyc_school_map)
####################################

model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram()

#
par(mar = rep(2, 4))
plot(model1)

# DW test
DW <- durbinWatsonTest(model1)
tidy(DW)

### Residual map
tmap_mode("view")
tm_shape(sat_info_3_residual) +
  tm_polygons("model1resids",
              palette = "RdYlBu")


qtm(sat_info_3_residual, fill = "model1resids")
  
