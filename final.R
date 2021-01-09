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

#1.数据输入和整理
#1.1读取sat成绩文件
sat_result <- read.csv("sat.csv", 
                       header = TRUE, sep = ",", encoding = "latin1")%>%
  clean_names()


#1.2读取高中评分数据
school_survey <- read.csv("sat-survey.csv", 
                          header = TRUE, sep = ",", encoding = "latin1")%>%
  clean_names()


#1.3连结高中评分数据和SAT成绩---连结后样本容量为416
result_survey <- sat_result %>%
  merge(.,
        school_survey, 
        by.x="dbn", 
        by.y="x_ef_bb_bf_dbn",
        no.dups = TRUE)%>%
  distinct(.,dbn, 
           .keep_all = TRUE)%>%
  clean_names()

#1.4输入高中的地理位置文件
school_dictionary <- read.csv("directory.csv", 
                            header = TRUE, sep = ",", encoding = "latin1")%>%
  clean_names()


#1.5连结高中的地理位置和学校成绩----形成参与了考SAT的学校的完整信息--332个样本
sat_info_1 <- school_dictionary %>%
  merge(.,
        result_survey, 
        by.x="dbn", 
        by.y="dbn",
        no.dups = TRUE)%>%
  distinct(.,dbn, 
           .keep_all = TRUE)%>%
  clean_names()

#1.6增加两个变量，总SAT的平均分以及高于和低于平均分的学校分类

sat_info_2 <- sat_info_1 %>%
  mutate(avg_sat_sum = (sat_critical_reading_avg_core + sat_math_avg_core + sat_writing_avg_core ))%>%
  mutate(total_average_sat= sum(avg_sat_sum)/332)

#1.7我想要知道高中在纽约市的分布，加载纽约市地图 #3857
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

#1.8绘制sat图
tmap_mode("view")
qtm(nyc_school_map, 
    fill = "avg_sat_sum", 
    borders = NULL,  
    fill.palette = "Blues")

tmap_mode("plot")
nyc_school_map %>%
  qtm(.,fill = "avg_sat_sum")

#1.8 根据尊重和安全的评分在空间展示
tmap_mode("view")
qtm(nyc_school_map, 
    fill = "total_safety_and_respect_score", 
    borders = NULL,  
    fill.palette = "Blues")

#1.9选择高于平均SAT分数,n=119
sat_good_performance <- sat_info_2 %>%
  filter(`avg_sat_sum`>1222.491)%>%
  clean_names()#n=119

#1.10根据zipcode出现过的次数
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

#2.空间格局分析
#这些较高的SAT在空间分布上的特点
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

##转换空间
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

####以上是想知道是否有任何的空间格局kinds of spatial patterning associated with the 
###excellent higher school in the are of london 
###在伦敦的优秀高中分布是否有某种空间格局类型
###将我们观察到的点和
###以上的公式The formula for calculating expected probabilities based on the Poisson distribution
###有一个相反的趋势，观察到的点是在起点高频率在结束的时候很低
###这和基于泊尔森分布的预测点是相反趋势，因此我们的模型可能有点的聚类或分散。
###测试

teststats <- quadrat.test(sat_good_performeance_spaces_2.ppp, nx = 6, ny = 6)

plot(sat_good_performeance_spaces_2.ppp,pch=16,cex=0.5, main="Excellent high school in New York City")
plot(teststats, add=T, col = "red")

#Ripley’s K----克服不准确性
K <- sat_good_performeance_spaces_2.ppp %>%
  Kest(., correction="border") %>%
  plot()
##从0到大约5300米似乎都是聚集cluster在纽约城，然后大概在五千五百米处到七千米左右就是分布随意和离散的dispersed.
##是否存在空间聚集

##DBSCAN
st_geometry(nyc_outline)
##我们可以看到我们正在聚类到半径5300m左右，图中最大的凸起在大约2500米和5250m，
##因此选取2500m
##因此，2500m可能是一个不错的起点，我们将从搜索至少4个点的集群开始……

sat_good_performeance_spaces_2.points <- sat_good_performeance_spaces %>%
  coordinates(.)%>%
  as.data.frame()

db <- sat_good_performeance_spaces_2.points %>%
  fpc::dbscan(.,eps = 2500, MinPts = 5)

plot(db, sat_good_performeance_spaces_2.points, main = "DBSCAN Output", frame = F)
plot(nyc_outline$geometry, add=T)

##寻找合适的eps值----

library(dbscan)

sat_good_performeance_spaces_2.points%>%
  dbscan::kNNdistplot(.,k=5) 

###The distance where you have a sharp change in curve is your epsilon
##似乎有五个cluster

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

#删除0的点
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

#3.1空间自相关
#转城坐标
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

#3.3需要计算落入每一个邮政区域的学校
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

#3.4查看发展情况--很小很小
tm_shape(points_sf_joined_d1_3) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("density"),
              title="excellent School Density")

#3.5验证-Moran'I
library(spdep)

#3.5.1calculate the centroids of all excellent high school in newyor计算质心
coordsW <- points_sf_joined_d1_3%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

#3.6generate a spatial weights matrix 创造空间权重矩阵
NYC_nb <- points_sf_joined_d1_3 %>%
  poly2nb(., queen=T)

plot(NYC_nb, st_geometry(coordsW), col="red")
plot(points_sf_joined_d1_3$geometry, add=T)

#3.7根据该矩阵创造空间权重实体
NYC.lw <- NYC_nb %>%
  nb2listw(., style="C",zero.policy = TRUE)
##this means that at least on site has no neighbours

#3.8M‘I 
I_Global_Density_ <- points_sf_joined_d1_3 %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(.,NYC.lw, zero.policy=TRUE)

I_Global_Density_

##解释：接近1是聚集，接近-1是分散。I = 0.0773

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

#解释
#Moran的I统计量= （记住1 =聚集，0 =无模式，-1 =分散），这表明我们有一些独特的聚集
#Geary的C统计量= （请记住Geary的C介于0和2之间； 1表示无空间自相关，小于1正空间自相关或相似值聚类，大于1负空间自相关或异值聚类），表明相似值是聚类
#一般G统计量= G>预期值，因此高值趋于聚集。


#4.回归分析
#4.看对学校的安全和尊重和学生的sat成绩有没有关系
#零假设-两者没有关系
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

#4.1回归模型

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
##解释

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

#安全性系数的直方图
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

#将broom和原始数据进行比较
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

#绘制所有线show the possible variance.
ggplot(SAT_aug, aes(total_safety_and_respect_score,
                    avg_sat_sum))+
  geom_line(aes(y = .fitted, group = id), alpha = .2, col = "cyan3") +  
  geom_point(data=filter(SAT_aug,id=="Apparent"))+
  labs(x="total_safety_and_respect_score",
       y="avg_sat_sum")

#正态分布
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

#5.2 模型的残差图
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
###连结

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

#检验是否存在异方差或者同方差
par(mar = rep(2, 4))
plot(model1)

#测试自相关
DW <- durbinWatsonTest(model1)
tidy(DW)

###残差图
tmap_mode("view")
tm_shape(sat_info_3_residual) +
  tm_polygons("model1resids",
              palette = "RdYlBu")


qtm(sat_info_3_residual, fill = "model1resids")
  