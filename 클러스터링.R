getwd()
setwd("C:/Users/Sim/Desktop/빅캠공모전")
set.seed(42)

########### 1.k-means Clustering
library(clValid) # 클러스터의 타당성 지표 평가
library(plotrix) # 클러스터링의 결과물의 차이 확인을 위한 레이더 차트


library(factoextra)
library(cluster)
library(hms)


#경제
data = read.csv("restart_plus_social.csv")
data=data[,-1]

data['eco_num']=data_economic[,7]+data_economic[,8]
str(data)

data_ft = data[,c(9,12)] #클러스터링 변수 추출
data_ft_scaled = scale(data_ft, center=TRUE, scale=TRUE)
str(data_ft)

tot_withinss = c() #군집내 중심과 개체간 거리의 제곱합들의 총합 저장할 공간
for (i in 1:10){
  set.seed(42)
  km_cl <- kmeans(data_ft_scaled,centers = i)
  tot_withinss[i] = km_cl$tot.withinss
}

plot(c(1:10), tot_withinss, type="b" ,
     main="Optimal number of clusters",
     xlab = "Number of clusters",
     ylab = "Total within-cluster sum of squares")

#1.2 분산비율이용
r2 <- c()
for (i in 1:10){
  set.seed(42) # for reproducibility
  km_cl <- kmeans(data_ft_scaled, centers = i)
  r2[i] <- km_cl$betweenss / km_cl$totss
}

plot(c(1:10), r2, type="b",
     main="The Elbow Method - Percentage of Variance Explained",
     xlab="Number of clusters",
     ylab="Percentage of Variance Explained")

## 2. silhouette 이용
library(factoextra)
km_cl = kmeans(data_ft_scaled, centers = 3)  
sil = silhouette(km_cl$cluster, dist(data_ft_scaled))
fviz_silhouette(sil) # center 10? 까지 돌려가면서 average확인



## k-means 클러스터링 결과 시각화
km_cl <- kmeans(data_ft_scaled,centers = 3, nstart=25)#center수 결정값 대입 

fviz_cluster(km_cl, data_ft_scaled, ellipse = TRUE, geom = "point")
#factoextra 에서 제공하는 시각화 함수 ellipse : 군집의 경계선 표현여부 

#클러스터링 결과 시각화 ver2
par(mfrow=c(1,2))
plot(km_cl.result)


## km 군집화 군집간 특성 비교 
#군집화 결과 원래 데이터에 삽입 
data_km_cl = data.frame(data_ft_scaled, cluster_ID = as.factor(km_cl$cluster))
kmc_summary = data.frame()


for (i in 1:(ncol(data_km_cl)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(data_km_cl[,i], data_km_cl$cluster_ID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3)) #클러스터 수 
rownames(kmc_summary) <- colnames(data_ft_scaled)

kmc_summary


#원래 데이터 확인해보기 
data$cluster_m = km_cl$cluster
data_economic = data
str(data_economic)
a = data[,c(1,2,9,12,18,19)]
str(a)
a[a$cluster == 1,]

####### K-medoid
#install.packages('fpc')
library(fpc)

km_cl2 <- pam(data_ft_scaled,4)#center수 결정값 대입 

tot_withinss = c() #군집내 중심과 개체간 거리의 제곱합들의 총합 저장할 공간
for (i in 1:10){
  set.seed(42)
  km_cl2 <- pam(data_ft_scaled,centers = i)
  tot_withinss[i] = km_cl2$tot.withinss
}

plot(c(1:10), tot_withinss, type="b" ,
     main="Optimal number of clusters",
     xlab = "Number of clusters",
     ylab = "Total within-cluster sum of squares")

#1.2 분산비율이용
r2 <- c()
for (i in 1:10){
  set.seed(42) # for reproducibility
  km_cl <- kmeans(data_ft_scaled, centers = i)
  r2[i] <- km_cl$betweenss / km_cl$totss
}

plot(c(1:10), r2, type="b",
     main="The Elbow Method - Percentage of Variance Explained",
     xlab="Number of clusters",
     ylab="Percentage of Variance Explained")

## 2. silhouette 이용
library(factoextra)
km_cl2 = pam(data_ft_scaled, centers = 3)  
sil = silhouette(km_cl2$cluster, dist(data_ft_scaled))
fviz_silhouette(sil) # center 10? 까지 돌려가면서 average확인

#클러스터 시각화
fviz_cluster(km_cl2, data_ft_scaled, ellipse = TRUE, geom = "point")

#클러스터 특성확인
data_md_cl = data.frame(data_ft_scaled, cluster_ID = as.factor(km_cl2$cluster))
kmc_summary = data.frame()


for (i in 1:(ncol(data_md_cl)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(data_md_cl[,i], data_md_cl$cluster_ID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:4)) #클러스터 수 
rownames(kmc_summary) <- colnames(data_ft_scaled)

kmc_summary


#원래 데이터 확인해보기 
data$cluster_md = km_cl2$cluster
data_economic = data
str(data_economic)
a = data[,c(1,2,9,12,18,19,20)]
str(a)
a[a$cluster_md == 4,]
## 경제 분석 결과 
## k-means : 관악구, 동작구, 영등포구, 은평구 
## k-medoid : 동작구 영등포구 은평구



#건강
data = read.csv("restart_plus_social.csv")
data=data[,-1]
str(data)

data_ft = data[,c(10,11)] #클러스터링 변수 추출
data_ft_scaled = scale(data_ft, center=TRUE, scale=TRUE)
str(data_ft)

tot_withinss = c() #군집내 중심과 개체간 거리의 제곱합들의 총합 저장할 공간
for (i in 1:10){
  set.seed(42)
  km_cl <- kmeans(data_ft_scaled,centers = i)
  tot_withinss[i] = km_cl$tot.withinss
}

plot(c(1:10), tot_withinss, type="b" ,
     main="Optimal number of clusters",
     xlab = "Number of clusters",
     ylab = "Total within-cluster sum of squares")

#1.2 분산비율이용
r2 <- c()
for (i in 1:10){
  set.seed(42) # for reproducibility
  km_cl <- kmeans(data_ft_scaled, centers = i)
  r2[i] <- km_cl$betweenss / km_cl$totss
}

plot(c(1:10), r2, type="b",
     main="The Elbow Method - Percentage of Variance Explained",
     xlab="Number of clusters",
     ylab="Percentage of Variance Explained")

## 2. silhouette 이용
library(factoextra)
km_cl = kmeans(data_ft_scaled, centers = 3)  
sil = silhouette(km_cl$cluster, dist(data_ft_scaled))
fviz_silhouette(sil) # center 10? 까지 돌려가면서 average확인



## k-means 클러스터링 결과 시각화
km_cl <- kmeans(data_ft_scaled,centers = 3, nstart=25)#center수 결정값 대입 

fviz_cluster(km_cl, data_ft_scaled, ellipse = TRUE, geom = "point")
#factoextra 에서 제공하는 시각화 함수 ellipse : 군집의 경계선 표현여부 

#클러스터링 결과 시각화 ver2
par(mfrow=c(1,2))
plot(km_cl.result)


## km 군집화 군집간 특성 비교 
#군집화 결과 원래 데이터에 삽입 
data_km_cl = data.frame(data_ft_scaled, cluster_ID = as.factor(km_cl$cluster))
kmc_summary = data.frame()


for (i in 1:(ncol(data_km_cl)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(data_km_cl[,i], data_km_cl$cluster_ID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3)) #클러스터 수 
rownames(kmc_summary) <- colnames(data_ft_scaled)

kmc_summary


#원래 데이터 확인해보기 
data$cluster_mean = km_cl$cluster
data_health = data
str(data_health)
a = data[,c(1,2,9,12,18)]
str(a)
data_health[data_health$cluster_mean == 2,1]

####### K-medoid
km_cl2 <- pam(data_ft_scaled,4)

## 2. silhouette 이용
library(factoextra)
km_cl2 = pam(data_ft_scaled,3)  
sil = silhouette(km_cl2$cluster, dist(data_ft_scaled))
fviz_silhouette(sil) # center 10? 까지 돌려가면서 average확인

#클러스터 시각화
fviz_cluster(km_cl2, data_ft_scaled, ellipse = TRUE, geom = "point")

#클러스터 특성확인
data_md_cl = data.frame(data_ft_scaled, cluster_ID = as.factor(km_cl2$cluster))
kmc_summary = data.frame()


for (i in 1:(ncol(data_md_cl)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(data_md_cl[,i], data_md_cl$cluster_ID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3)) #클러스터 수 
rownames(kmc_summary) <- colnames(data_ft_scaled)

kmc_summary


#원래 데이터 확인해보기 
data$cluster_md = km_cl2$cluster
data_health = data
str(data_health)
data_health[data_health$cluster_md == 2,1]
## 건강 분석 결과 
## k-means : 강북구 강서구 구로구 금천구 노원구 도봉구 동대문구 성북구 송파구 양천구  
## k-medoid : 강북구 강서구 구로구 금천구 노원구 도봉구 동대문구 성북구 송파구 양천구 


#여가
data = read.csv("restart_plus_social.csv")
data=data[,-1]
str(data)

data_ft = data[,c(13,17)] #클러스터링 변수 추출
data_ft_scaled = scale(data_ft, center=TRUE, scale=TRUE)
str(data_ft)

tot_withinss = c() #군집내 중심과 개체간 거리의 제곱합들의 총합 저장할 공간
for (i in 1:10){
  set.seed(42)
  km_cl <- kmeans(data_ft_scaled,centers = i)
  tot_withinss[i] = km_cl$tot.withinss
}

plot(c(1:10), tot_withinss, type="b" ,
     main="Optimal number of clusters",
     xlab = "Number of clusters",
     ylab = "Total within-cluster sum of squares")

#1.2 분산비율이용
r2 <- c()
for (i in 1:10){
  set.seed(42) # for reproducibility
  km_cl <- kmeans(data_ft_scaled, centers = i)
  r2[i] <- km_cl$betweenss / km_cl$totss
}

plot(c(1:10), r2, type="b",
     main="The Elbow Method - Percentage of Variance Explained",
     xlab="Number of clusters",
     ylab="Percentage of Variance Explained")

## 2. silhouette 이용
library(factoextra)
km_cl = kmeans(data_ft_scaled, centers = 5)  
sil = silhouette(km_cl$cluster, dist(data_ft_scaled))
fviz_silhouette(sil) # center 10? 까지 돌려가면서 average확인



## k-means 클러스터링 결과 시각화
km_cl <- kmeans(data_ft_scaled,centers = 5, nstart=25)#center수 결정값 대입 

fviz_cluster(km_cl, data_ft_scaled, ellipse = TRUE, geom = "point")
#factoextra 에서 제공하는 시각화 함수 ellipse : 군집의 경계선 표현여부 


## km 군집화 군집간 특성 비교 
#군집화 결과 원래 데이터에 삽입 
data_km_cl = data.frame(data_ft_scaled, cluster_ID = as.factor(km_cl$cluster))
kmc_summary = data.frame()


for (i in 1:(ncol(data_km_cl)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(data_km_cl[,i], data_km_cl$cluster_ID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:5)) #클러스터 수 
rownames(kmc_summary) <- colnames(data_ft_scaled)

kmc_summary


#원래 데이터 확인해보기 
data$cluster_mean = km_cl$cluster
data_social = data
str(data_social)
a = data[,c(1,2,9,12,18)]
str(a)
data_social[data_social$cluster_mean ==3 ,1]

####### K-medoid
km_cl2 <- pam(data_ft_scaled,5)

## 2. silhouette 이용
library(factoextra)
km_cl2 = pam(data_ft_scaled,5)  
sil = silhouette(km_cl2$cluster, dist(data_ft_scaled))
fviz_silhouette(sil) # center 10? 까지 돌려가면서 average확인

#클러스터 시각화
fviz_cluster(km_cl2, data_ft_scaled, ellipse = TRUE, geom = "point")

#클러스터 특성확인
data_md_cl = data.frame(data_ft_scaled, cluster_ID = as.factor(km_cl2$cluster))
kmc_summary = data.frame()


for (i in 1:(ncol(data_md_cl)-1)){
  kmc_summary = rbind(kmc_summary, 
                      tapply(data_md_cl[,i], data_md_cl$cluster_ID, mean))
}

colnames(kmc_summary) <- paste("cluster", c(1:3)) #클러스터 수 
rownames(kmc_summary) <- colnames(data_ft_scaled)

kmc_summary


#원래 데이터 확인해보기 
data$cluster_md = km_cl2$cluster
data_social = data
str(data_social)
data_social[data_social$cluster_md == 2,1]
## 건강 분석 결과 
## k-means : "강북구"   "동대문구" "종로구"   
## k-medoid : "강북구"   "동대문구" "종로구"  



