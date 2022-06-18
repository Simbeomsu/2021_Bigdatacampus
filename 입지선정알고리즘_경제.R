#입지선정 : p-median 알고리즘 

#데이터 읽어오기 
library(tbart) # P-Median 함수가 저장되어 있는 패키지
library(geosphere) # 위경도로 거리를 구할 수 있는 패키지
library(data.table)
library(tidyverse)
library(readr)

getwd()
setwd("C:/Users/Sim/Desktop/빅캠공모전/data")

# 거리계산행렬
make_dist_mat <- function(lon, lat, p_median) {
  n = length(lon)
  dist_mat <- matrix(NA, n, n)
  for (i in 1:n) {
    lon1 <- lon[i]
    lat1 <- lat[i]
    if (i %% 20 == 0) {
      cat(i, '')
    }
    for (j in 1:n) {
      lon2 <- lon[j]
      lat2 <- lat[j]
      dist_mat[i, j] <- distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    }
  } # 거리를 1대1 대응을 통해 구함 
  #결과는 1번 1번 거리 1번2번 거리 1번 3번 거리 
  return(dist_mat)
}


p_median <- function(distance_matrix, num_p, seed) {
  set.seed(seed) # 시드 지정
  initial_value = sample(1:nrow(distance_matrix), num_p) # 초기값을 랜덤으로 p개 선정
  result_vec <- tb.raw(distance_matrix, initial_value, T)  # 초기값 넣어서 p_median 알고리즘 적용
  distance_matrix[-result_vec, -result_vec] <- 0
  distance_matrix[result_vec, ] <- 0 # 중심점들과 연결점들만 남김
  
  for_vec <-  1:nrow(distance_matrix)
  for_vec <- for_vec %>% setdiff(result_vec)
  for (i in for_vec) {
    a <- distance_matrix[i, result_vec]
    a[which(a != distance_matrix[i, result_vec] %>% min)] <- 0 
    # 중심점과 연결점의 조합 중 최단거리만 남김
    distance_matrix[i, result_vec] <- a 
  }
  return(distance_matrix)
}

################### 여기까진 함수정의 ######################

### 경제(여의동 상도1동 진관동)
##송천동 
# 데이터 불러오기
options(digits=10)
dong = "여의동"

metro = fread('경제지하철위치2.csv')
bus = fread("경제버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("경제건물.csv")



building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()
metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()


#distm(c(127.0679, 37.64788 ), c(127.0642461545723, 37.64488842684196), fun = distHaversine)
#상명초등학교와 상계역 거리는 461m임.


#버스정거장 반경 50m 내의 빌딩들 체크 

building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(bus_lat)){
  tem_bus_lon <- as.numeric(bus$X_COORD[i]) 
  tem_bus_lat <- as.numeric(bus$Y_COORD[i])
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_bus_lon, tem_bus_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon) # 67
length(unique(building_bus_lon)) #32
# => 어떤 건물이 a정거장과 b정거장에 동시에 있음. 
# 해당구역 건물 : 441개 건물

lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)
#추가사항? 버스 두정류장 이상이면 3배까지 할까?

length(building_lat)

building_lat = c(building_lat,lat_list)
building_lon = c(building_lon,lon_list)

length(building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(building_lon, building_lat)

### P-median알고리즘을 중심수를 변화해가며 적용
dobong1_1 = p_median(dist_mat, 1, seed = 42)

building_lat[94]
building_lon[94]


### 경제 (여의동 상도1동 진관동)
##상도1동 
# 데이터 불러오기
options(digits=10)
dong = "송천동"

metro = fread('건강지하철위치2.csv')
bus = fread("건강버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("건강건물.csv")



building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()
metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()


#distm(c(127.0679, 37.64788 ), c(127.0642461545723, 37.64488842684196), fun = distHaversine)
#상명초등학교와 상계역 거리는 461m임.


#버스정거장 반경 50m 내의 빌딩들 체크 

building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(bus_lat)){
  tem_bus_lon <- as.numeric(bus$X_COORD[i]) 
  tem_bus_lat <- as.numeric(bus$Y_COORD[i])
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_bus_lon, tem_bus_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon) # 67
length(unique(building_bus_lon)) #32
# => 어떤 건물이 a정거장과 b정거장에 동시에 있음. 
# 해당구역 건물 : 441개 건물

lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)
#추가사항? 버스 두정류장 이상이면 3배까지 할까?

length(building_lat)

building_lat = c(building_lat,lat_list)
building_lon = c(building_lon,lon_list)

length(building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(building_lon, building_lat)

### P-median알고리즘을 중심수를 변화해가며 적용
dobong1_1 = p_median(dist_mat, 1, seed = 42)

building_lat[94]
building_lon[94]


### 경제 (여의동 상도1동 진관동)
##진관동
# 데이터 불러오기
options(digits=10)
dong = "잠실3동"

metro = fread('건강지하철위치2.csv')
bus = fread("건강버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("건강건물.csv")



building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()
metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()


#distm(c(127.0679, 37.64788 ), c(127.0642461545723, 37.64488842684196), fun = distHaversine)
#상명초등학교와 상계역 거리는 461m임.


#버스정거장 반경 50m 내의 빌딩들 체크 

building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(bus_lat)){
  tem_bus_lon <- as.numeric(bus$X_COORD[i]) 
  tem_bus_lat <- as.numeric(bus$Y_COORD[i])
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_bus_lon, tem_bus_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon) # 67
length(unique(building_bus_lon)) #32
# => 어떤 건물이 a정거장과 b정거장에 동시에 있음. 
# 해당구역 건물 : 441개 건물

lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)
#추가사항? 버스 두정류장 이상이면 3배까지 할까?

length(building_lat)

building_lat = c(building_lat,lat_list)
building_lon = c(building_lon,lon_list)

length(building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(building_lon, building_lat)

### P-median알고리즘을 중심수를 변화해가며 적용
dobong1_1 = p_median(dist_mat, 2, seed = 42)
dobong1_1 = p_median(dist_mat, 3, seed = 42)

dobong1_1 = p_median(dist_mat, 4, seed = 42)

dobong1_1 = p_median(dist_mat, 5, seed = 42)

building_lat[3462]
building_lon[3462]










