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

### 건강 (송천동 잠실3동 개봉2동 목5동 월계2동)
##송천동
# 데이터 불러오기
options(digits=10)
dong = "송천동"

metro = fread('건강지하철위치3.csv')
bus = fread("건강버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("건강건물3.csv")

주택 = building %>% filter(행정동== dong) %>% filter(주거시설 == 1)
주택_lat = 주택$위도
주택_lon = 주택$경도
length(주택_lat)

building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)

metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()

public_lat = c(bus_lat,metro_lat)
public_lon = c(bus_lon, metro_lon)

#버스정거장 반경 50m 내의 빌딩들 체크 
building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(public_lat)){
  tem_public_lat <- public_lat[i]
  tem_public_lon <- public_lon[i]
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_public_lon, tem_public_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon)
length(unique(building_bus_lon))


lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)


length(building_lat)

f_building_lat = c(building_lat,lat_list,주택_lat)
f_building_lon = c(building_lon,lon_list,주택_lon)

length(f_building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(f_building_lon, f_building_lat)

#p-median 알고리즘 적용 
result = p_median(dist_mat, 1, seed = 42)

first_lat = f_building_lat[1621]
first_lon = f_building_lon[1621]


##선정지역 근방 100미터 건물 제외 후 적합

del_lat = c()
del_lon = c()
for (j in 1:length(f_building_lat)){
  distance <- distm(c(first_lon, first_lat), c(f_building_lon[j], f_building_lat[j]), fun = distHaversine)
  if(distance < 300){
    del_lat <- c(del_lat, f_building_lat[j])
    del_lon <- c(del_lon, f_building_lon[j]) 
  }
}

f2_building_lat = f_building_lat [!f_building_lat %in% del_lat]
f2_building_lon = f_building_lon [!f_building_lon %in% del_lon]

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat2 = make_dist_mat(f2_building_lon, f2_building_lat)

#p-median 알고리즘 적용 
result2 = p_median(dist_mat2, 1, seed = 42)

second_lat = f2_building_lat[498]
second_lon = f2_building_lon[498]

### 건강 (송천동 잠실3동 개봉2동 목5동 월계2동)
##잠실3동 
# 데이터 불러오기
options(digits=10)
dong = "잠실3동"

metro = fread('건강지하철위치3.csv')
bus = fread("건강버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("건강건물3.csv")

주택 = building %>% filter(행정동== dong) %>% filter(주거시설 == 1)
주택_lat = 주택$위도
주택_lon = 주택$경도
length(주택_lat)

building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)

metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()

public_lat = c(bus_lat,metro_lat)
public_lon = c(bus_lon, metro_lon)

#버스정거장 반경 50m 내의 빌딩들 체크 
building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(public_lat)){
  tem_public_lat <- public_lat[i]
  tem_public_lon <- public_lon[i]
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_public_lon, tem_public_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon)
length(unique(building_bus_lon))


lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)


length(building_lat)

f_building_lat = c(building_lat,lat_list,주택_lat,주택_lat)
f_building_lon = c(building_lon,lon_list,주택_lon,주택_lon)

length(f_building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(f_building_lon, f_building_lat)

#p-median 알고리즘 적용 
result = p_median(dist_mat, 1, seed = 42)

first_lat = f_building_lat[27]
first_lon = f_building_lon[27]
cat(first_lat,first_lon )


##선정지역 근방 100미터 건물 제외 후 적합

del_lat = c()
del_lon = c()
for (j in 1:length(f_building_lat)){
    distance <- distm(c(first_lon, first_lat), c(f_building_lon[j], f_building_lat[j]), fun = distHaversine)
    if(distance < 500){
      del_lat <- c(del_lat, f_building_lat[j])
      del_lon <- c(del_lon, f_building_lon[j]) 
  }
}

f2_building_lat = f_building_lat [!f_building_lat %in% del_lat]
f2_building_lon = f_building_lon [!f_building_lon %in% del_lon]

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat2 = make_dist_mat(f2_building_lon, f2_building_lat)

#p-median 알고리즘 적용 
result2 = p_median(dist_mat2, 1, seed = 42)

second_lat = f2_building_lat[18]
second_lon = f2_building_lon[18]
cat(second_lat, ',' ,second_lon)


### 건강 (송천동 잠실3동 개봉2동 목5동 월계2동)
##개봉2동 
# 데이터 불러오기
options(digits=10)
dong = "개봉2동"

metro = fread('건강지하철위치3.csv')
bus = fread("건강버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("건강건물3.csv")
unique(building$행정동)

주택 = building %>% filter(행정동== dong) %>% filter(주거시설 == 1)
주택_lat = 주택$위도
주택_lon = 주택$경도
length(주택_lat)

building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)

metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()

public_lat = c(bus_lat,metro_lat)
public_lon = c(bus_lon, metro_lon)

#버스정거장 반경 50m 내의 빌딩들 체크 
building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(public_lat)){
  tem_public_lat <- public_lat[i]
  tem_public_lon <- public_lon[i]
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_public_lon, tem_public_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon)
length(unique(building_bus_lon))


lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)


length(building_lat)

f_building_lat = c(building_lat,lat_list,주택_lat)
f_building_lon = c(building_lon,lon_list,주택_lon)

length(f_building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(f_building_lon, f_building_lat)

#p-median 알고리즘 적용 
result = p_median(dist_mat, 1, seed = 42)

first_lat = f_building_lat[463]
first_lon = f_building_lon[463]
cat(first_lat, first_lon)


##선정지역 근방 500미터 건물 제외 후 적합

del_lat = c()
del_lon = c()
for (j in 1:length(f_building_lat)){
  distance <- distm(c(first_lon, first_lat), c(f_building_lon[j], f_building_lat[j]), fun = distHaversine)
  if(distance < 500){
    del_lat <- c(del_lat, f_building_lat[j])
    del_lon <- c(del_lon, f_building_lon[j]) 
  }
}

f2_building_lat = f_building_lat [!f_building_lat %in% del_lat]
f2_building_lon = f_building_lon [!f_building_lon %in% del_lon]

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat2 = make_dist_mat(f2_building_lon, f2_building_lat)

#p-median 알고리즘 적용 
result2 = p_median(dist_mat2, 1, seed = 42)

second_lat = f2_building_lat[48]
second_lon = f2_building_lon[48]
cat(second_lat,',',second_lon)








### 건강 (송천동 잠실3동 개봉2동 목5동 월계2동)
##목5동 
# 데이터 불러오기
options(digits=10)
dong = "목5동"

metro = fread('건강지하철위치3.csv')
bus = fread("건강버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("건강건물3.csv")
unique(building$행정동)

주택 = building %>% filter(행정동== dong) %>% filter(주거시설 == 1)
주택_lat = 주택$위도
주택_lon = 주택$경도
length(주택_lat)

building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)

metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()

public_lat = c(bus_lat,metro_lat)
public_lon = c(bus_lon, metro_lon)

#버스정거장 반경 50m 내의 빌딩들 체크 
building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(public_lat)){
  tem_public_lat <- public_lat[i]
  tem_public_lon <- public_lon[i]
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_public_lon, tem_public_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon)
length(unique(building_bus_lon))


lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)


length(building_lat)

f_building_lat = c(building_lat,lat_list,주택_lat)
f_building_lon = c(building_lon,lon_list,주택_lon)

length(f_building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(f_building_lon, f_building_lat)

#p-median 알고리즘 적용 
result = p_median(dist_mat, 1, seed = 42)

first_lat = f_building_lat[157]
first_lon = f_building_lon[157]
cat(first_lat, first_lon)


##선정지역 근방 500미터 건물 제외 후 적합

del_lat = c()
del_lon = c()
for (j in 1:length(f_building_lat)){
  distance <- distm(c(first_lon, first_lat), c(f_building_lon[j], f_building_lat[j]), fun = distHaversine)
  if(distance < 500){
    del_lat <- c(del_lat, f_building_lat[j])
    del_lon <- c(del_lon, f_building_lon[j]) 
  }
}

f2_building_lat = f_building_lat [!f_building_lat %in% del_lat]
f2_building_lon = f_building_lon [!f_building_lon %in% del_lon]

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat2 = make_dist_mat(f2_building_lon, f2_building_lat)

#p-median 알고리즘 적용 
result2 = p_median(dist_mat2, 1, seed = 42)

second_lat = f2_building_lat[150]
second_lon = f2_building_lon[150]
cat(second_lat,',',second_lon)





### 건강 (송천동 잠실3동 개봉2동 목5동 월계2동)
##월계2동 
# 데이터 불러오기
options(digits=10)
dong = "월계2동"

metro = fread('건강지하철위치3.csv')
bus = fread("건강버정위치.csv")
welfare = fread("기존복지시설.csv")
building = fread("건강건물3.csv")
unique(building$행정동)

주택 = building %>% filter(행정동== dong) %>% filter(주거시설 == 1)
주택_lat = 주택$위도
주택_lon = 주택$경도
length(주택_lat)

building_lat = building %>% filter(행정동 == dong) %>% select(위도) %>% unlist() %>% as.vector()
building_lon = building %>% filter(행정동 == dong) %>% select(경도) %>% unlist() %>% as.vector()
length(building_lat)

bus_lat = bus %>% filter(행정동 == dong) %>% select(Y_COORD) %>% unlist() %>% as.vector()
bus_lon = bus %>% filter(행정동 == dong) %>% select(X_COORD) %>% unlist() %>% as.vector()
length(bus_lat)

metro_lat = metro %>% filter(dong == dong) %>% select(위도) %>% unlist() %>% as.vector()
metro_lon = metro %>% filter(dong == dong) %>% select(경도) %>% unlist() %>% as.vector()

public_lat = c(bus_lat,metro_lat)
public_lon = c(bus_lon, metro_lon)

#버스정거장 반경 50m 내의 빌딩들 체크 
building_bus_lon = c()
building_bus_lat = c()
for (i in 1:length(public_lat)){
  tem_public_lat <- public_lat[i]
  tem_public_lon <- public_lon[i]
  cat(i,"번 버스정류장")
  cat("\n")
  for (j in 1:length(building_lat)){
    distance <- distm(c(tem_public_lon, tem_public_lat), c(building_lon[j], building_lat[j]), fun = distHaversine)
    if(distance < 50){
      cat(c(building_lon[j],building_lat[j]))
      building_bus_lon <- c(building_bus_lon, building_lon[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
      building_bus_lat <- c(building_bus_lat, building_lat[j]) # 거리 50m 보다 작으면 데이터프레임에 추가
    }
  }
  cat("\n")
}

length(building_bus_lon)
length(unique(building_bus_lon))


lon_list = unique(building_bus_lon)
lat_list = unique(building_bus_lat)


length(building_lat)

f_building_lat = c(building_lat,lat_list,주택_lat,주택_lat)
f_building_lon = c(building_lon,lon_list,주택_lon,주택_lon)

length(f_building_lat)

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat = make_dist_mat(f_building_lon, f_building_lat)

#p-median 알고리즘 적용 
result = p_median(dist_mat, 1, seed = 42)

first_lat = f_building_lat[40]
first_lon = f_building_lon[40]
cat(first_lat,',', first_lon)


##선정지역 근방 200미터 건물 제외 후 적합

del_lat = c()
del_lon = c()
for (j in 1:length(f_building_lat)){
  distance <- distm(c(first_lon, first_lat), c(f_building_lon[j], f_building_lat[j]), fun = distHaversine)
  if(distance < 200){
    del_lat <- c(del_lat, f_building_lat[j])
    del_lon <- c(del_lon, f_building_lon[j]) 
  }
}

f2_building_lat = f_building_lat [!f_building_lat %in% del_lat]
f2_building_lon = f_building_lon [!f_building_lon %in% del_lon]

### 해당건물좌표를 기반으로 거리행렬 계산
dist_mat2 = make_dist_mat(f2_building_lon, f2_building_lat)

#p-median 알고리즘 적용 
result2 = p_median(dist_mat2, 1, seed = 42)

second_lat = f2_building_lat[223]
second_lon = f2_building_lon[223]
cat(second_lat,',',second_lon)




