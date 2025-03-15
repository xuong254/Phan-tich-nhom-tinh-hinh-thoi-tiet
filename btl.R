# Cài đặt các thư viện cần thiết 
install.packages("tidyverse")    # Thay thế pandas + numpy
install.packages("factoextra")   # Hỗ trợ phân cụm K-Means
install.packages("cluster")      # Hỗ trợ thuật toán K-Means
install.packages("GGally")       # Thay thế parallel_coordinates từ pandas



# Load thư viện
library(tidyverse)   # Thay thế pandas + numpy
library(factoextra)  # Hỗ trợ K-Means clustering
library(cluster)     # Hỗ trợ thuật toán K-Means
library(GGally)      # Thay thế parallel_coordinates


--------------
 # Đọc dữ liệu từ tệp CSV
data <- read.csv("minute_weather.csv", header = TRUE, stringsAsFactors = FALSE)

# Kiểm tra dữ liệu
head(data)  # Hiển thị 6 dòng đầu tiên

str(data)   # Xem cấu trúc dữ liệu

--------------
  
# Kiểm tra tổng số giá trị NA trong từng cột
  colSums(is.na(data))

--------------
  
 # Kiểm tra kiểu dữ liệu của từng cột
  str(data)

# Kiểm tra kích thước của dữ liệu (số dòng và số cột)
dim(data)

# Hiển thị thống kê tổng quan về dữ liệu
summary(data)

---------------------
  # Lấy số lượng hàng và cột
  dim(data)

-----------------------
  # Xem 6 dòng đầu tiên của dataframe (mặc định)
  head(data)
----------------------


# Lọc các dòng có rowID là bội số của 10
sampled_df <- data %>% filter(rowID %% 10 == 0)

# Kiểm tra kích thước dataframe lọc được
dim(sampled_df)

--------------------------
  str(sampled_df)  # Xem kiểu dữ liệu của từng cột

# Giữ lại chỉ các cột số
numeric_cols <- sampled_df[sapply(sampled_df, is.numeric)]

# Tạo dataframe thống kê
stats_df <- data.frame(
  Mean = apply(numeric_cols, 2, mean, na.rm = TRUE),
  Std_Dev = apply(numeric_cols, 2, sd, na.rm = TRUE),
  Min = apply(numeric_cols, 2, min, na.rm = TRUE),
  Q1 = apply(numeric_cols, 2, quantile, probs = 0.25, na.rm = TRUE),
  Median = apply(numeric_cols, 2, median, na.rm = TRUE),
  Q3 = apply(numeric_cols, 2, quantile, probs = 0.75, na.rm = TRUE),
  Max = apply(numeric_cols, 2, max, na.rm = TRUE)
)
-----------------
# Chuyển vị giống với .transpose() trong pandas
t(stats_df)
#LƯU ÝÝ
-------------------------

# Lọc dữ liệu và lấy số dòng và số cột
filtered_df <- sampled_df %>% filter(rain_accumulation == 0)

dim(filtered_df)  # Trả về (số dòng, số cột)

----------------------

  
  
# Xóa cột 'rain_accumulation' và 'rain_duration'
  sampled_df <- sampled_df %>% select(-rain_accumulation, -rain_duration)

# Lấy số lượng hàng trước khi xóa NA
rows_before <- nrow(sampled_df)

# Loại bỏ các dòng chứa giá trị NA
sampled_df <- na.omit(sampled_df)

# Lấy số lượng hàng sau khi xóa NA
rows_after <- nrow(sampled_df)


--------------------
#Xóa bỏ 2 cột (rain_accumulation) và (rain_duration) do dữ liệu ở 2 cột này không có giá trị 
  rows_before - rows_after


------------------------
  library(dplyr)
sampled_df %>% colnames()

-------------------------
  # Lưu danh sách tên cột dưới dạng vector ký tự
  features <- c("air_pressure", "air_temp", "avg_wind_direction", "avg_wind_speed", 
                "max_wind_direction", "max_wind_speed", "relative_humidity")

-------------------
  # Chọn các cột có trong features từ sampled_df
  select_df <- sampled_df[, features]

# Hiển thị danh sách các cột
colnames(select_df)

---------------------
  library(dplyr)
glimpse(select_df)  # Hiển thị thông tin tóm tắt

------------------
  

X_df <- select_df %>%
  mutate(across(everything(), ~ scale(.)[,1]))

# Hiển thị kết quả
X_df



-------------------------------------------------
  suppressWarnings({
    wcss <- sapply(1:11, function(k)
    kmeans(X_df, centers = k, nstart = 25)$tot.withinss)
  })
# Hiển thị WCSS
wcss


---------------------------------------------------
  

# Tạo dataframe chứa k và WCSS
elbow_df <- data.frame(k = 1:11, wcss = wcss)

# Vẽ biểu đồ Elbow
ggplot(elbow_df, aes(x = k, y = wcss)) +
  geom_point(color = "red", size = 3) +  # Vẽ điểm tròn (bo-)
  geom_line(color = "blue", linetype = "solid") +  # Nối đường
  labs(title = "Biểu đồ Elbow để tìm k tối ưu",
       x = "Số lượng cụm (k)", 
       y = "WCSS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Căn giữa tiêu đề


---------------------------------------
  # Chạy thuật toán K-Means với 4 cụm
  set.seed(42)  # Đặt seed để kết quả có thể tái lặp
model <- kmeans(X_df, centers = 4, nstart = 25)  

# In thông tin mô hình
print(model)

-----------------------------------------
  # Lấy tọa độ trung tâm các cụm
  centers <- model$centers

# Chuyển đổi thành data frame
centers_df <- as.data.frame(centers)

# Hiển thị kết quả
print(centers_df)

---------------------------------------

  # Định nghĩa hàm tạo DataFrame chứa tâm cụm
  pd_centers <- function(featuresUsed, centers) {
    colNames <- c(featuresUsed, "prediction")  # Thêm cột 'prediction'
    
    # Gán chỉ số cụm vào tâm cụm
    Z <- as.data.frame(cbind(centers, prediction = 0:(nrow(centers) - 1)))
    
    # Đặt tên cột
    colnames(Z) <- colNames
    
    return(Z)
  }

-----------------------
# Hàm vẽ đồ thị song song
parallel_plot <- function(data) {
  ggparcoord(data,
             columns = 1:(ncol(data)-1),  # Chọn tất cả các cột trừ 'prediction'
             groupColumn = "prediction",  # Cột để phân nhóm màu
             scale = "uniminmax",  # Chuẩn hóa giá trị về khoảng [0,1]
             alphaLines = 0.6) +   # Độ trong suốt
    theme_minimal() + 
    labs(title = "Biểu đồ song song của các cụm") +
    theme(plot.title = element_text(hjust = 0.5))
}

---------------------------------------------
  P <- pd_centers(features, centers)
print(P)


--------------------------------------------------
  
colnames(data)  # Hiển thị danh sách các cột trong dataframe

  


  -----------------------------------------------------
  

# Vẽ biểu đồ phân tán Áp suất không khí tại thời điểm đo
ggplot(data, aes(x = rowID, y = air_pressure)) +
  geom_point(color = "red", alpha = 0.5, size = 1) +
  labs(title = "Biểu đồ phân tán Áp suất không khí",
       x = "Thời điểm đo (rowID)",
       y = "Áp suất không khí (hPa)") +
  theme_minimal()


-----------------------------------------------------
  
# Vẽ biểu đồ phân tán cho nhiệt độ không khí (Đơn vị: độ F)
  ggplot(data, aes(x = rowID, y = air_temp)) +
  geom_point(color = "blue", alpha = 0.5, size = 1) +
  labs(title = "Biểu đồ phân tán Nhiệt độ không khí",
       x = "Thời điểm đo (rowID)",
       y = "Nhiệt độ không khí (°F)") +
  theme_minimal()

------------------------------
  #Lượng mưa tích lũy theo thời gian 
  ggplot(data, aes(x = as.POSIXct(hpwren_timestamp), y = rain_accumulation)) +
  geom_line(color = "darkblue", na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 50)) +  # Điều chỉnh phạm vi trục Y
  labs(title = "Lượng mưa tích lũy theo thời gian",
       x = "Thời gian",
       y = "Lượng mưa (mm)") +
  theme_minimal()


  --------------------------------------------------
  
  #Biểu đồ hướng gió trung bình và tốc độ gió
ggplot(data, aes(x = avg_wind_direction, y = avg_wind_speed)) +
  geom_point(color = "red", alpha = 0.7, na.rm = TRUE) +
  labs(title = "Hướng gió trung bình và tốc độ gió",
       x = "Hướng gió (°)",
       y = "Tốc độ gió (m/s)") +
  theme_minimal()





  
  -------------------------------------
  # Thêm cột index để làm trục x
  data$index <- seq_len(nrow(data))

  -------------------------------------
  #Nhiệt độ và độ ẩm
  ggplot(data, aes(x = air_temp, y = relative_humidity)) +
  geom_point(color = "blue", alpha = 0.6) +
  labs(title = "Mối quan hệ giữa Nhiệt độ và Độ ẩm",
       x = "Nhiệt độ (°F)",
       y = "Độ ẩm (%)") +
  theme_minimal()



-----------------------------------
  
  
#Biểu đồ đường của dữ liệu khí tượng
 # Cài đặt thư viện 
install.packages("tidyverse")
# Load thư viện)
library(tidyverse)

# Chỉ chọn các cột khí tượng cần thiết
features <- c("air_pressure", "air_temp", "avg_wind_direction", "avg_wind_speed",
              "max_wind_direction", "max_wind_speed", "relative_humidity")

# Kiểm tra nếu thiếu cột thì loại bỏ
features <- features[features %in% colnames(data)]
if (length(features) == 0) {
  stop("Lỗi: Không có dữ liệu khí tượng phù hợp!")
}

# Tính trung bình cho từng yếu tố
mean_values <- data %>%
  select(all_of(features)) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Yeu_to", values_to = "Gia_tri")

# Vẽ biểu đồ đường
ggplot(mean_values, aes(x = Yeu_to, y = Gia_tri, group = 1)) +
  geom_point(color = "blue", size = 3) +  # Vẽ điểm tròn
  geom_line(color = "blue", linetype = "solid") +  # Vẽ đường nối
  labs(title = "Biểu đồ đường của dữ liệu khí tượng",
       x = "", 
       y = "Giá trị trung bình") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Xoay nhãn trục X
        plot.title = element_text(hjust = 0.5))  # Căn giữa tiêu đề
  
  
-----------------------------------------
  # Cài đặt và tải các thư viện cần thiết
  install.packages("ggplot2")  # Nếu chưa cài đặt
install.packages("dplyr")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(ggpubr)

# Tạo dữ liệu mẫu tương tự
set.seed(42)
num_samples <- 2000

data <- data.frame(
  avg_wind_direction = runif(num_samples, 0, 360),  # Hướng gió trung bình
  relative_humidity = runif(num_samples, 10, 90),  # Độ ẩm
  prediction = factor(sample(0:4, num_samples, replace = TRUE))  # Nhãn nhóm 0-4
)

# Vẽ biểu đồ scatter plot với ggplot2
ggplot(data, aes(x = avg_wind_direction, y = relative_humidity, color = prediction)) +
  geom_point(alpha = 0.6, size = 1) +  # Độ trong suốt và kích thước điểm
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +  # Màu cụm
  theme_minimal() +
  labs(title = "Biểu đồ phân tán cụm K-Means ",
       x = "hướng gió trung bình",
       y = "độ ẩm tương đối",
       color = "dự đoán")

#DDONE
 ----------------------------------
  
  # Cài đặt và tải các thư viện cần thiết
  install.packages("ggplot2")  # Nếu chưa cài đặt
install.packages("dplyr")

library(ggplot2)
library(dplyr)

# Tạo dữ liệu giả lập: Độ ẩm trung bình mỗi tháng
set.seed(42)
humidity_data <- data.frame(
  month = factor(month.name, levels = month.name),  # Tên tháng (January, February, ...)
  relative_humidity = round(runif(12, 50, 90), 1)  # Độ ẩm ngẫu nhiên từ 50% đến 90%
)

# Vẽ biểu đồ cột
ggplot(humidity_data, aes(x = month, y = relative_humidity, fill = month)) +
  geom_bar(stat = "identity") +  # Biểu đồ cột với giá trị thực
  scale_fill_manual(values = rainbow(12)) +  # Tạo màu sắc khác nhau cho từng tháng
  theme_minimal() +
  labs(title = "Biểu đồ độ ẩm trung bình trong 1 năm",
       x = "Tháng",
       y = "Độ ẩm tương đối (%)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Xoay nhãn trục x cho dễ đọc

  

#DDONE

  
 ---------------------------------
  # Cài đặt thư viện nếu chưa có
  install.packages("ggplot2")
install.packages("dplyr")

# Load thư viện
library(ggplot2)
library(dplyr)

# Tạo dữ liệu giả lập với tốc độ gió trung bình theo tháng
set.seed(123)
df_avg_wind <- data.frame(
  month = month.name,  # 12 tháng
  avg_wind_speed = c(4, 10, 6, 12, 7, 14, 8, 11, 5, 13, 6, 9)  # Dữ liệu giả lập
)

# Sắp xếp thứ tự tháng để hiển thị đúng trình tự
df_avg_wind$month <- factor(df_avg_wind$month, 
                            levels = rev(c("January", "February", "March", "April", "May", "June", 
                                           "July", "August", "September", "October", "November", "December")))

# Vẽ biểu đồ cột ngang tốc độ gió trung bình theo tháng
ggplot(df_avg_wind, aes(x = avg_wind_speed, y = month, fill = avg_wind_speed)) +
  geom_col(width = 0.6) +  # Biểu đồ cột ngang
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Màu từ xanh nhạt đến xanh đậm
  theme_minimal() +
  labs(title = "Tốc Độ Gió Trung Bình Theo Tháng",
       x = "Tốc độ gió trung bình (m/s)",
       y = "Tháng") +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(face = "bold"))  # Nhấn mạnh trục tháng
#DDONE

--------------------------
  
  # Cài đặt thư viện nếu chưa có
  install.packages("ggplot2")
install.packages("dplyr")

# Load thư viện
library(ggplot2)
library(dplyr)

# Tạo dữ liệu giả lập với nhiệt độ trung bình theo tháng (15°C - 39°C)
set.seed(123)
df_temp <- data.frame(
  month = month.name,  # 12 tháng
  avg_temperature = c(15, 16, 18, 20, 23, 26, 29, 28, 26, 22, 18, 15)  # Nhiệt độ theo tháng
)

# Sắp xếp thứ tự tháng để hiển thị đúng trình tự
df_temp$month <- factor(df_temp$month, 
                        levels = rev(c("January", "February", "March", "April", "May", "June", 
                                       "July", "August", "September", "October", "November", "December")))

# Vẽ biểu đồ cột ngang hiển thị nhiệt độ trung bình theo tháng
ggplot(df_temp, aes(x = avg_temperature, y = month, fill = avg_temperature)) +
  geom_col() +  # Biểu đồ cột ngang
  scale_fill_gradient(low = "blue", high = "red") +  # Màu xanh lạnh -> đỏ nóng
  theme_minimal() +
  labs(title = "Nhiệt Độ Trung Bình Theo Tháng",
       x = "Nhiệt độ trung bình (°C)",
       y = "Tháng") +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold"))

  
  #DONDONE
----------------------


# Tạo dữ liệu giả lập với lượng mưa thay đổi theo tháng (mm)
set.seed(123)
df_rain <- data.frame(
  month = month.name,  # 12 tháng
  precipitation = c(50, 40, 60, 70, 90, 120, 150, 140, 110, 80, 60, 55)  # Lượng mưa ngẫu nhiên
)

# Sắp xếp thứ tự tháng để hiển thị đúng trình tự
df_rain$month <- factor(df_rain$month, 
                        levels = rev(c("January", "February", "March", "April", "May", "June", 
                                       "July", "August", "September", "October", "November", "December")))

# Vẽ biểu đồ cột ngang hiển thị lượng mưa trong năm
ggplot(df_rain, aes(x = precipitation, y = month, fill = precipitation)) +
  geom_col() +  # Biểu đồ cột ngang
  scale_fill_gradient(low = "lightblue", high = "blue") +  # Màu sắc thể hiện mức độ mưa
  theme_minimal() +
  labs(title = "Lượng Mưa Trung Bình Theo Tháng",
       x = "Lượng mưa (mm)",
       y = "Tháng") +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold"))

#DDINE
----------------------------


  # Cài đặt thư viện nếu chưa có
  install.packages("ggplot2")
install.packages("dplyr")

# Load thư viện
library(ggplot2)
library(dplyr)

# Tạo dữ liệu giả lập với độ ẩm tương đối theo tháng (%)
set.seed(123)
df_humidity <- data.frame(
  month = month.name,  # 12 tháng
  relative_humidity = c(85, 80, 75, 70, 65, 60, 55, 58, 62, 68, 75, 82)  # Độ ẩm giả lập (%)
)

# Sắp xếp thứ tự tháng theo đúng trình tự từ tháng 1 đến tháng 12
df_humidity$month <- factor(df_humidity$month, 
                            levels = c("January", "February", "March", "April", "May", "June", 
                                       "July", "August", "September", "October", "November", "December"))

# Vẽ biểu đồ cột thẳng với màu xanh nước biển
ggplot(df_humidity, aes(x = month, y = relative_humidity, fill = relative_humidity)) +
  geom_col(width = 0.6) +  # Biểu đồ cột thẳng
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Màu từ xanh nhạt đến xanh đậm
  theme_minimal() +
  labs(title = "Độ Ẩm Tương Đối Trung Bình Theo Tháng",
       x = "Tháng",
       y = "Độ ẩm trung bình (%)") +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))  # Xoay nhãn tháng

#DONE 
-------------------
 
  





  





