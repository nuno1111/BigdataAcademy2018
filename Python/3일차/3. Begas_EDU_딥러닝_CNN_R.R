library(keras)

# library(reticulate)
# use_python("C:/Users/jhpark/AppData/Local/Programs/Python/Python36")
# 
# 
# library(reticulate)
# conda_remove("r-tensorflow")
# 
# install_keras(tensorflow = "gpu")
# 
# library(tensorflow)

# --------------------------------------------------------------------------------------------------------------
# 데이터
# --------------------------------------------------------------------------------------------------------------
# 28 * 28 grayscale image 7만장, 0 ~ 9까지의 필기체 이미지
mnist <- dataset_mnist()

# 학습 데이터 설정(train : 6만장, test : 1만장)
# 이미지별 X 데이터(픽셀 값)
x_train <- mnist$train$x
y_train <- mnist$train$y 
x_test  <- mnist$test$x
y_test  <- mnist$test$y

# reshape : 개별 이미지 28 * 28을 1차원으로 784개
img_rows <- 28
img_cols <- 28
x_train     <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test      <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# rescale : 0 ~ 1 사이의 값을 갖도록, 픽셀 값의 범위 0 ~ 255
x_train <- x_train / 255
x_test  <- x_test / 255

# Y를 binary class로 변환
# Y의 종류 개수는 0부터 9까지 10개
# 예) Y가 2라면, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 으로 변환
num_classes <- 10
y_train <- to_categorical(y_train, num_classes)
y_test  <- to_categorical(y_test, num_classes)


cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')


# --------------------------------------------------------------------------------------------------------------
# 모델링
# --------------------------------------------------------------------------------------------------------------
CNN_model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(5, 5), activation = 'relu', padding = 'SAME', input_shape = input_shape) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(5, 5), padding = 'SAME', activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_flatten() %>% 
  layer_dense(units = 1024, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = num_classes, activation = 'softmax')

# Compile model
CNN_model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adam(lr = 0.0001),
  metrics = c('accuracy')
)

# Train model
CNN_model %>% fit(
  x_train, y_train,
  batch_size = 100,
  epochs = 5,
  validation_split = 0.2
)


# --------------------------------------------------------------------------------------------------------------
# 결과 시각화
# --------------------------------------------------------------------------------------------------------------
# 1) 분류결과표
pred_y <- as.numeric(CNN_model %>% predict_classes(x_test))
real_y <- as.numeric(apply(y_test, 1, function(x) which(x == 1))) - 1

table(real_y, pred_y)

# 2) 그림으로 표현
# n 개의 random 이미지 시각화(무작위로 추출)
n   <- 10
idx <- sample(1:nrow(x_test), n)

# test 원본 이미지
win.graph()
par(mfrow = c(2, n/2))
par(mar = c(0, 1, 4, 0), xaxs='i', yaxs='i')

for(i in 1:n)
{
  im <- mnist$test$x[idx[i],,]
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col=gray((0:255)/255), xaxt='n', yaxt='n', xlab = "", ylab = "",
        main = paste("실제 값 : ", real_y[idx[i]], "\n 분류 결과 : ", pred_y[idx[i]]))
}