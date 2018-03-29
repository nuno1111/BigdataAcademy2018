install.packages("keras")
library(keras)

# python keras 환경 설치 : 파이썬 설치 및 keras 관련 패키지 모두 설치
install_keras()

# --------------------------------------------------------------------------------------------------------------
# 데이터
# --------------------------------------------------------------------------------------------------------------
# 28 * 28 grayscale image 7만장, 0 ~ 9까지의 필기체 이미지
mnist <- dataset_mnist()

# 학습 데이터 설정(train : 6만장, test : 1만장)
# 이미지별 X 데이터(픽셀 값)
x_train <- mnist$train$x 
x_test  <- mnist$test$x

# reshape : 개별 이미지 28 * 28을 1차원으로 784개
dim(x_train) <- c(nrow(x_train), 784)
dim(x_test)  <- c(nrow(x_test), 784)

# rescale : 0 ~ 1 사이의 값을 갖도록, 픽셀 값의 범위 0 ~ 255
x_train <- x_train / 255
x_test  <- x_test / 255


# --------------------------------------------------------------------------------------------------------------
# 모델링
# --------------------------------------------------------------------------------------------------------------
# 0) 노드 개수
num_input    <- 784
num_hidden_1 <- 256
num_hidden_2 <- 128

# 1) input layer
input_img <- layer_input(shape = num_input)

# 2) encoder layer
encoded_layer1 <- layer_dense(input_img, num_hidden_1, activation = "sigmoid")
encoded_layer2 <- layer_dense(encoded_layer1, num_hidden_2, activation = "sigmoid")

# 3) decoder layer
decoded_layer1 <- layer_dense(encoded_layer2, num_hidden_1, activation = "sigmoid")
decoded_layer2 <- layer_dense(decoded_layer1, num_input, activation = "sigmoid")


# model 정의
autoencoder <- keras_model(input_img, decoded_layer2)

# loss function 및 최적화방법 설정
autoencoder %>% compile(loss = "mse",
                        optimizer = optimizer_rmsprop(lr = 0.01), 
                        metrics = c('accuracy'))

# model fitting
autoencoder %>% fit(x_train, x_train,
                    epochs = 50,
                    batch_size = 256,
                    shuffle = TRUE,
                    validation_split = 0.2)
                        
# --------------------------------------------------------------------------------------------------------------
# 결과 시각화
# --------------------------------------------------------------------------------------------------------------
# n * n 개의 random 이미지 시각화(무작위로 추출)
n   <- 4
idx <- sample(1:nrow(x_test), n^2, replace = FALSE)

# test 원본 이미지
win.graph()
par(mfcol = c(n, n)) # n * n
par(mar = c(0, 0, 0, 0), xaxs='i', yaxs='i') # 간격 0

for(i in idx)
{ 
  im <- mnist$test$x[i,,]
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col=gray((0:255)/255), xaxt='n')
}

# autoencoder 결과 이미지
result      <- autoencoder %>% predict(x_test[idx,])
dim(result) <- c(n^2, 28, 28)

win.graph()
par(mfcol=c(n, n))
par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')

for(i in 1:n^2)
{ 
  im <- result[i,,]
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col=gray((0:255)/255), xaxt='n')
}

