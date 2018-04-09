
# 기준년도,시간,사용컬럼 벡터정의
yyyymm_vec <- c("201312","201401","201402","201403","201404","201405","201406","201407","201408","201409","201410","201411","201412")
hour_vec <- c(
  "00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"
)
col_names <- c(
  "YYYYMM","ID_300","00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23"
)

# GIS 파일 읽어오기
library(rgdal)
lnd <- readOGR(dsn = "./파일럿프로젝트/GIS", layer = "PCELL_ID_300") # GIS SHP 파일 로딩

# 데이터 파일 읽어오기
df_time <- read.csv("./파일럿프로젝트/TIME/TIME_300.csv")
df_time <- df_time[,0:26] # 비율데이터 제거
colnames(df_time) <- col_names # 변수명 변경

# 데이터 파일 Pivot
library(reshape)
df_time_pivot <- melt(df_time, id.vars = c("YYYYMM","ID_300")) # 데이터 PIVOT
df_time_pivot <- df_time_pivot[df_time_pivot$ID_300 > 1000020000,] # 추자도 제거하고 제주도만
colnames(df_time_pivot) <- c("YYYYMM","ID_300","Hour","Value") # 변수명 변경

# 데이터 로그취하기
df_time_pivot$Value <- log(df_time_pivot$Value+1)

# 데이터 있는 격자만 남기기
lnd_target <- lnd[lnd$ID_300 %in% df_time_pivot$ID_300,]
# plot(lnd_target)

library(sqldf)
library(shiny)
library(shinythemes)

# 서버프로그램
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    yyyymm = input$yyyymm
    hour = input$hour
    
    lnd_tmp <- lnd_target
    
    df_time_tmp <- df_time_pivot[df_time_pivot$YYYYMM == strtoi(yyyymm), ]
    df_time_tmp <- df_time_tmp[df_time_tmp$Hour == hour, ]
    
    lnd_tmp_data <- lnd_tmp@data
    lnd_tmp@data <- sqldf("SELECT B.* FROM lnd_tmp_data A LEFT OUTER JOIN df_time_tmp B ON A.ID_300 = B.ID_300  ")
    
    lnd_tmp <- lnd_tmp[!is.na(lnd_tmp$ID_300) ,]
    lnd_tmp_2 <- lnd_tmp[lnd_tmp$Value > 2 & lnd_tmp$Value <= 3 ,]
    lnd_tmp_3 <- lnd_tmp[lnd_tmp$Value > 3 & lnd_tmp$Value <= 4 ,]
    lnd_tmp_4 <- lnd_tmp[lnd_tmp$Value > 4 & lnd_tmp$Value <= 5 ,]
    lnd_tmp_5 <- lnd_tmp[lnd_tmp$Value > 5 & lnd_tmp$Value <= 6 ,]
    lnd_tmp_6 <- lnd_tmp[lnd_tmp$Value > 6 ,]
    
    # qtm(lnd_tmp, "Value")
    title <- paste(substr(yyyymm,1,4),"년",substr(yyyymm,5,6),"월",hour,"시")
    
    p0 <- plot(lnd_tmp_2 , col = rgb(255,216,216,maxColorValue = 255), main=title, xlim=c(874217.8, 951017.8), ylim=c(1464669.9, 1508469.9))
    plot(lnd_tmp_3, add = T, col = rgb(255,167,167,maxColorValue = 255))
    plot(lnd_tmp_4, add = T, col = rgb(241,95,95,maxColorValue = 255))
    plot(lnd_tmp_5, add = T, col = rgb(204,51,51,maxColorValue = 255))
    plot(lnd_tmp_6, add = T, col = rgb(103,0,0,maxColorValue = 255))
    p0
  })
}


# Define UI for application that draws a map
ui <- fluidPage(
  
  # Application title
  titlePanel("유동인구지도"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("yyyymm", "년/월",
                  c("2013년 12월" = "201312",
                    "2014년 1월" = "201401",
                    "2014년 2월" = "201402",
                    "2014년 3월" = "201403",
                    "2014년 4월" = "201404",
                    "2014년 5월" = "201405",
                    "2014년 6월" = "201406",
                    "2014년 7월" = "201407",
                    "2014년 8월" = "201408",
                    "2014년 9월" = "201409",
                    "2014년 10월" = "201410",
                    "2014년 11월" = "201411",
                    "2014년 12월" = "201412"
                  )),
      selectInput("hour", "시간",
                  c("00시 ~ 01시" = "00",
                    "01시 ~ 02시" = "01",
                    "02시 ~ 03시" = "02",
                    "03시 ~ 04시" = "03",
                    "04시 ~ 05시" = "04",
                    "05시 ~ 06시" = "05",
                    "06시 ~ 07시" = "06",
                    "07시 ~ 08시" = "07",
                    "08시 ~ 09시" = "08",
                    "09시 ~ 10시" = "09",
                    "10시 ~ 11시" = "10",
                    "11시 ~ 12시" = "11",
                    "12시 ~ 13시" = "12",
                    "13시 ~ 14시" = "13",
                    "14시 ~ 15시" = "14",
                    "15시 ~ 16시" = "15",
                    "16시 ~ 17시" = "16",
                    "17시 ~ 18시" = "17",
                    "18시 ~ 19시" = "18",
                    "19시 ~ 20시" = "19",
                    "20시 ~ 21시" = "20",
                    "21시 ~ 22시" = "21",
                    "22시 ~ 23시" = "22",
                    "23시 ~ 00시" = "23"
                  ))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = "700px", width = "1000px")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)


