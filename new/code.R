library(shiny)
library(rms)
library(survival)
library(DT)
library("survival")
library("survminer")
library(rms)
library(readxl)
library(openxlsx)

stomach_model <- readRDS("D:/data/tian/数据分析网页/new/stomach_model.rds")



# 定义用户界面
ui <- fluidPage(
  titlePanel("Survival prediction tool for T3-4N+M0 gastric cancer"),
  sidebarLayout(
    sidebarPanel(
      h4("Clinical parameters"),
      selectInput("Age", "Age", 
                  choices = c("<50 years" = "<50",
                              "50-60 years" = "50-60",
                              "≥60 years" = ">=60")),
      selectInput("Grade", "Grade", 
                  choices = c("G1" = "G1", 
                              "G2" = "G2", 
                              "G3" = "G3",
                              "G4" = "G4")),
      selectInput("Nstage", "N", 
                  choices = c("N1" = "N1", "N2" = "N2", "N3" = "N3")),
      selectInput("Radiotherapy", "Radiotherapy", 
                  choices = c("Yes" = "Yes", "no" = "no")),
      selectInput("RNE", "Regional nodes excision", 
                  choices = c("<18个" = "<18",
                              "≥18个" = "≥18")),
      selectInput("time_point", "predicted time", 
                  choices = c("1 year" = 12, "3 year" = 36, "5 year" = 60)),
      actionButton("predict", "predict", class = "btn-primary"),
      br(), br(),
      helpText("Note: This tool is based on the Cox proportional hazards model for prediction. The results are for reference only.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predict result",
                 h3("Individualized survival prediction"),
                 verbatimTextOutput("prediction"),
                 plotOutput("nomogram_plot", height = "600px")
        ),
        tabPanel("Calibration curves",
                 h3("Calibration curves of model"),
                 selectInput("calibration_time", "Time point",
                             choices = c("1 year" = 12, "3 year" = 36, "5 year" = 60)),
                 plotOutput("calibration_plot", height = "500px")
        ),
        tabPanel("Introduction",
                 h3("Tool Usage Guide"),
                 p("1. Input patient clinical parameters on the left side."),
                 p("2. Select the desired prediction time point."),
                 p("3. Click the 'Calculate Survival Probability' button."),
                 p("4. View the prediction results and nomogram on the right side."),
                 br(),
                 h4("Parameter Description"),
                 tags$ul(
                   tags$li("Age: Patient's age group at diagnosis (<50 years, 50-60 years, ≥60 years)"),
                   tags$li("Tumor Grade: Assessed based on tumor cell differentiation (G1, G2, G3, G4)"),
                   tags$li("N Stage: Lymph node metastasis status (N1, N2, N3)"),
                   tags$li("Radiotherapy: Yes=Radiation after surgery or Radiation prior to surgery, No=No radiation)"),
                   tags$li("Regional Lymph Node Harvested Number: Number of regional lymph nodes harvested during surgery (<18, ≥18)")
                 )
        )
      )
    )
  )
)

# 服务器逻辑
server <- function(input, output, session) {
  
  # 加载模型
  model <- reactive({
    readRDS("stomach_model.rds")
  })
  
  # 获取模型因子水平（安全的方式）
  get_model_factors <- reactive({
    req(model())
    model_obj <- model()
    
    factors_list <- list()
    
    # 安全地获取每个变量的因子水平
    tryCatch({
      if (!is.null(model_obj$model$Age)) {
        factors_list$Age <- levels(model_obj$model$Age)
      } else {
        factors_list$Age <- c("<50", "50-60", ">=60")
      }
    }, error = function(e) {
      factors_list$Age <- c("<50", "50-60", ">=60")
    })
    
    tryCatch({
      if (!is.null(model_obj$model$Grade)) {
        factors_list$Grade <- levels(model_obj$model$Grade)
      } else {
        factors_list$Grade <- c("G1", "G2", "G3", "G4")
      }
    }, error = function(e) {
      factors_list$Grade <- c("G1", "G2", "G3", "G4")
    })
    
    tryCatch({
      if (!is.null(model_obj$model$Nstage)) {
        factors_list$Nstage <- levels(model_obj$model$Nstage)
      } else {
        factors_list$Nstage <- c("N1", "N2", "N3")
      }
    }, error = function(e) {
      factors_list$Nstage <- c("N1", "N2", "N3")
    })
    
    tryCatch({
      if (!is.null(model_obj$model$Radiotherapy)) {
        factors_list$Radiotherapy <- levels(model_obj$model$Radiotherapy)
      } else {
        factors_list$Radiotherapy <- c("no", "Yes")
      }
    }, error = function(e) {
      factors_list$Radiotherapy <- c("no", "Yes")
    })
    
    tryCatch({
      if (!is.null(model_obj$model$RNE)) {
        factors_list$RNE <- levels(model_obj$model$RNE)
      } else {
        factors_list$RNE <- c("<18", "≥18")
      }
    }, error = function(e) {
      factors_list$RNE <- c("<18", "≥18")
    })
    
    return(factors_list)
  })
  
  # 预测生存概率
  prediction_data <- eventReactive(input$predict, {
    req(input$Age, input$Grade, input$Nstage, input$Radiotherapy, input$RNE)
    
    # 获取模型因子水平
    model_factors <- get_model_factors()
    
    print("模型因子水平:")
    print(model_factors)
    
    # 创建新数据框 - 使用模型中的因子水平
    new_data <- data.frame(
      Age = factor(input$Age, levels = model_factors$Age),
      Grade = factor(input$Grade, levels = model_factors$Grade),
      Nstage = factor(input$Nstage, levels = model_factors$Nstage),
      Radiotherapy = factor(input$Radiotherapy, levels = model_factors$Radiotherapy),
      RNE = factor(input$RNE, levels = model_factors$RNE)
    )
    
    print("输入数据:")
    print(new_data)
    
    # 获取预测时间点
    time_point <- as.numeric(input$time_point)
    
    # 计算生存概率 - 添加详细的错误处理
    surv_result <- tryCatch({
      surv_prob <- survest(model(), newdata = new_data, times = time_point)
      
      print("预测结果:")
      print(surv_prob)
      
      # 检查结果是否为数值
      if (length(surv_prob$surv) == 0) {
        stop("生存概率结果为空")
      }
      
      if (!is.numeric(surv_prob$surv)) {
        stop("生存概率不是数值类型")
      }
      
      list(
        surv = surv_prob$surv,
        linear_predictors = surv_prob$linear.predictors,
        error = NULL
      )
    }, error = function(e) {
      showNotification(paste("预测错误:", e$message), type = "error")
      list(
        surv = NA_real_,
        linear_predictors = NA_real_,
        error = e$message
      )
    })
    
    # 安全地处理结果
    survival_prob_value <- NA_real_
    linear_predictor_value <- NA_real_
    
    if (!is.null(surv_result$surv) && is.numeric(surv_result$surv) && length(surv_result$surv) > 0) {
      survival_prob_value <- round(surv_result$surv * 100, 1)
    }
    
    if (!is.null(surv_result$linear_predictors) && is.numeric(surv_result$linear_predictors) && length(surv_result$linear_predictors) > 0) {
      linear_predictor_value <- round(surv_result$linear_predictors, 2)
    }
    
    # 返回结果
    list(
      data = new_data,
      time = time_point,
      survival_prob = survival_prob_value,
      linear_predictor = linear_predictor_value,
      error = surv_result$error
    )
  })
  
  # 显示预测结果
  output$prediction <- renderText({
    pred <- prediction_data()
    
    if (!is.null(pred$error) && !is.na(pred$error)) {
      return(paste("预测错误:", pred$error, "\n请检查输入参数是否与模型训练时一致"))
    }
    
    if (is.na(pred$survival_prob)) {
      return("无法计算生存概率，请检查输入参数")
    }
    
    paste0("在 ", pred$time, " 个月时（约", round(pred$time/12, 1), "年），\n",
           "预测生存概率为: ", pred$survival_prob, "%\n",
           "线性预测值: ", pred$linear_predictor)
  })
  
  # 显示列线图
  output$nomogram_plot <- renderPlot({
    # 定义生存函数
    survival <- Survival(model())
    survival1 <- function(x) survival(12, x)
    survival2 <- function(x) survival(36, x)
    survival3 <- function(x) survival(60, x)
    
    # 绘制列线图
    plot(nomogram(model(), 
                  fun = list(survival1, survival2, survival3), 
                  lp = FALSE,
                  fun.at = c(0.05, seq(0.1, 0.9, by = 0.1), 0.95), 
                  funlabel = c("1 year survival rate", "3 year survival rate", "5 year survival rate")),
         xfrac = 0.3)
  })
  
  # 显示校正曲线
  output$calibration_plot <- renderPlot({
    time_point <- as.numeric(input$calibration_time)
    
    # 创建校准曲线
    fstomach <- cph(Surv(Survival.months, status_code1111) ~ Age + Grade + Nstage + Radiotherapy + RNE,
                    data = stomach, y = TRUE, x = TRUE, surv = TRUE, 
                    na.action = na.delete, time.inc = time_point)
    
    cal <- calibrate(fstomach, cmethod = "KM", method = "boot", 
                     u = time_point, m = 50, B = 1000)
    
    # 绘制校准曲线
    par(mar = c(5, 5, 3, 2), cex = 1.0)
    plot(cal, lwd = 2, lty = 1,
         errbar.col = rgb(0, 118, 192, maxColorValue = 255),
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = paste0("列线图预测的", time_point/12, "年总生存概率"),
         ylab = paste0("实际的", time_point/12, "年总生存概率"),
         col = rgb(192, 98, 83, maxColorValue = 255))
    abline(0, 1, lty = 2, col = "gray")
  })
}

# 运行应用
shinyApp(ui = ui, server = server)
