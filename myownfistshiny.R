library(shiny)
library(data.table)
library(magrittr)

ui <- navbarPage(title="Two group mean and proportion comparison",
                 tabPanel("Mean comparison",
                          sidebarLayout(
                            sidebarPanel(
                              fluidRow(
                                column(4,numericInput("mean1","Group 1 mean",value=100)),
                                column(4,numericInput("sd1","Group 1 s.d.",value=1)),
                                column(4,numericInput("n1","Group 1 n",value=10))
                              ),
                              fluidRow(
                                column(4,numericInput("mean2","Group 2 mean",value=200)),
                                column(4,numericInput("sd2","Group 2 s.d.",value=2)),
                                column(4,numericInput("n2","Group 2 n",value=20))
                              )
                            ),
                            mainPanel(
                              dataTableOutput("result_1")
                            )
                          )
                 ),
                 tabPanel("Proportion comparison",
                          sidebarLayout(
                            sidebarPanel(
                              fluidRow(
                                column(6,numericInput("case1","Group 1 case",value=10)),
                                column(6,numericInput("N1","Group 1 N",value=30))
                              ),
                              fluidRow(
                                column(6,numericInput("case2","Group 2 case",value=20)),
                                column(6,numericInput("N2","Group 2 N",value=40))
                              )
                            ),
                            mainPanel(
                              dataTableOutput("result_2")
                            )
                          )
                 )
                 )

server <- function(input, output) {
  output$result_1 <- renderDataTable({
    F_cal<-input$sd1^2/input$sd2^2
    F_criteria <- qf(0.975,df1=input$n1-1,df2=input$n2-1)
    if (F_cal>F_criteria) { # 등분산 기각, modified version of two sample t test
      t <- (input$mean1-input$mean2)/sqrt(input$sd1^2/input$n1+input$sd2^2/input$n2)
      df <- floor((input$sd1^2/input$n1+input$sd2^2/input$n2)^2
                  /(input$sd1^4/(input$n1^2*(input$n1-1))+input$sd2^4/(input$n2^2*(input$n2-1))))
      CI <- paste(round((input$mean1-input$mean2)-qt(0.975,df)*sqrt(input$sd1^2/input$n1+input$sd2^2/input$n2),5),
                  "~",round((input$mean1-input$mean2)+qt(0.975,df)*sqrt(input$sd1^2/input$n1+input$sd2^2/input$n2),5))
    } else { # 등분산, two sample t test
      sd_p <- ((input$n1-1)*input$sd1^2+(input$n2-1)*input$sd2^2)/(input$n1+input$n2-2)
      t <- (input$mean1-input$mean2)/sqrt(sd_p^2*(1/input$n1+1/input$n2))
      df <- input$n1+input$n2-2
      CI <- paste(round((input$mean1-input$mean2)-qt(0.975,df)*sqrt(sd_p^2*(1/input$n1+1/input$n2)),5),
                   "~",round((input$mean1-input$mean2)+qt(0.975,df)*sqrt(sd_p^2*(1/input$n1+1/input$n2)),5))
    }
    p_value <- 2*pt(-abs(t),df)
    data.table(t=t,df=df,p_value=p_value,CI=CI) %>% setnames(c("p_value","CI"),c("p-value","two-sided 95% CI"))
  })
  output$result_2 <- renderDataTable({
    q <- (input$case1+input$case2)/(input$N1+input$N2)
    fisher_criteria <- ifelse(input$N1>input$N2,ifelse(q>0.5,input$N2*(1-q),input$N2*q),ifelse(q>0.5,input$N1*(1-q),input$N1*q))
    if (fisher_criteria>=5) { # two sample z test for binomial proportion
      res <- prop.test(c(input$case1,input$case2),c(input$N1,input$N2))
      X_squared <- as.numeric(res$statistic)
      p_value <- res$p.value
      CI <- paste(round(res$conf.int[1],5),"~",round(res$conf.int[2],5))
      data.table(X_squared=X_squared,p_value=p_value,CI=CI) %>% setnames(c("X-squared","p-value","two-sided 95% CI"))
    } else { # fisher's exact test
      res <- fisher.test(rbind(c(input$case1,input$case2),c(input$N1-input$case1,input$N2-input$case2)))
      p_value <- res$p.value
      CI <- paste(round(res$conf.int[1],5),"~",round(res$conf.int[2],5))
      data.table(p_value=p_value,CI=CI) %>% setnames(c("p-value","two-sided 95% CI"))
    }
  })
} 

shinyApp(ui = ui, server = server)