install.packages("shiny")
install.packages("shinyWidgets")
install.packages("plotly")
install.packages("DT")
install.packages("shinythemes")


library(shiny)
library(readxl)
library(plotly)
library(DT)
library(data.table)
library(shinyWidgets)
library(shinythemes)
library(caret)
library(dplyr)




dat63.2<-read.csv("dat63-2.csv")
dat63.2




model5 <- readRDS("modelreg.rds")






#user interface part

ui<-fluidPage(  imageOutput("chula",width = "970px",height = "250px"),
              navbarPage(title = "page",id="inTabset",theme=shinythemes::shinytheme("slate"),
                         
                     tabPanel("HOME",
                fluidRow(column(12,align="center",
                        br(),
                        h1("Statistic and Data Science",align = "center",style="color:#FF6699"),
                        br(),
                        h5("ยินดีต้อนรับนิสิตทุกท่านเข้าสู่แอพพลิเคชั่นสำหรับตรวจสอบคะแนน นิสิตสามารถตรวจสอบคะแนนกลางภาค ได้จากการกรอกรหัสนิสิตของตนเองด้านล่างนี้ได้เลยครับ"),
                        br(),
                        hr(),
                        textInput("id", "กรุณากรอกรหัสนิสิต"),
                        tags$style(type="text/css", "#id {text-align:center;color:#FF6699;}"),
                        
                        actionButton('toNext', 'ถัดไป'),
                        h5(em(textOutput("noid")),style="color:red"),
                        
                        br(), 
                        br(), 
                        br(), 
                        br(), 
                        
                        ))
                         ),
              
    
                    tabPanel("คะแนนกลางภาค",
                        sidebarLayout(sidebarPanel(strong("ข้อมูลนักเรียน ",style="color:#FF6699"),
                                                     textOutput("studentName"),
                                                     textOutput("idstudent"),
                                                     
                                                     h3(textOutput("point"),"\n"),
                                                     tags$style(type="text/css", "#point {text-align:center;color:#FF6699;}"),
                                                     hr(),
                                                     p("สถิติภาพรวมภายในห้อง")  ,
                                                     verbatimTextOutput("summary"),
                                                     hr(),
                                                   
                  
                                                     br(),
                                                     selectInput(inputId = "charts",
                                                                 choices = c("Boxplot","Histogram","Dot plot"),
                                                                 label = c("ประเภทของกราฟ")),
                                                   actionButton('top', 'แสดงตำแหน่งของคุณบนกราฟ'),
                                                   hr(),  
                                                   actionButton('to2', 'ทำนายผลการเรียน')
                                                     ),
                                    mainPanel(h1("กราฟแสดงผลการเรียน",align = "center",style="color:#FF6699"),
                                             plotOutput("plot", width = "600px",height = "400px"),
                                             br(), 
                                             br(), 
                                             br(), 
                                             br(),
                                            ),
                                     
                            
                                  
                                    ), value = "panel2"),
             
           
                  tabPanel("ทำนายผลการเรียน",
                          pageWithSidebar( h3('โปรแกรมทำนายผลการเรียน' ,style="color:#FF6699"),
                                           
                                    sidebarPanel(tags$label(h4('ให้นักเรียนปรับค่าสำหรับการทำนาย')),
                                                 br(),
                                                 strong("ข้อมูลนักเรียน ",style="color:#FF6699"),
                                                 textOutput("Namest"),
                                                 textOutput("idst"),

                                    sliderInput("att", label = "คะแนนการเข้าเรียน", value = 3,
                                           min = min(0),
                                           max = max(5)),
                                    sliderInput("work", label = "คะแนนการส่งงาน", value = 3,
                                           min = min(0),
                                           max = max(5)),
                          
                                    actionButton("submitbutton", "เรียบร้อย", 
                                          class = "btn btn-primary")
                                                
                                                  ), 
                          mainPanel(headerPanel(h2("วิธีการและผลลัพธ์การทำนาย",align = "center",style="color:#FF6699")),
                            
                            
                            tabsetPanel(tabPanel("วิธีการให้คะแนน",
                                                 br(),
                            tags$label(h3("วิธีการให้คะแนน")),
                                    p(strong("ให้นักเรียนคาดการณ์และเลือกระดับการประเมินการเข้าเรียนดังนี้",align = "center",style="color:#FF6699")),
                                    p("     5 คะแนน หมายถึง มีการขาดเรียนไม่เกิน 3 ครั้ง "),
                                    p("     4 คะแนน หมายถึง มีการขาดเรียน 4 - 6 ครั้ง"),
                                    p("     3 คะแนน หมายถึง มีการขาดเรียน 7 – 9  ครั้ง"),
                                    p("     2 คะแนน หมายถึง มีการขาดเรียน 10 – 12 ครั้ง"),
                                    p("     1 คะแนน หมายถึง มีการขาดเรียนมากกว่า 13 - 15 ครั้ง"),
                                    p("     0 คะแนน หมายถึง มีการขาดเรียนมากกว่า 15 ครั้ง"),
                                    hr(),
                            p(strong("ให้นักเรียนคาดการณ์เลือกระดับการประเมินการส่งงานดังนี้",align = "center",style="color:#FF6699")),
                            p("     5 คะแนน หมายถึง ส่งงานครบถ้วน  สมบูรณ์ทุกครั้ง"),
                            p("     4 คะแนน หมายถึง ส่งงานขาด 1 ชิ้น"),
                            p("     3 คะแนน หมายถึง ส่งงานขาด 2 ชิ้น"),
                            p("     2 คะแนน หมายถึง ส่งงานขาด 3 ชิ้น"),
                            p("     1 คะแนน หมายถึง ส่งงานขาดมากกว่า 3 ชิ้น"),
                            p("     0 คะแนน หมายถึง ไม่ส่งงานเลย"),                                    
                            hr(), 
                            br(), 
                            br(), 
                            br(),                             
                                    
                            
                                              ),
                                                tabPanel("ผลการทำนาย",
                              
                                  tags$label(h3('Status/Output')), # Status/Output Text Box
                                  verbatimTextOutput('contents'),
                          
                                  tableOutput('tabledata'), # Prediction results table
                                  
                                  verbatimTextOutput('pre1'),
                                  tags$style(type="text/css", "#pre1 {text-align:center;color:#FF6699;font-size: 60px;}"),
                                  hr(),
                                  textOutput("vpre"),
                                  tags$style(type="text/css", "#vpre {text-align:center;color:#FF6699;font-size: 15px;}"),
                                  hr(),
                                  br(),
                                  br(), 
                                  br(), 
                                  br(), 
                                  
                                
                                                )                          
                                    )
                            )),value = "panel3"
             
                       
                  )))





server<-function(input, output,session){
  
  #เรียงข้อมูลใหม่
  dat63.2<-arrange(dat63.2,dat63.2$midterm)
  
  
  filterID<-reactive({
    filter(dat63.2, id==input$id)
  })
    
  
  
  
  
#HOME   #เปลี่ยนที่อยู่ไฟล์รูปภาพ####
  output$chula <- renderImage({
    list(
      src = "1.png",
        filetype = "image/png",
        alt = "pic1"
      )
  },deleteFile=FALSE)
  
  output$noid<- renderText({
    req(input$id)
    if(nrow(as.data.frame(filterID()))=="1"){"รหัสนิสิตถูกต้อง"}
    else{"รหัสนิสิตไม่ถูกต้อง กรุณากรอกใหม่"}
   
  })
  
  observeEvent(input$toNext,{
    req(input$id)
    updateTabsetPanel(session, "inTabset",
                                               selected = "panel2")
  })
 
  
  observeEvent(input$to2,{updateTabsetPanel(session, "inTabset",
                                               selected = "panel3")
  })
  

  
  
  #page2 score
  
  
  
  output$studentName<-renderText({
    paste("ชื่อ-สกุล : ", unlist(filterID()$name),unlist(filterID()$lastname))
    
    
    })
  
  output$idstudent<-renderText({
    paste("รหัสนิสิต : ", input$id)
  
    })
  
  

  output$point<-renderText({
    
    paste0("คะแนนกลางภาคของนิสิต : ",unlist(filterID()$midterm))
    
    })
  
  output$summary<-renderPrint(summary(dat63.2$midterm))
  
  
  
  output$plot<-renderPlot({
    
    
    if(input$charts=="Dot plot"){if(input$top>0){ggplot(dat63.2) + geom_dotplot(aes(midterm),
                                                                               alpha=0.8, 
                                                                               binwidth = 0.8, 
                                                                               fill=ifelse(dat63.2$id==input$id, "Red", "#8C9494")) + labs(x="Midterm score", y="Propotion")+theme_classic()}
      
                                else {ggplot(dat63.2) + geom_dotplot(aes(midterm),
                                   alpha=0.8, 
                                   binwidth = 0.8, 
                                   fill= "#8C9494") + labs(x="Midterm score", y="Propotion")+theme_classic()}}
    else if(input$charts=="Boxplot"){if(input$top>0){boxplot(dat63.2$midterm,horizontal = TRUE, col="#8C9494", xlab = "Midterm score")
      stripchart(dat63.2$midterm, 
                 method = "stack", 
                 cex = 0.8,
                 pch  = 19,
                 col =  "#2A3038", 
                 add=TRUE)
      stripchart(filterID()$midterm, 
                 method = "stack",
                 pch  = 13,
                 cex  = 4,
                 lwd  = 4,
                 col  = "red", 
                 add = TRUE)}
      else{boxplot(dat63.2$midterm,horizontal = TRUE, col="#8C9494", xlab = "Midterm score")
        stripchart(dat63.2$midterm, 
                   method = "stack", 
                   cex = 0.8,
                   pch  = 19,
                   col =  "black", 
                   add=TRUE)}
      }
  
    else{ggplot(dat63.2) + geom_histogram(aes(midterm))+theme_classic()}
    
    })
  
  
  
  
#สร้างการทำนาย
 
  output$Namest<-renderText({
    paste("ชื่อ-สกุล : ", unlist(filterID()$name),unlist(filterID()$lastname))
    
    
  })
  
  output$idst<-renderText({
    paste("รหัสนิสิต : ", input$id)
    
  })
  
  datasetInput <- reactive({   
     data.frame(
      midterm = filterID()$midterm,
      att = input$att,
      work = input$work ,
      stringsAsFactors = FALSE) })

  pre <- reactive({predict(model5,newdata = datasetInput())})
    
  output$pre1<-renderText({
    
    req (input$submitbutton)
     if(pre()>"75") {"pass"}
      else if (pre()>"65"){"worry"}
      else{"fall"}
      
  })
 
  
  #Calculation complete
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("ทำนายเสร็จเรียบร้อย") 
    } 
    else {
      return("ระบบพร้อมทำนายผล")
    }
  })
  
  #submitbutton
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })

#สิ้นสุด
  
  output$vpre<- renderText({
    req(input$submitbutton)
    
    if( pre()>"75"){"ผลการเรียนของคุณอยู่ในกลุ่มผ่าน เป็นสัญญาณที่ดีในการเรียน แต่คุณอย่าประมาทจงรักษาคุณภาพและมาตรฐานการเรียนของคุณไว้เพื่อให้ผลลัพธ์ปลายเทอมเป็นที่น่าพอใจ"}
    else if( pre()>"65"){" ผลการเรียนของคุณอยู่ในกลุ่มเสี่ยง  ควรวางแผนการเรียนเพื่อพัฒนาผลการเรียนและทำคะแนนปลายภาคให้สูงเพื่อให้ผลการเรียนของคุณสูงขึ้น"}
    else{" ผลการเรียนของคุณมีโอกาสตก คุณต้องรีบพัฒนาตนเองโดยต้องทำคะแนนปลายภาคให้สูงขึ้นและศึกษาหาความรู้เพิ่มเติมเพื่อให้ผลการเรียนของคุณดีขึ้น "}
  })
  
  



}




shinyApp(ui=ui, server=server)

?fileInput

