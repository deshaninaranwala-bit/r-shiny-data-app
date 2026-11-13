library(shiny)
library(ggplot2)
library(bslib)

all_names<-ls("package:datasets")
df_names<-all_names[sapply(all_names,function(x)is.data.frame(get(x)))]

ui <- fluidPage(
  
  theme=bs_theme(bootswatch = "flatly"),
  
  titlePanel("Inbuilt Data Analyser"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("select1",label="Select dataset",choices=df_names),
      radioButtons("select4",label="What graph you need?",choices=list("Scatterplot","Histogram","Line Chart")),
      selectInput("select2",label="x-axis",choices=NULL),
      conditionalPanel(
        condition="input.select4 !='Histogram'",
        selectInput("select3",label="y-axis",choices=NULL)),
      conditionalPanel(
        condition="input.select4=='Scatterplot'",
        selectInput("color_var",label="Color by(Optional)",choices=c("None")))),
      
    mainPanel(plotOutput("plot"))))

server <- function(input, output,session) {
  
 my_data<-reactive({
   req(input$select1)
   get(input$select1,"package:datasets")
 })
 
 observe({
   req(my_data())
   cols<-colnames(my_data())
   updateSelectInput(session,"select2",choices=cols)
   updateSelectInput(session,"select3",choices=cols)
   updateSelectInput(session,"color_var",choices=c("None",cols))
 })
 
 output$plot<-renderPlot({
   req(input$select2)
   data<-my_data()
   
   req(input$select2 %in% colnames(data))
   
   if(input$select4=="Scatterplot"){
     req(input$select3)
     
     if(input$color_var!="None" && !is.null(input$color_var)){
       p<-ggplot(data,aes(x=.data[[input$select2]],y=.data[[input$select3]],color=.data[[input$color_var]]))+
         geom_point(size=3,alpha=0.8)
     }else{
       p<-ggplot(data,aes(x=.data[[input$select2]],y=.data[[input$select3]]))+
         geom_point(size=3,color="#2c3e50",alpha=0.8)
     }
     p+labs(title="Scatterplot",,xlab=input$select2,ylab=input$select3)+theme_minimal(base_size=14)
   }else if(input$select4=="Histogram"){
     ggplot(data,aes(x=.data[[input$select2]]))+geom_histogram(fill="#3498db",col="white",bins=30)+
       labs(title="Histogram",xlab=input$select2)+theme_minimal(base_size=14)
   }
   else{
     req(input$select3)
     ggplot(data,aes(x=.data[[input$select2]],y=.data[[input$select3]]))+geom_line(color="#e74c3c",linewidth=1)+
       labs(title="Line Chart",xlab=input$select2,ylab=input$select3)+theme_minimal(base_size=14)
   }
 })
}

shinyApp(ui = ui, server = server)
