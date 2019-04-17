


library(shiny)
library(ggplot2)
library(markdown)
library(knitr)
library(jsonlite)
library(httr)
library(dplyr)
library(readxl)
library(XML)
library(stringr)
library(lubridate)
library(tidyr)
library(DT)
library(reader)
library(xlsx)

ui <- fluidPage(tabsetPanel(
  tabPanel("Recoleccion",
           headerPanel("Recoleccion de datos"),
           
           sidebarPanel(
             fileInput('datafile', 'Elegir archivo',
                       accept=c('text/csv', 'text/comma-separated-values,text/plain'))
           ),
           mainPanel(
             dataTableOutput("filetable")
           )
  ),
  tabPanel("Preparacion",
           headerPanel("Preparacion de datos"),
           
           sidebarPanel(   
             sliderInput("idMuestreo", "Deslice el porcentaje de la poblacion del muestreo",min = 0, max = 100, value = 80),
             uiOutput("colOutLiers"),
             uiOutput("colNormalizacion")
           ),
           mainPanel(
             h2("Tabla de muestreo"),
             dataTableOutput("preptable"),
             h2("Tabla de imputacion"),
             dataTableOutput("imputable"),
             h2("Tabla sin valores outliers"),
             dataTableOutput("outlierstable"),
             h2("Tabla normalizada"),
             dataTableOutput("normaltable")
           )
  ),
  tabPanel("Exploracion",
           sidebarLayout(
             sidebarPanel(   
               headerPanel("Transformacion de datos"),
               uiOutput("colSelect"),
               textInput("inputQuery","Ingresar busqueda iniciando con df %>%"),
               selectInput("selectData", "Seleccionar tipo de archivo",c('csv',"xls","xlsx","tsv","json","xml"),selected = FALSE),
               downloadButton('downloadData', 'Descargar')
             ),
             mainPanel(
               h2("Tabla de transformacion"),
               dataTableOutput("transtable")
             )
           ),
           sidebarLayout(
             sidebarPanel(   
               headerPanel("Exploracion de datos"),
               uiOutput("colExpSelect"),
               uiOutput("colExpSelectOne"),
               textOutput("Med"),
               textOutput("Max"),
               textOutput("Min"),
               tableOutput("Quantile")
             ),
             mainPanel(
               h2("Grafico de dispersion"),
               plotOutput("expPlot")
             )
           )
  ),
  tabPanel("Analisis",
           headerPanel("Analisis de datos"),
           sidebarPanel(   
             uiOutput("colX"),
             uiOutput("colY")
           ),
           mainPanel(
             h2("Linea Regresional"),
             plotOutput("plotRegresion")
           )
  ),
  tabPanel("Visualizacion",
           sidebarLayout(
             sidebarPanel(
               titlePanel("", "Administracion de la informacion"),
               h2("Grafico dinamico:"),
               textInput("inputQueryVisual","Realizar ggplot con df")
             ),
             mainPanel(
               h4("Salida de datos"),
               plotOutput("idSalidaDinamica")
             )
           ),
           sidebarLayout(
             sidebarPanel(
               titlePanel("", "Administracion de la informacion"),
               h2("Grafico 1:"),
               h4("Temperatura promedio segun el mes"),
               selectInput("idMesG1", "Mes:",c(
                 "Todos" = "",
                 "Enero" = "jan",
                 "Febrero"="feb",
                 "Marzo"="mar",
                 "Abril"="apr",
                 "Mayo"="may",
                 "Junio"="jun",
                 "Julio"="jul",
                 "Agosto"="aug",
                 "Setiembre"="sep",
                 "Octubre"="oct",
                 "Noviembre"="nov",
                 "Diciembre"="dec")
               )
             ),
             mainPanel(
               h4("Salida de datos"),
               plotOutput("idSalida1")
               
             )
           ),
           sidebarLayout(
             sidebarPanel(
               titlePanel("", "Administracion de la informacion"),
               h2("Grafico 2:"),
               h4("Numero de incendios por mes"),
               sliderInput("idSlider", "Deslice para filtrar la cantidad",min = 1, max = 200, value = c(1,200))
             ),
             mainPanel(
               h4("Salida de datos"),
               plotOutput("idSalida2")
               
             )
           ),
           sidebarLayout(
             sidebarPanel(
               titlePanel("", "Administracion de la informacion"),
               h2("Grafico 3:"),
               h4("Top de meses con mayores perdidas"),
               selectInput("idTop", "Top:",c(
                 "Top 3" = 3,
                 "Top 5" = 5)
               )
             ),
             mainPanel(
               h4("Salida de datos"),
               plotOutput("idSalida3")
               
             )
           )
  ),
  tabPanel("Enviar correo",
           sidebarLayout(
           
           sidebarPanel(
             textInput(inputId = "txtRemitente", label = "De:", placeholder = "from@gmail.com",value = ""),
             passwordInput(inputId = "txtPasswd",label = "Password",placeholder = "Password",value = ""),
             textInput(inputId = "txtPara", label = "Para:", placeholder = "para@*******.com",value = ""),
             textInput(inputId = "txtAsunto", label = "Asunto:",value = ""),
             textAreaInput(inputId = "txtMensaje",label = "Mensaje",placeholder = "Su mensaje aqui"),
             actionButton(inputId = "btnEnviar", "Enviar correo"),
             div(
               style="text-align:center;
                height:50px;
                color:red;
                font-size: 15px;
                padding-top:10px;
                position:relative;",
               textOutput("txtError")
             ),
             div(
               style="text-align:center;
                height:50px;
                color:green;
                font-size: 15px;
                padding-top:10px;
                position:relative;",
               textOutput("txtResult")
             )
           ),
           mainPanel(
           )
  ))
)
)

server <- function(input, output) {
  filedata<- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    ext <- get.ext(infile$datapath)
    if(ext=="xlsx"){
      read_xlsx(infile$datapath)
    }
    else if(ext=="xls"){
      read_xls(infile$datapath)
    }
    else if(ext=="json"){
      fromJSON(infile$datapath)
    }
    else if(ext=="xml"){
      xmlToDataFrame(infile$datapath)
    }
    else{
      reader(infile$datapath)
    }
  })
  output$filetable <- renderDataTable({
    filedata()
  })
  
  output$colOutLiers <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectizeInput("selecOutLiers", "Columnas a para borrar valores outliers",items,multiple = TRUE)
  })
  output$colNormalizacion <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectizeInput("selecNormalizacion", "Columnas a normalizar",items,multiple = TRUE)
  })
  output$preptable<- renderDataTable({
    df <-filedata()
    if (is.null(df)) return(NULL)
    id_muestreo = sample(1:NROW(df), NROW(df)*as.numeric(input$idMuestreo)/100)
    ##dataset de entrenamieno (80%)
    entrenamiento = df[id_muestreo,]
    entrenamiento
  })
  output$imputable<- renderDataTable({
    df <-filedata()
    if (is.null(df)) return(NULL)
    datosInput <- sapply(df, function(x) ifelse(is.na(x),mean(as.double(x),na.rm = T),x))
    datosInput <- as.data.frame(datosInput)
    datosInput
  })
  output$imputable<- renderDataTable({
    df <-filedata()
    if (is.null(df)) return(NULL)
    datosInput <- sapply(df, function(x) ifelse(is.na(x),mean(as.double(x),na.rm = T),x))
    datosInput <- as.data.frame(datosInput)
    datosInput
  })
  output$outlierstable<- renderDataTable({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$selecOutLiers)) return(NULL)
    remove_outliers <- function(x) {
      qnt <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
      H <- 1.5 * IQR(x, na.rm = TRUE)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      y
    }
    remove_outliers2<-function(x){
      datacol <- as.double(as.character(x))
      x<- remove_outliers(datacol)
      x
    }
    for(i in input$selecOutLiers){
      df[,as.character(i) ]<- remove_outliers2(df[,as.character(i) ])
    }
    
    datosLimpios = na.omit(df)
    datosLimpios
  })
  output$normaltable<- renderDataTable({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$selecNormalizacion)) return(NULL)
    normalizar <- function(x) {
      maximo = max(x)
      minimo = min(x)
      x <- sapply(x, function(x) (x -minimo)/(maximo-minimo))
      x
    }
    normalizar2<-function(x){
      datacol <- as.double(as.character(x))
      x<- normalizar(datacol)
      x
    }
    for(i in input$selecNormalizacion){
      df[,as.character(i) ]<- normalizar2(df[,as.character(i) ])
    }
    df
  })
  
  ##Transformacion
  
  output$colSelect <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectizeInput("selecSelec", "Columnas a seleccionar",items,multiple = TRUE)
  })
  getTranstable<- reactive({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (!is.null(input$selecSelec)) {df <- df %>% select(input$selecSelec)}
    if (input$inputQuery != "") {
      c <- parse(text = as.character(input$inputQuery))
      df <- eval(c) 
    }
    df
  })
  
  output$transtable<- renderDataTable({
    getTranstable()
  })
  
  
  
  #Exploracion
  output$colExpSelect <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectizeInput("selecExpSelect", "Columnas a seleccionar para la dispersion",items,multiple = TRUE)
  })
  output$colExpSelectOne <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("selecExpSelectOne", "Columna a explorar",items,selected = FALSE)
  })
  output$expPlot<- renderPlot({
    df <-filedata()
    if (is.null(df)) return(NULL)
    cad<-NULL
    if (is.null(input$selecExpSelect)) return(NULL)
    for(i in input$selecExpSelect){
      cad <- c(cad,as.character(i))
    }
    plot(df[,cad])
  })
  
  output$Med<- renderText({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$selecExpSelectOne)) {return(NULL)}
    paste("La mediana de ",
          median(df[,as.character(input$selecExpSelectOne)]),sep = paste(input$selecExpSelectOne," es: ") )
  })
  output$Min<- renderText({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$selecExpSelectOne)) {return(NULL)}
    paste("El minimo de ",
          min(df[,as.character(input$selecExpSelectOne)]),sep = paste(input$selecExpSelectOne," es: ") )
  })
  output$Max<- renderText({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$selecExpSelectOne)) {return(NULL)}
    paste("El maximo de ",
          max(df[,as.character(input$selecExpSelectOne)]),sep = paste(input$selecExpSelectOne," es: ") )
  })
  output$Quantile <- renderTable({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$selecExpSelectOne)) {return(NULL)}
    #quantile<-quantile(df[,as.character(input$selecExpSelectOne)], prob=seq(0, 1, length = 5))
    quantile_df <- function(x, probs, na.rm =F, names = F, type = 7, ...){
      z <- quantile(x, probs, na.rm, names, type)
      return(data.frame(Q = probs, values = z))
    }
    
    quantile_df(df[,as.character(input$selecExpSelectOne)], probs = c(0.0,0.25, 0.50,0.75,1))
  })
  
  
  #Analisis
  output$colX <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("selectX", "Elija el eje X",items,selected = FALSE)
  })
  output$colY <- renderUI({
    df <-filedata()
    if (is.null(df)) return(NULL)
    
    items=names(df)
    names(items)=items
    selectInput("selectY", "Elija el eje Y",items,selected = FALSE)
  })
  output$plotRegresion<- renderPlot({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if (is.null(input$selectX)) {return(NULL)}
    if (is.null(input$selectY)) {return(NULL)}
    modelo <- lm(df[,as.character(input$selectY)] ~ df[,as.character(input$selectX)], data = df)
    Ys <- predict(modelo,df) #Valores Predichos
    b0 <- round(modelo$coefficients[1],2)
    b1 <- round(modelo$coefficients[2],2)
    qplot(x = df[,as.character(input$selectX)], y = df[,as.character(input$selectY)],data = df, 
          main = paste(as.character(input$selectX)," vs ",as.character(input$selectY)), xlab = as.character(input$selectX),
          ylab = as.character(input$selectY), geom = c("point")) + geom_line(aes(y=Ys), lwd = 1.2, color = 4) +
      geom_text(x=median(df[,as.character(input$selectX)]),y=max(df[,as.character(input$selectY)]),aes(label = paste(as.character(input$selectY),"^", " = ", b0, " + ", b1, "*",as.character(input$selectX))))
  })
  
  
  #Visualizacion
  output$idSalidaDinamica <- renderPlot({
    df <-filedata()
    if (is.null(df)) return(NULL)
    if(input$inputQueryVisual != "")
    {
      c <- parse(text = as.character(input$inputQueryVisual))
      c<- eval(c) 
    }
    c
  })
  format(Sys.time(), "%d%m%d%Y%H%M%S%OS")
  Sys.time()
  #download
  tryCatch({
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', format(Sys.time(), "%d%m%d%Y%H%M%S%OS"), paste(".",input$selectData,sep = ""), sep='')
    },
    content = function(con) {
      if(input$selectData == "csv")
      {
        write.csv2(getTranstable(), con)
      }
      if(input$selectData == "xlsx"||input$selectData == "xls")
      {
        write.xlsx2(getTranstable(),sheetName = "Sheet1",file = con)
      }
      
      if(input$selectData == "tsv")
      {
        write.table(getTranstable(),sep = '\t',file = con,row.names = FALSE)
      }
      if(input$selectData == "json")
      {
        write(getTranstable()%>% toJSON(dataframe = "rows", raw = "mongo"),file = con)
#        getTranstable()%>% toJSON(dataframe = "rows", raw = "mongo")
      }
      
      if(input$selectData == "xml")
      {
        tmpdata<-getTranstable()
        xml <- xmlTree()
        xml$addTag("document", close=FALSE)
        for (i in 1:NROW(tmpdata)) {
          xml$addTag("row", close=FALSE)
          for (j in names(tmpdata)) {
            xml$addTag(j, tmpdata[i, j])
          }
          xml$closeTag()
        }
        xml$closeTag()
        saveXML(xml,file = con) 
      }
    })
  },error = function(e) {   print(e)  },finally = {return(200)})
  
  datos <- read.csv("/home/rstudio/ProyectoFinal/dataset/forestfires.csv",stringsAsFactors = FALSE)
  output$idSalida1=renderPlot({
    ggplot(datos %>% filter(str_detect(month,input$idMesG1)) %>% group_by(month) 
           %>% summarise(temperatura = mean(temp)),aes(x=month,y=temperatura,fill=month))+geom_bar(stat ="identity")
  })
  output$idSalida2=renderPlot({
    ggplot(datos %>% group_by(month) %>% summarise(cantidad = n()) %>% filter(input$idSlider[1]<cantidad & cantidad<input$idSlider[2]),aes(x=month,y=cantidad,fill=month))+geom_bar(stat ="identity")
  })
  output$idSalida3=renderPlot({
    ggplot(datos%>% group_by(month) %>% summarise(perdida=sum(area)) %>% arrange(desc(perdida)) %>%
             head(as.numeric(input$idTop)), aes(x=1, y=perdida,fill=month)) + geom_bar(stat='identity', colour='black') + coord_polar(theta='y')
  })
  
  #Enviar Correo
  tryCatch({
  
  observeEvent(input$btnEnviar,{
    sender<-input$txtRemitente
    password<-input$txtPasswd
    recipient<-input$txtPara
    subject<-input$txtAsunto
    msg<-input$txtMensaje
    body = list(sender = sender,password = password,recipient=recipient,subject=subject,msg=msg)
    result <- POST("http://35.237.66.122:8000/sendMail",body = body,encode = "json",verbose())
    Output <- content(result)
    if(Output == 200)
    {
      output$txtResult <- renderText({
        "El mensaje fue enviado con exito."})
    }else{
      output$txtError <- renderText({
        "Hubo un error al enviar el mensaje."}
      )
    }
  })},error = function(e){print(e)},finally = {print("ok")})
}


shinyApp(ui = ui, server = server)

