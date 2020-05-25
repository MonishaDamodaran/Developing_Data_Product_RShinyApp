#initialize
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(forecast)



#DEFAULT DATA
AirPassengers <- as.data.frame(AirPassengers)
AirPassengers$index = 1:nrow(AirPassengers)


#plotting theme for ggplot2
.theme<- theme(
    axis.line = element_line(colour = 'gray', size = .75),
    panel.background = element_blank(),
    plot.background = element_blank()
)


# UI for app
ui<-(fluidPage(
    # title
    titlePanel("Univariate Time Series Plot"),
    
    #input
    sidebarLayout(
    sidebarPanel
    (
        # Input: Select a file ----
        
        fileInput("file1", "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Semicolon = ";",
                                 Comma = ",",
                                 Tab = "\t"),
                     selected = ","),
        # Horizontal line ----
        tags$hr(),
        
        
        # Input: Select what to display
        selectInput("dataset","Data:",
                    choices =list(AirPassengers = "AirPassengers", mtcars = "mtcars",
                                  uploaded_file = "inFile"), selected=NULL),
        selectInput("variable","Variable:", choices = NULL, selected = NULL),
        selectInput("group","Group:", choices = NULL,selected = NULL),
        selectInput("plot.type","Plot Type:",
                    list(linegraph = "linegraph", none = "none")),
        selectInput("acfpcf", "acfpacf", list(acf = "acf", pacf = "pacf" 
                                                  )),
    
        
        checkboxInput("show.points", "show points", TRUE)
    ),
    
    # output
    mainPanel(
        h3(textOutput("caption")),
        #h3(htmlOutput("caption")),
        uiOutput("plot"),
        uiOutput("plot1"),# depends on input
        tabsetPanel(
                    tabPanel("Documentation",
                             br(),
                             p("The app name Univariate Time series Analysis signifies a 
                                                          very simple visualization of univariate time series data"),
                             p("The R dataset strong(AirPassengers) is used here. The dataset
                                                          is first converted to a data frame object since it is a ts object. 
                                                          The year and index column is been mutated to AirPassengers data."),
                             br(),
                             
                             strong("LEFT PANE"),
                             
                             br(),
                             
                             p("1) The left pane consists of a browse box to upload files,"),
                             p("2) The check box to get header of the imported files,"),
                             p("3) The radio button to select the type of imported files,"),
                             p("4) The drop down list displaying the data name strong(AirPassengers)
                                                          by default and if the file has uploaded from your drive select 
                                                          upload files from the list,"),
                             p("5) The variable and group drop down boxes are column names of 
                                                          the selected data. It is used as a x axis and y axis for plots,"),
                             p("6) The Plot type box consists of option to get strong(Linegraph or None",),
                             p("7) Acf and Pacf plots are displayed in main panel by selecting 
                                                          the required one from acfpacf dropdown list,"),
                             p("8) The final checkbox gives the option to display the points 
                                                          in the plotted graph."),
                             
                             br(),
                             
                             strong(em("Once the app is opened, it displays the linegraph of the variable x vs
                                                           x of AirPassengers data. Choosing the group to Index shows the actual line 
                                                           chart of Passengers data over time")),
                             
                             br(),
                             
                             p("I have provided an example time series dataset in my 
                                                          github page to try visualizing the plots using my app")
                             
                             
                    ))
        
        
        
        
    )
)))


# shiny server side code for each call
server<-(function(input, output, session){
    
    #update group and
    #variables based on the data
    observe({
        #browser()
        if(!exists(input$dataset)) return() #make sure upload exists
        var.opts<-colnames(get(input$dataset))
        updateSelectInput(session, "variable", choices = var.opts)
        updateSelectInput(session, "group", choices = var.opts)
    })
    
    output$caption<-renderText({
        switch(input$plot.type,
               "linegraph" 	= 	"linegraph"
               )
              # "density" 	=	"Density plot",
              # "bar" 		=	"Bar graph")
    })
    
    
    output$caption1 <- renderText({
        switch(input$acfpcf,
               "acf" = "acf",
               "pacf" = "pacf")
    })
    
    output$plot <- renderUI({
        plotOutput("p")
    })
    
    
    output$plot1 <- renderUI({
        plotOutput("q")
    })
    
    #get data object
    get_data<-reactive({
        
        if(!exists(input$dataset)) return() # if no upload
        
        check<-function(x){is.null(x) || x==""}
        if(check(input$dataset)) return()
        
        
        obj<-list(data=get(input$dataset),
                  variable=input$variable,
                  group=input$group
        )
        
        #require all to be set to proceed
        if(any(sapply(obj,check))) return()
        #make sure choices had a chance to update
        check<-function(obj){
            !all(c(obj$variable,obj$group) %in% colnames(obj$data))
        }
        
        if(check(obj)) return()
        
        
        obj
        
    })
    
    #plotting function using ggplot2
    
    
    

    output$p <- renderPlot({
        
        data = get(input$dataset)
        data = as.data.frame(data)
        var.opts<-colnames(data)
        plot.obj<-get_data()
        
        #conditions for plotting
        #if(is.null(plot.obj)) return()
        
        #make sure variable and group have loaded
        if(plot.obj$variable == "" | plot.obj$group =="") return()
        
        
        if(input$plot.type=="linegraph")	{
            #control for 1D or 2D graphs
            # if (class(plot.obj$group == "date"){
            #     input$group = as.Date(plot.obj$group)
            # }
            p<-ggplot(plot.obj$data,
                      aes_string(
                          x 		= plot.obj$group,
                          y 		= plot.obj$variable
                          #fill 	= plot.obj$group # let type determine plotting
                      )
            ) + geom_line()
            
        if(input$show.points==TRUE)
            {
                p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
            }
            
        } 
         
        p<-p+labs(
            fill 	= input$group,
            x 		= "",
            y 		= input$variable
        )  +
            .theme
        print(p)
    })
    
    getContents <- reactive({
        # input$upload_file will be NULL initially. After the user selects and uploads a file
        req(input$upload_file)
        uf <<- read.csv(input$dataset$datapath)
        uf[is.na(uf)] <- 0
        uf$date <- as.Date(uf$date, format = "%Y-%m-%d")
        return(uf)
    })
    
    output$dataTable <- renderTable({		
        getContents()
    })
    
    output$q <- renderPlot({
        
        data = get(input$dataset)
        var.opts<-colnames(get(input$dataset))
        
        plot.obj<-get_data()
        
        #conditions for plotting
        if(is.null(plot.obj)) return()
        
        #make sure variable and group have loaded
        if(plot.obj$variable == "" | plot.obj$group =="") return()
        
        c = data[var.opts == plot.obj$variable]
                          
        
        
        if(input$acfpcf =="acf")	{	
            acf(c)
            
        }else {
            pacf(c)
        } 
        
    })        
    
    # set uploaded file
    upload_data<-reactive({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        #could also store in a reactiveValues
        read.csv(inFile$datapath,
                 header = input$header,
                 sep = input$sep)
    })
    
    observeEvent(input$file1,{
        inFile<<-upload_data()
    })
    
    
})


# Create Shiny app ----
shinyApp(ui, server)