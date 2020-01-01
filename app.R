library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(rsconnect)
library(ggplot2)
library(ggforce)
library(gridExtra)
library(DT)
library(readxl)
library(jsonlite)
library(formattable)
library(rsconnect)


clut <- read.csv(file = "un_cgidd_country_lut.csv",stringsAsFactors = FALSE)[,-1]
clut <- filter(clut,!is.na(NameCountry))
cmd_lut <- read.csv(file = "cmd_lut.csv",stringsAsFactors = FALSE)

Logged = FALSE;
my_username <- "user1"
my_password <- "1qaz2wsx"

mycss <- "
#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
}
"
#ui1 <- function(){
#  tagList(h3("Please Sign in"),
#          div(id = "login",
#              wellPanel(textInput("userName", "Username"),
#                        passwordInput("passwd", "Password"),
#                        br(),actionButton("Login", "Log in"))),
#          tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
#  )}

#ui2 <- function(){tagList(tabPanel("Test"))}


#ui = (htmlOutput("page"))
#ui2 <- fluidPage(
ui <- function() {
  dashboardPage(
    
    #theme = shinytheme("journal"),
    #titlePanel(title = div(img(src="2018 canback_logo_long.jpg",height = 60, width = 255))),
    dashboardHeader(
      title = "United Nations Comtrade Analysis",
      titleWidth = 400,
      dropdownMenu(
        type = "messages",
        messageItem(
          from = "Customer Support",
          message = "It usually take about 2 minutes to retrieve data from UN Comtrade. Thanks for your patience"
        )
      )),
    
    
    
    dashboardSidebar(
      width = 400,
      sidebarMenu(
        h4("Input Section"),
        textInput(inputId = "country", label = "Please Enter Country Name", value = "China",width = 400),
        
        selectInput(inputId = "year", label = "Please Select Year", choices = c(1990:2018), selected = 2017,width = 400),
        actionButton("submit","Submit"),
        br(),
        h4("Output Tabs"),
        menuItem("Application Introduction",tabName = "Introduction",icon = icon("angle-right"),badgeLabel = "New",badgeColor = "green"),
        menuItem("Exports Imports Component Analysis",tabName = "Component",icon = icon("angle-right")),
        menuItem("International Trade Partners Analysis",tabName = "Partners",icon = icon("angle-right")),
        menuItem("Dataset Overview",tabName = "Dataset",icon = icon("angle-right")),
        br(),
        h4("Data Downloading Center"),
        selectInput(inputId = "dataset", label = "Please Select the Data to Download", choices = c("Exports","Imports","Partners"),width = 400),
        
        radioButtons(inputId = "type", label = "Format type:",
                     choices = c("Excel (CSV)","Text (TSV)","Text (Space Separated)","Doc")),
        downloadButton("downloadData",label = "Download")
      )
    ),
    
    dashboardBody(
      tabItems(
          #type = "pills",
          
        tabItem(
            tabName = "Introduction",
            fluidRow(
              box(
                width = 12,
                h2("Introduction"),
                h4("This shiny app conducts International Trade Analysis for 213 countries in 29 years (1990-2018)\n"),
                h4("Please find raw data in United Nations Comtrade Database website: https://comtrade.un.org/data/\n\n\n"),
                h4(),
                h1("\n"),
                h4("Functions of this App:\n"),
                h4("1. Allow customers to input the name of country and select the year to study on"),
                h4("2. Exports and imports categories similar to sample deck are available under Components tab"),
                h4("3. Major international trade of partners and their corresponding proportions are available under Partners tab"),
                h4("4. Customers could view the actual dataset under Dataset tab. By selecting dataset and type, customers could download corresponding dataset with required format")
              )
              
            )
            
          ),
          
        tabItem(
            tabName = "Component",
            fluidRow(
              box(
                em("Country Exports Breakdown"),
                br(),
                plotOutput("export")
              ),
              box(
                em("Country Imports Breakdown"),
                br(),
                plotOutput("import")
              )
            ),
            
            fluidRow(
              box(
                selectInput(inputId = "exportCat",label = "Export Categories",
                            choices = c("Animal products","Plant products","Minerals and organic chemicals","Inorganic and man made chemicals","Manufactured goods","Vehicles and machinery","Other")),
                plotOutput("exportAll")
              ),
              box(
                selectInput(inputId = "importCat",label = "Import Categories",
                            choices = c("Animal products","Plant products","Minerals and organic chemicals","Inorganic and man made chemicals","Manufactured goods","Vehicles and machinery","Other")),
                plotOutput("importAll")
             )
             
             ),
            
            fluidRow(
              box(
                tableOutput("refexptable")
            ),
            box(
              tableOutput("refimptable")
            )
            )
          ),
        
          
          tabItem(
            tabName = "Partners",
            fluidRow(
              box(
                selectInput(inputId = "partnerType",label = "Partner Type",choices = c("Import Partners","Export Partners"),selected = "Export Partners"),
                plotOutput("partners"),
                width = 12
              )
            ),
            
            fluidRow(
              valueBoxOutput("totalTrade",width = 6),
              valueBoxOutput("topPartner",width = 6)
            )
          ),
            
          
          tabItem(
            tabName = "Dataset",
            DT::dataTableOutput("dataset")
          )
        )
      )
      
  )
}





server <- function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
    }
    if (USER$Logged == TRUE) 
    {
      output$page <- renderUI({
        ui2()
      })
      #print(ui)
    }
  })
  
  tradedata <- eventReactive(input$submit,{
    
    countryfilter <- dplyr::filter(clut, NameCountry == as.character(input$country))
    codecountry <- as.numeric(countryfilter$id)
    tradeurl <- paste0("http://comtrade.un.org/api/get?max=50000&type=C&freq=A&px=HS&ps=",as.numeric(input$year),"&r=",codecountry,"&p=0&rg=all&cc=ALL&fmt=json")
    
    fromJSON(tradeurl)$dataset
    
  })
  
  partnerdata <- eventReactive(input$submit,{
    
    countryfilter <- dplyr::filter(clut, NameCountry == as.character(input$country))
    codecountry <- as.numeric(countryfilter$id)
    partnerurl <- paste0("http://comtrade.un.org/api/get?max=50000&type=C&freq=A&px=HS&ps=",as.numeric(input$year),"&r=",codecountry,"&p=all&rg=all&cc=TOTAL&fmt=json")
    
    fromJSON(partnerurl)$dataset
  })
  
  exportdata <- reactive({
    
    tradedata <- tradedata()
    exportdata <- filter(tradedata, rgCode == 2)
    exportdata <- exportdata[-length(exportdata[,1]),]
    
    exportdata %>%
      mutate(CommodityCode = as.numeric(cmdCode)) %>%
      filter(CommodityCode < 99) %>%
      left_join(cmd_lut, by = c('CommodityCode' = 'Code')) #%>%
      #group_by(Category) %>%
      #summarize(value = sum(TradeValue)) %>%
      #arrange(desc(value))
    
  })
  
  importdata <- reactive({
    
    tradedata <- tradedata()
    importdata <- filter(tradedata,rgCode == 1)
    importdata <- importdata[-length(importdata[,1]),]
    
    importdata %>%
      mutate(CommodityCode = as.numeric(cmdCode)) %>%
      filter(CommodityCode < 99) %>%
      left_join(cmd_lut, by = c('CommodityCode' = 'Code')) #%>%
      #group_by(Category) %>%
      #summarize(value = sum(TradeValue)) %>%
      #arrange(desc(value))
    
  })
  
  exporttabdata <- reactive({
    
    tradedata <- tradedata()
    exportdata <- filter(tradedata, rgCode == 2)
    exportdata <- exportdata[-length(exportdata[,1]),]
    
    exportdata %>%
      mutate(CommodityCode = as.numeric(cmdCode)) %>%
      filter(CommodityCode < 99) %>%
      left_join(cmd_lut, by = c('CommodityCode' = 'Code')) %>%
      group_by(Category) %>%
      summarize(value = sum(TradeValue)) %>%
      arrange(desc(value))
    
  })
  
  importtabdata <- reactive({
    
    tradedata <- tradedata()
    importdata <- filter(tradedata,rgCode == 1)
    importdata <- importdata[-length(importdata[,1]),]
    
    importdata %>%
      mutate(CommodityCode = as.numeric(cmdCode)) %>%
      filter(CommodityCode < 99) %>%
      left_join(cmd_lut, by = c('CommodityCode' = 'Code')) %>%
      group_by(Category) %>%
      summarize(value = sum(TradeValue)) %>%
      arrange(desc(value))
    
  })
  
  partneralldata <- reactive({
    
    partneralldata <- partnerdata()
    
    partneralldata %>%
      filter(ptCode != 0, rgCode %in% c(1,2)) %>%
      group_by(rgDesc,ptTitle) %>%
      summarize(value = sum(TradeValue)) %>%
      inner_join(clut,by =c("ptTitle" = "text")) %>%
      select("CodeCountry"=id,NameCountry,"Series"=rgDesc,"Value" = value)
    
  })
  
  datasetInput <- reactive({
    
    
    switch(input$dataset,
           "Exports" = exporttabdata(),
           "Imports" = importtabdata(),
           "Partners" = partneralldata())
    
  })
  
  fileext <- reactive({
    
    switch(input$type,
           "Excel (CSV)" = "csv","Text (TSV)" = "txt","Text (Space Separated)" = "txt","Doc" = "doc")
    
  })
  
  output$export <- renderPlot({
    
    exportdata <- exportdata()
    exportdata %>%
      group_by(Category) %>%
      summarize(value = sum(TradeValue)) %>%
      arrange(desc(value)) %>%
      mutate(total = sum(value)) %>%
      mutate(Percent = percent(value/total, digits = 1)) %>%
      mutate(end = 2 * pi * cumsum(value)/sum(value),
             start = lag(end, default = 0),
             middle = 0.5 * (start + end),
             hjust = ifelse(middle > pi, 1, 0),
             vjust = ifelse(middle < pi/2 | middle > 3 * pi/2 ,0, 1)) %>%
      ggplot() +
      theme_void() +
      geom_arc_bar(aes(x0 = 0,y0 = 0, r0 = 0, r = 1,
                       start = start, end = end, fill = Category), color = "transparent")+
      scale_fill_manual(values = c("#969696","#00B0F0","#A68A4A","#111565","#FFC000","#D6D7D9","#FFFFCC","#A3CE5E")) +
      geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Percent, hjust = hjust, vjust = vjust)) +
      theme(legend.position = "right", text = element_text(size = 14)) +
      coord_fixed() +
      scale_x_continuous(limits = c(-1.2, 1.15),  # Adjust so labels are not cut off
                         name = "", breaks = NULL, labels = NULL) +
      scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                         name = "", breaks = NULL, labels = NULL)
    
  })
  
  output$import <- renderPlot({
    
    importdata <- importdata()
    
    importdata %>%
      group_by(Category) %>%
      summarize(value = sum(TradeValue)) %>%
      arrange(desc(value)) %>%
      mutate(total = sum(value)) %>%
      mutate(Percent = percent(value/total, digits = 1)) %>%
      mutate(end = 2 * pi * cumsum(value)/sum(value),
             start = lag(end, default = 0),
             middle = 0.5 * (start + end),
             hjust = ifelse(middle > pi, 1, 0),
             vjust = ifelse(middle < pi/2 | middle > 3 * pi/2 ,0, 1)) %>%
      ggplot() +
      theme_void() +
      geom_arc_bar(aes(x0 = 0,y0 = 0, r0 = 0, r = 1,
                       start = start, end = end, fill = Category), color = "transparent")+
      scale_fill_manual(values = c("#969696","#00B0F0","#A68A4A","#111565","#FFC000","#D6D7D9","#FFFFCC","#A3CE5E")) +
      geom_text(aes(x = 1.05 * sin(middle), y = 1.05 * cos(middle), label = Percent, hjust = hjust, vjust = vjust)) +
      theme(legend.position = "right", text = element_text(size = 14)) +
      coord_fixed() +
      scale_x_continuous(limits = c(-1.2, 1.15),  # Adjust so labels are not cut off
                         name = "", breaks = NULL, labels = NULL) +
      scale_y_continuous(limits = c(-1, 1),      # Adjust so labels are not cut off
                         name = "", breaks = NULL, labels = NULL)
  })
  
  output$exportAll <- renderPlot({
    
    exportdata <- exportdata()
    exportdata %>%
      filter(Category == input$exportCat) %>%
      #mutate(value = sum(TradeValue)) %>%
      group_by(CommodityCode) %>%
      summarize(value = sum(TradeValue)) %>%
      ggplot(aes(x=as.character(CommodityCode),y=value)) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_bar(stat = "identity",fill = "lightblue") +
      theme(axis.title.y = element_blank(),axis.title.x = element_blank())
    
  })
  
  output$importAll <- renderPlot({
    
    importdata <- importdata()
    importdata %>%
      filter(Category == input$importCat) %>%
      #mutate(value = sum(TradeValue)) %>%
      group_by(CommodityCode) %>%
      summarize(value = sum(TradeValue)) %>%
      ggplot(aes(x=as.character(CommodityCode),y=value)) +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_bar(stat = "identity",fill = "lightblue") +
      theme(axis.title.y = element_blank(),axis.title.x = element_blank())
    
  })
  
  
  output$partners <- renderPlot({
    
    partnerdata <- partnerdata()
    
    partnerexport <- partnerdata %>%
      filter(ptCode != 0, rgCode == 2) %>%
      group_by(ptTitle) %>%
      summarize(value = sum(TradeValue)) %>%
      inner_join(clut,by =c("ptTitle" = "text"))
    
    
    
    partnerimport <- partnerdata %>%
      filter(ptCode != 0, rgCode == 1) %>%
      group_by(ptTitle) %>%
      summarize(value = sum(TradeValue)) %>%
      inner_join(clut,by =c("ptTitle" = "text"))
    
    gexp <- 
      partnerexport %>%
      filter(!is.na(NameCountry)) %>%
      mutate(total = sum(value)) %>%
      mutate(percentage = percent(value/total,digits = 1)) %>%
      arrange(desc(percentage)) %>%
      head(10) %>%
      ggplot(aes(x= reorder(NameCountry, percentage), y = percentage)) +
      #ggtitle("Main Export Partners") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_bar(stat = "identity",fill = "lightblue") +
      #scale_y_continuous(breaks = c(0,0.2,0.05)) +
      #ylim(c(0,0.3))+
      geom_text(aes(label = percentage),vjust = -0.5) +
      theme(axis.title.y = element_blank(),axis.title.x = element_blank()) #+
      #theme(plot.margin = unit(c(0.05,0.01,0.01,0.01),"cm")) +
      #coord_flip()
    
    gimp <- partnerimport %>%
      filter(!is.na(NameCountry)) %>%
      mutate(total = sum(value)) %>%
      mutate(percentage = percent(value/total,digits = 1)) %>%
      arrange(desc(percentage)) %>%
      head(10) %>%
      ggplot(aes(x= reorder(NameCountry, percentage), y = percentage)) +
      #ggtitle("Main Import Partners") +
      theme(panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      geom_bar(stat = "identity",fill = "lightblue") +
      #scale_y_continuous(breaks = c(0,0.2,0.05)) +
      #ylim(c(0,0.3))+
      geom_text(aes(label = percentage),vjust = -0.5) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank()) #+
      #theme(plot.margin = unit(c(0.01,0.01,0.01,0.01),"cm")) +
      #coord_flip()
    
    if (input$partnerType == "Export Partners") {
      gexp
    } else {
      gimp
    }
    
  })
  
  output$totalTrade <- renderValueBox({
    
    if (input$partnerType == "Export Partners") {
      exportdata <- exportdata()
      value <- round(sum(exportdata$TradeValue)/1e9,1)
      valueBox(
        paste0(value,"B"),"Total Export",icon = icon("list")
        )
    } else {
      importdata <- importdata()
      value <- round(sum(importdata$TradeValue)/1e9,1)
      valueBox(
        paste0(value,"B"),"Total Import",icon = icon("list")
      )
    }
    
  })
  
  output$topPartner <- renderValueBox({
    
    partnerdata <- partnerdata()
    
    partnerexport <- partnerdata %>%
      filter(ptCode != 0, rgCode == 2) %>%
      group_by(ptTitle) %>%
      summarize(value = sum(TradeValue)) %>%
      inner_join(clut,by =c("ptTitle" = "text")) %>%
      filter(!is.na(NameCountry)) %>%
      arrange(desc(value))
    
    topexport <- partnerexport$NameCountry[1]
    
    
    
    partnerimport <- partnerdata %>%
      filter(ptCode != 0, rgCode == 1) %>%
      group_by(ptTitle) %>%
      summarize(value = sum(TradeValue)) %>%
      inner_join(clut,by =c("ptTitle" = "text")) %>%
      filter(!is.na(NameCountry)) %>%
      arrange(desc(value))
    
    topimport <- partnerimport$NameCountry[1]
    
    if (input$partnerType == "Export Partners") {
      valueBox(
        topexport,"Top Export Partner",icon = icon("thumbs-up", lib = "glyphicon")
        )
    } else {
      value <- round(sum(importdata$value)/1e9,1)
      valueBox(
        topimport,"Top Import Partner",icon = icon("thumbs-up", lib = "glyphicon")
      )
    }
  })
  
  output$dataset <- DT::renderDataTable({
    
    DT::datatable(datasetInput())
    
  })
  
  output$refexptable <- renderTable({
    
    cmd_lut %>%
      filter(Category == input$exportCat) %>%
      select(-Category)
    
  })
  
  output$refimptable <- renderTable({
    
    cmd_lut %>%
      filter(Category == input$exportCat) %>%
      select(-Category)
    
  })
  
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste(input$dataset, fileext(), sep = ".")
    },
    
    content = function(file) {
      sep <- switch(input$type, "Excel (CSV)" = ",","Text (TSV)" = "\t","Text (Space Separated)" = " ","Doc" = " ")
      write.table(datasetInput(), file, sep = sep,row.names = F)
      
    }
  )

}

shinyApp(ui, server)
#shinyApp()
