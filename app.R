#
# ggplotr: This is a shiny app to plot data in a csv file
#
# Written 2018 by Anushi Shah and Gordon McDonald
#

library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(plotly)
library(rbokeh)


# Define server logic required to draw a histogram
server<-shinyServer(function(input, output,session) {
  
  set.seed(122)
  
  # Adjust file upload limit to 100 mb
  options(shiny.maxRequestSize = 100*1024^2)
  
  ## Reactive table to print metadata
  output$tableOfData <- reactiveTable(function() {
    
    if (is.null(input$dataFile)) {
      # User has not uploaded a file yet, use a default one
      return(NULL)
    }
    
    dataDf <<- read.csv(input$dataFile$datapath,header = TRUE)
    subsetdataDf <<- dataDf[input$colNames]
    #return(subsetdataDf)
  })
  
  ## Multi-select drop down box for selecting meta data columns
  
  output$dataColumns <- reactiveUI(function() {
    
    if (is.null(input$dataFile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    
    dataDf <- read.csv(input$dataFile$datapath,header = TRUE)
    dataColumnNames <- colnames(dataDf)
    
    pickerInput(
      inputId = "colNames", 
      label = "Select columns for visualization:", 
      choices = dataColumnNames, 
      selected = dataColumnNames,
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `live-search` = TRUE
      ), 
      multiple = TRUE
    )
    
  })
  
  ##### Code to Add plots on click #############
  plotOutput <- function(id)
  {
    xInput = paste0("xcol", id)
    yInput = paste0("ycol", id)
    colInput = paste0("colcol", id)
    sizeInput = paste0("sizecol",id)
    
    renderPlotly( ggplot(subsetdataDf, 
                       aes_string(x = input[[xInput]], 
                                  y = input[[yInput]],
                                  color = input[[colInput]],
                                  size = input[[sizeInput]])) +
                  geom_point() )
    
  }
  
  
  removeMeButton <- function(id) {
    actionButton(inputId = paste0("rmBtn", id), 
                 label = "",
                 icon=icon("times"))
    
  }
  
  ## Fancy dropdown box for input parameters for plots
  PlotOptionsDropDown <- function(id, sel1 = 1, sel2 = 2, sel3 = 1, sel4 = 1)
  {
    
    dropdownButton(
      selectInput(inputId = paste0("xcol", id), label = 'X Variable', choices = names(subsetdataDf), selected = names(subsetdataDf)[[sel1]]),
      selectInput(inputId = paste0("ycol", id), label = 'Y Variable', choices = names(subsetdataDf), selected = names(subsetdataDf)[[sel2]]),
      selectInput(inputId = paste0("colcol", id), label = 'Color Variable', choices = c("'red'",names(subsetdataDf)), selected = c("'red'",names(subsetdataDf))[[sel3]]),
      selectInput(inputId = paste0("sizecol", id), label = 'Size Variable', choices = c(1,names(subsetdataDf)), selected = c(1,names(subsetdataDf))[[sel4]]),
      
      
      circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
      tooltip = tooltipOptions(title = "Click to see inputs !"),
      inputId = paste0("rmDrpBtn", id)
      
    )
  }
  
  #container contains all the UI elements which can be removed/added, as a reactive thinggy.
  #each element of container$uiComponents is itself a list of the remove button and its associated plot.
  container <- reactiveValues(uiComponents = list())
  
  output$container <- renderUI({
    
    container$uiComponents
    
  })
  
  #updates all controls to their old values which were stored below (unless not stored, in which case use default or don't update)
  
  observe({
    #if the 'add' button has been clicked this will re-run, but not if it 
    #hasn't rendered properly yet (NULL) and not if it hasn't been clicked yet (0)
    if(is.null(input$addBtn) || input$addBtn == 0) return()
    
    isolate({
      #collects all control values or all plots and stores them in a list
      
      
      #append new plot+controls to UIcomponents list
      id = as.integer(runif(1,1e5,1e6-1))
      
      uiComponentCount <- length(container$uiComponents)
      
      
      container$uiComponents <- 
        append(
          container$uiComponents, 
          list(
            list(
              "dropdownBtn" = PlotOptionsDropDown(id),
              "plotOut" = plotOutput(id),
              "removeBtn" = removeMeButton(id),
              br(),br(),
              hr()
            )
          ))
      
      # only execute if there already is at least one plot
      if(uiComponentCount>0){
        
        #collect all the ids of object already on screen (not the one we just added)
        allTheIds <- sapply(container$uiComponents, function(uiCom) {
          substring(uiCom$removeBtn$attribs$id,first=6,last=1000L) 
        })
        
        #loop through all the ids of things on screen and not the one we just added, 
        #replacing their dropdowns with new ones with the on-screen selected item 
        #as their default selection for when it is redrawn.
        for (iter in 1:(length(allTheIds)-1)) {
          #id of this plot
          myId <- allTheIds[iter]
          
          #strings to address each dropdown selection
          xInput <- paste0("xcol", myId)
          yInput <- paste0("ycol", myId)
          colInput <- paste0("colcol", myId)
          sizeInput <- paste0("sizecol", myId)
          
          #replace the dropdowns with new ones with the right things selected
          container$uiComponents[[iter]]["dropdownBtn"] <- 
            list(PlotOptionsDropDown(myId,
                                  sel1 = which(names(subsetdataDf)==input[[xInput]]),
                                  sel2 = which(names(subsetdataDf)==input[[yInput]]),
                                  sel3 = which(c("'red'",names(subsetdataDf))==input[[colInput]]),
                                  sel4 = which(c(1,names(subsetdataDf))==input[[sizeInput]])
            ))
          
        }}
      
      
    }) ## End of isolate
  }) ## End of observe
  
  
  ## REMOVE 
  observe({
    if(length(container$uiComponents) == 0) return()

    #get ids of all remove buttons
    rmBtnIds <- sapply(container$uiComponents, function(uiCom) {
      uiCom$removeBtn$attribs$id
    })
    
    #get value of all remove buttons (are any of them currently being clicked?)
    rmBtnVals <- sapply(rmBtnIds, function(btnId) input[[btnId]])
    
    #if any of them are still null and not zero then return
    if(any(sapply(rmBtnVals, is.null))) return()
    #if(any(sapply(drpBtnVals, is.null))) return()
    
    #if they are all zero (none are being clicked) then return
    if(all(rmBtnVals == 0)) return()
    #if(all(drpBtnVals == 0)) return()
    
    #what's left over: one of the buttons must be being clicked...then run the next bit
    isolate({
      #set the element in the list of UI components which contains the remove button which is 
      #being clicked to NULL which means remove the element. (this element also contains the 
      #associated plot object)
      container$uiComponents[[which(rmBtnVals > 0)]] <- NULL
      
      if(length(container$uiComponents) == 0) return()
      
      #check that all the other inputs are listed correctly and overwrite
      allTheIds <- sapply(container$uiComponents, function(uiCom) {
        substring(uiCom$removeBtn$attribs$id,first=6,last=1000L) 
      })
      
      #loop through all the ids of things on screen and not the one we just added, 
      #replacing their dropdowns with new ones with the on-screen selected item 
      #as their default selection for when it is redrawn.
      for (iter in 1:(length(allTheIds))) {
        #id of this plot
        myId <- allTheIds[iter]
        
        #string to address x-dropdown and y-dropdown
        xInput <- paste0("xcol", myId)
        yInput <- paste0("ycol", myId)
        colInput <- paste0("colcol", myId)
        sizeInput <- paste0("sizecol", myId)
        
        #replace the dropdowns with new ones with the right things selected
        container$uiComponents[[iter]]["dropdownBtn"] <- 
          list(PlotOptionsDropDown(myId,
                                sel1 = which(names(subsetdataDf)==input[[xInput]]),
                                sel2 = which(names(subsetdataDf)==input[[yInput]]),
                                sel3 = which(c("'red'",names(subsetdataDf))==input[[colInput]]),
                                sel4 = which(c(1,names(subsetdataDf))==input[[sizeInput]])
          ))
        
      }
      
    })
  })
  
  ### End of Code to Add plots on click #############
  
}) ## End of shinyServer method

# DASHBOARD UI

## ui.R ##
  sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Upload Data", icon = icon("file"),tabName = "uploadData"),
    
    menuItem("Visualisations", icon = icon("area-chart"),tabName="VizTab")
  ) ## End of SidebarMenu 
  
) ## End of Sidebar 



body <- dashboardBody(
  tabItems(
    tabItem(tabName = "uploadData",
            h2("Upload a .csv file"),
            
            sidebarPanel(
              fileInput(inputId = "dataFile", 
                        label = "Upload data (.csv format):",
                        multiple = FALSE)
            ),
            
            mainPanel(
              textOutput("fileprint"),
              
              uiOutput("dataColumns"),
              tableOutput("tableOfData")
              
              
            ) # End of main panel
            
    ),## End of uploadData tabName 
    
    tabItem(tabName = "VizTab",
            conditionalPanel( condition = "output.tableOfData",
                              actionButton("addBtn", "Add Plot"),
                              br(),br(),
                              uiOutput("container")
            ) # End of conditionalPanel
    ) ## End of VizTab tab
    
  )
)

# Put them together into a dashboardPage
ui<-shinyUI(dashboardPage(
  dashboardHeader(title = "ggplotr",
                  titleWidth = 300),
  sidebar,
  body
))

#run the app
shinyApp(ui = ui, server = server)
