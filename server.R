library(pacman)
p_load(shiny,grid,tidyverse,maptools,broom,rgeos,rgdal,sp,micromap)



shinyServer(function(input,output){
  
    # variables to control the sequence of processes 
    controlVar <- reactiveValues(fileReady = FALSE, tableReady = FALSE,checkReady = FALSE)
    
    # to keep the data upload
    dta <- NULL
    TYPE <- NULL
    
    # allow user to upload their own data but should follow some rules
    observeEvent(input$file1, {
      controlVar$fileReady <- FALSE
      if (is.null(input$file1))
        return()
      inFile <- input$file1
      dta <<- read.csv(inFile$datapath)
      if(!is.data.frame(dta))
        return()
      controlVar$fileReady <- TRUE
    })
    
    # show buttons only when file is uploaded
    output$buttonsUI <- renderUI({
      if (controlVar$fileReady)
        div(
          actionButton('go','go')
        )
    })
    
    # make sure choose plot type
    observeEvent(input$type, {
      controlVar$checkReady <- FALSE
      if (is.null(input$type))
        return()
      if (length(input$type) >1){
        output$warning <- renderText("Error: can't select two or more type")
        return()
      }
      if (input$type == 1){
        TYPE <<- "dot"
      }else if (input$type == 2){
        TYPE <<- "dot_cl"
      }else if (input$type == 3){
        TYPE <<- "bar"
      }else if (input$type == 4){
        TYPE <<- "bar_cl"
      }else{
        TYPE <<- "box_summary"
      }
      controlVar$checkReady <- TRUE
    })
    
    #should press go button and shiny will start plot
    observeEvent(input$go, {
      controlVar$tableReady <- FALSE
      
      # simulate running a cool script on dat
      Sys.sleep(2)
      controlVar$tableReady <- TRUE  
    })
  
  #create map file
  Taiwan_map <- readOGR("TWN_adm2.shp")
  tw.polys <- create_map_table(Taiwan_map, "NAME_2")
  tmp <- data.frame(ID = unique(tw.polys$ID),
                    NAME = rep(NA,22))
  tmp$NAME <- c("金門縣","連江縣","新竹市","高雄市","新北市","台中市",
                "台南市","台北市","桃園市","彰化縣","嘉義市","嘉義縣",
                "新竹縣","花蓮縣","基隆市","苗栗縣","南投縣","澎湖縣",
                "屏東縣","台東縣","宜蘭縣","雲林縣")
  tw.polys <- left_join(tw.polys,tmp,by = c("ID","ID"))
  
  #let user specify grouping numbers
  observeEvent(input$number,{
    number <<- input$number
  })
  
  #let user specify panel header
  observeEvent(input$header,{
    header <<- input$header
  })
  
  #let user specify x axis title
  observeEvent(input$xtitle,{
    xtitle <<- input$xtitle
  })
  
  #
  
  #control output plot
  output$out.plot <- renderPlot({
    if (controlVar$fileReady == TRUE & controlVar$tableReady == TRUE & controlVar$checkReady == TRUE){
      if(TYPE == "dot_cl"){
        mmplot(stat.data = dta, map.data = tw.polys,
               panel.types = c("dot_legend","labels", "dot_cl", "map"),
               panel.data = list(NA, "NAME",
                                 list('mean', 'Low_95', 'Up_95'), NA),
               ord.by = "mean", grouping = number, median.row = FALSE,
               map.link = c("NAME","NAME"), plot.height = 20, plot.width = 30,
               colors = brewer.pal(9, "BuPu")[3:9],
               map.color2 = "lightgray",
               map.all = TRUE,
               panel.att = list(
                 list(1, point.type =20, point.size = 2),
                 list(2, header = "縣市", panel.width = 0.5,
                      align = "left", text.size = 0.8),
                 list(3, header = header, 
                      graph.bgcolor = brewer.pal(8,"Greys")[1],
                      xaxis.ticks = list(N1, N2, N3, N4, N5 ),
                      xaxis.labels = list(N1, N2, N3, N4, N5 ),
                      xaxis.title = xtitle),	
                 list(4, header = "",
                      inactive.border.color = gray(.7), inactive.border.size = 1.5,
                      panel.width = 1.2)
               ))
      }else if(TYPE == "dot"){
        mmplot(stat.data = dta, map.data = tw.polys,
               panel.types = c("dot_legend","labels", "dot", "map"),
               panel.data = list(NA, "NAME",
                                 list('mean'), NA),
               ord.by = "mean", grouping = number, median.row = FALSE,
               map.link = c("NAME","NAME"), plot.height = 20, plot.width = 30,
               colors = brewer.pal(9, "BuPu")[3:9],
               map.color2 = "lightgray",
               map.all = TRUE,
               panel.att = list(
                 list(1, point.type =20, point.size = 2),
                 list(2, header = "縣市", panel.width = 0.5,
                      align = "left", text.size = 0.8),
                 list(3, header = header, 
                      graph.bgcolor = brewer.pal(8,"Greys")[1],
                      xaxis.ticks = list(N1, N2, N3, N4, N5 ),
                      xaxis.labels = list(N1, N2, N3, N4, N5 ),
                      xaxis.title = xtitle),	
                 list(4, header = "",
                      inactive.border.color = gray(.7), inactive.border.size = 1.5,
                      panel.width = 1.2)
               ))
      }else if(TYPE == "bar"){
        mmplot(stat.data = dta, map.data = tw.polys,
               panel.types = c("dot_legend","labels", "bar", "map"),
               panel.data = list(NA, "NAME",
                                 list('mean'), NA),
               ord.by = "mean", grouping = number, median.row = FALSE,
               map.link = c("NAME","NAME"), plot.height = 20, plot.width = 30,
               colors = brewer.pal(9, "BuPu")[3:9],
               map.color2 = "lightgray",
               map.all = TRUE,
               panel.att = list(
                 list(1, point.type =20, point.size = 2),
                 list(2, header = "縣市", panel.width = 0.5,
                      align = "left", text.size = 0.8),
                 list(3, header = header, 
                      graph.bgcolor = brewer.pal(8,"Greys")[1],
                      xaxis.ticks = list(N1, N2, N3, N4, N5 ),
                      xaxis.labels = list(N1, N2, N3, N4, N5 ),
                      xaxis.title = xtitle),	
                 list(4, header = "",
                      inactive.border.color = gray(.7), inactive.border.size = 1.5,
                      panel.width = 1.2)
               ))
      }else if(TYPE == "bar_cl"){
        mmplot(stat.data = dta, map.data = tw.polys,
               panel.types = c("dot_legend","labels", "bar_cl", "map"),
               panel.data = list(NA, "NAME",
                                 list('mean', 'Low_95', 'Up_95'), NA),
               ord.by = "mean", grouping = number, median.row = FALSE,
               map.link = c("NAME","NAME"), plot.height = 20, plot.width = 30,
               colors = brewer.pal(9, "BuPu")[3:9],
               map.color2 = "lightgray",
               map.all = TRUE,
               panel.att = list(
                 list(1, point.type =20, point.size = 2),
                 list(2, header = "縣市", panel.width = 0.5,
                      align = "left", text.size = 0.8),
                 list(3, header = header, 
                      graph.bgcolor = brewer.pal(8,"Greys")[1],
                      xaxis.ticks = list(N1, N2, N3, N4, N5),
                      xaxis.labels = list(N1, N2, N3, N4, N5),
                      xaxis.title = xtitle),	
                 list(4, header = "",
                      inactive.border.color = gray(.7), inactive.border.size = 1.5,
                      panel.width = 1.2)
               ))
      }else{
        mmplot(stat.data = dta, map.data = tw.polys,
               panel.types = c("dot_legend", "labels", "box_summary", "map"),
               panel.data = list(NA, "NAME",
                                 list("Min", "Q1", "Median", "Q3", "Max"), NA),
               ord.by = "Median", grouping = number, median.row =FALSE,
               map.link = c("NAME","NAME"), plot.height = 20, plot.width = 30,
               colors = brewer.pal(9, "BuPu")[3:9],
               map.color2 = "lightgray",
               panel.att = list(
                 list(1, point.type = 20, point.size = 2),
                 list(2, header = "縣市", panel.width = 0.5,
                      align = "left", text.size = 0.8),
                 list(3, header = header, 
                      graph.bgcolor = brewer.pal(8,"Greys")[1],
                      xaxis.ticks = list(Q1, Q2, Q3, Q4, Q5),
                      xaxis.labels = list(Q1, Q2, Q3, Q4, Q5),
                      xaxis.title = xtitle, graph.bar.size = 0.4),	
                 list(4, header = "",
                      inactive.border.color = gray(.7), inactive.border.size = 2,
                      panel.width = 1.5)
               ))
      }

    }
    controlVar$tableReady <- FALSE
  })
  
  ##can change values and reprint another plot
  output$buttonsUI <- renderUI({
    if (controlVar$fileReady)
      div(
        actionButton('go','go')
      )
  })
  
})

