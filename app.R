library(rsconnect)
library(shiny)
# library(faosws)
# library(faoswsUtil)
# library(faoswsStandardization)
library(data.table)
library(ggplot2)
library(reshape2)
library(highcharter)
library(dplyr)
library(DT)
library(plotly)

load("data/oldSua.RData")
load("data/tree.Rdata")
load("data/parentNodes.RData")
load("data/namedCPC.RData")
load("data/rankTableNamed.RData")
load("data/crudeBalEl.RData")



setnames(namedCPC,c("measuredItemSuaFbs","measuredItemSuaFbs_description"),c("CPCcode","Description"))
emptyCPCtable=data.table(CPCcode="/",Description="/")


oldSua=oldSua[timePointYears%in%c(2000:2013)]
# manually remove one duplicated value for Angola 
oldSua=oldSua[!(geographicAreaM49=="24"&measuredElementSuaFbs=="production"
                &measuredItemSuaFbs=="0112"&Status!="")]



oldUtil <- oldSua[!(measuredElementSuaFbs%in%c("production","imports"))]
setnames(oldUtil,colnames(oldSua)[1:8],c("geographicAreaM49","Country","years","Value","Status","Element","commodity_Code","commodity"))

oldUtil=oldUtil[,1:8,with=FALSE]

oldSupply <- oldSua[(measuredElementSuaFbs%in%c("production","imports")),]
setnames(oldSupply,colnames(oldSua)[1:8],c("geographicAreaM49","Country","years","Value","Status","Element","commodity_Code","commodity"))
oldSupply=oldSupply[,1:8,with=FALSE]


# Calulate Utilization Table

UtilTableAll <- data.table(data.frame(oldUtil))
UtilTableFinal=unique(UtilTableAll)

setnames(oldSua,colnames(oldSua)[1:8],c("geographicAreaM49","Country","years","Value","Status","Element","commodity_Code","commodity"))
countries <- c("",oldSua[,unique(Country)])
commodities <- c("",oldSua[,unique(commodity)])
parentComm <- oldSua[commodity_Code%in%parentNodes,unique(commodity)]
parentComm <- c(parentComm,"2161", "2162", "2165", "2166", "2167", "2168", "21112", "21117.01", 
                "21117.02", "21118.02", "21118.03", "21119.01", "21123", "21152", 
                "21159.01", "21159.02", "21170.01", "21170.93", "21511.03", "21513", 
                "21519.02", "21519.03", "21631.01", "21631.02", "21641.01", "21641.02", 
                "21673", "21691.01", "21691.02", "21691.03", "21691.04", "21691.07", 
                "21691.08", "21691.09", "21691.11", "21691.12", "21691.13", "21691.14", 
                "23210.04", "23210.06", "23210.03", "23210.05", "01499.07", "21691.10", 
                "21691.90", "23620", "01241.02", "21433", "21433.01", "24212.02", 
                "24310.01", "24230.01", "24230.02", "24230.03", "24310.02", "24310.03", 
                "24310.04", "2413", "21511.02", "21512.01", "22241.01", "22241.02", 
                "22242.01", "22242.02", "22249.01", "22120", "24110", "23511.02", 
                "01193", "01199.02", "2351f")
supplies <- oldSua[SU=="supply",unique(Element)]
utilizations <- oldSua[SU=="utilization",unique(Element)]
oldSua=oldSua[,1:8,with=FALSE]


SingleBal = crudeBalEl[,.N,by=c("measuredItemSuaFbs","measuredItemFbsSua_description","measuredElementSuaFbs")]

SingleBal[,sumN:=sum(N),by=c("measuredItemFbsSua_description")]
SingleBal[,perc:=paste0(round((N/sumN)*100,2),"%")]
SingleBal=SingleBal[order(measuredItemFbsSua_description)]
SingleBal=SingleBal[!is.na(measuredItemFbsSua_description)]

plotCol=c("green","blue","purple","orange","red")

statusTable=data.table(StatusFlag=oldSua[,unique(Status)],description=c("Single balancer","Official", 
                                                                        "Calculated","Trend (carry forward)","Fao Estimation", 
                                                                        "Semi Official","Missing","Trading Partner", "Imputed"))

ui <- navbarPage(strong("RANKING TABLE APPROACH AT SUA FILLING"),
                 tabPanel(span(strong("RANKING TABLE"), style="color:red"),
                          br(),
                          div(dataTableOutput(outputId = "UtiTable2"),style="font-size:90%",spacing="xs")             
                 ),
                 tabPanel(span(strong("Exploring Old Sua"),style = "color:blue"),
                          br(),
                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                              selectInput(inputId = "country", label = "Select a Country", 
                                          choices = countries, selected = "", multiple = FALSE,
                                          selectize = TRUE, width = NULL, size = NULL)),
                          div(style="display: inline-block;vertical-align:top; width: 300px;",
                              selectInput(inputId = "whatToVisualize", label = "Single commodity or Tree?", 
                                          choices = c("","Single commodity", "tree"), selected = "", multiple = FALSE,
                                          selectize = TRUE, width = NULL, size = NULL)),
                          # Only show this if the selection is "tree" 
                          div(style="display: inline-block;vertical-align:top; width: 300px;",conditionalPanel(
                            condition = 'input.whatToVisualize == "tree"',
                            selectInput(inputId = "primary", label = "Select a PRIMARY EQUIVALENT commodity", 
                                        choices = c("",parentComm), selected = "", multiple = FALSE,
                                        selectize = TRUE, width = NULL, size = NULL)
                          ),
                          conditionalPanel(
                            condition = 'input.whatToVisualize == "Single commodity"',
                            selectInput(inputId = "commodity", label = "Select a Commodity", 
                                        choices = c("",commodities), selected = "", multiple = FALSE,
                                        selectize = TRUE, width = NULL, size = NULL))
                          ),
                          hr(),    
                          tabsetPanel(
                            tabPanel(span("Sua Tables",style="color:blue"),
                                     br(),
                                     h4("Tables always show Supplies and Utilizations"),
                                     h5("Absolute Values and Status flags (S) are reported"),
                                     conditionalPanel(
                                       condition = 'input.whatToVisualize == "Single commodity"',
                                       br(),
                                       h4(textOutput("tableTitle")),
                                       br(),
                                       div(dataTableOutput(outputId = "tableSingle"),style="font-size:90%",spacing="xs")
                                     ),
                                     conditionalPanel(
                                       condition = 'input.whatToVisualize == "tree"',
                                       br(),
                                       h4(textOutput("tableTreeTitle")),
                                       br(),
                                       uiOutput("tablesTitles"),
                                       uiOutput("tables")
                                     )
                            ),
                            tabPanel(span("Sua Plots",style="color:blue"),
                                     h4("Plots are ratio values"),
                                     h5("Supplies are proportion of Total Supplies"),
                                     h5("Utilizations are proportion of Total Utilizations"),
                                     selectInput(inputId = "supUtilAll", label = "What do you want to see?", 
                                                 choices = c("Supplies and Utilizations", "Only Supplies", "Only Utilizations"), selected = "Supplies and Utilizations", multiple = FALSE,
                                                 selectize = TRUE, width = NULL, size = NULL),
                                     conditionalPanel(
                                       condition = 'input.whatToVisualize == "Single commodity"',
                                       br(),
                                       h4(textOutput("plotTitle_single")),
                                       br(),
                                       highchartOutput("plotCode",width = "600px",height = "500px"),
                                       align = "center"
                                     ),
                                     conditionalPanel(
                                       condition = 'input.whatToVisualize == "tree"',
                                       br(),
                                       h4(textOutput("plotTitle_tree")),
                                       br(),
                                       uiOutput("hchart"),
                                       align = "center"
                                     )
                            ),
                            tabPanel(span("Status flags Legend", style="color:blue"),
                                     br(),
                                     div(dataTableOutput(outputId = "StatusTable"),style="font-size:90%",spacing="xs")
                                     
                            )
                          )
                 )
                 
)       


server <- function(input, output) {
  
  ######################### STATIC outputs  
  output$UtiTable2 <- renderDataTable({
    datatable(rankTableNamed[order(geographicAreaM49_description,measuredItemFbsSua,rank)],
              colnames=c("Country_code", "Country","BalancingElement","commodity_Code","commodity","RANK","inverseRANK"),
              filter = "top",options = list(pageLength = 50))
  },options = list(scrollX = FALSE))
  
  output$StatusTable <- renderDataTable({
    datatable(statusTable
              ,filter = "top",options = list(pageLength = 50))
  },options = list(scrollX = FALSE))
  
  
  ######################### other outputs
  
  printcountry = reactive({
    suppressWarnings(oldSua[Country==input$country,unique(geographicAreaM49)])
  })
  
  # OUTPUT single Code 
  printCodes_single = reactive({
    oldSua[commodity==input$commodity,unique(commodity_Code)]
  })
  
  BalElementC <- reactive({
    crudeBalEl[measuredItemSuaFbs==printCodes_single()&geographicAreaM49==printcountry(),unique(measuredElementSuaFbs)]
  })
  
  filteredData_code <- reactive({
    tbl_df(oldSua[geographicAreaM49==printcountry()
                  &commodity_Code==printCodes_single(),c(3,6,4),with=FALSE])
  })
  
  filteredData_plot <- reactive({
    
    if(dim(filteredData_code())[1]!=0){
      dcast(filteredData_code(),years ~ Element, value.var="Value"  )
    }
  })
  
  filteredData_codeT <- reactive({
    tbl_df(oldSua[geographicAreaM49==printcountry()
                  &commodity_Code==printCodes_single(),c(3,5,6,4),with=FALSE])
  })
  
  
  
  filtData_Util <- reactive({  
    if(dim(filteredData_code())[1]!=0){
      filteredData_plot()[,c(1,which(colnames(filteredData_plot())%in%utilizations))]
    }
  })
  
  filtData_sup <- reactive({  
    if(dim(filteredData_code())[1]!=0){
      filteredData_plot()[,c(1,which(colnames(filteredData_plot())%in%supplies))]
    }  
  })
  
  filteredData_codeTS1<- reactive ({
    if(dim(filteredData_codeT())[1]!=0){
      dcast(filteredData_codeT(),years ~ Element, value.var="Status"  )
    }
  })
  filteredData_codeTS<- reactive ({
    if(dim(filteredData_codeT())[1]!=0){
      setnames(filteredData_codeTS1(),c("years",paste0(colnames(filteredData_codeTS1())[2:length(colnames(filteredData_codeTS1()))],"_S")))
    }
  })  
  filteredData_Value <- reactive({
    if(dim(filteredData_codeT())[1]!=0){
      dcast(filteredData_codeT(),years ~ Element, value.var="Value"  )
    }
  })
  
  filteredData_TableS <- reactive({
    data.table(cbind(filteredData_Value(),filteredData_codeTS()[,2:length(filteredData_codeTS()[1,])]))
  })
  
  filteredData_Table <- reactive({
    filteredData_TableS()[,c("years",sort(colnames(filteredData_TableS()[,2:length(colnames(filteredData_TableS())),with=FALSE]))),with=FALSE]
  })
  
  
  ## plot
  
  
  observeEvent(input$supUtilAll,
               {
                 observeEvent(input$commodity,
                              { output$plotTitle_single <- renderText({
                                suppressWarnings(paste0(input$country," (M49=", printcountry(),")"))
                              })
                              
                              if(input$commodity==""){
                                output$plotCode <- renderHighchart({NULL})
                              }else{
                                if(dim(filteredData_code())[1]==0){
                                  output$plotCode <- renderHighchart({NULL})
                                }else{
                                  if(input$supUtilAll=="Supplies and Utilizations"){
                                    output$plotCode <- renderHighchart({
                                      hc = highchart()
                                      hc = hc_title(hc, text = paste0(input$commodity," (CPC=",printCodes_single(),", Single Balancer = ",BalElementC(),")"))
                                      hc = hc_xAxis(hc,categories = filtData_Util()$years)
                                      for (i in 2: dim(filtData_Util())[2]){
                                        hc = hc_add_series(hc, name = colnames(filtData_Util())[i],data = round(filtData_Util()[[i]],4))
                                      }
                                      for (i in 2: dim(filtData_sup())[2]){
                                        hc = hc_add_series(hc, name = colnames(filtData_sup())[i],data = round(filtData_sup()[[i]],4),dashStyle= "LongDash")
                                      }
                                      hc
                                    })
                                  }else{
                                    if(input$supUtilAll=="Only Supplies"){
                                      output$plotCode <- renderHighchart({
                                        hc = highchart()
                                        hc = hc_title(hc, text = paste0(input$commodity," (CPC=",printCodes_single(),", Single Balancer = ",BalElementC(),")"))
                                        hc = hc_xAxis(hc,categories = filtData_Util()$years)
                                        for (i in 2: dim(filtData_sup())[2]){
                                          hc = hc_add_series(hc, name = colnames(filtData_sup())[i],data = round(filtData_sup()[[i]],4),dashStyle= "LongDash")
                                        }
                                        hc
                                      })
                                    }else{
                                      if(input$supUtilAll=="Only Utilizations"){
                                        output$plotCode <- renderHighchart({
                                          hc = highchart()
                                          hc = hc_title(hc, text = paste0(input$commodity," (CPC=",printCodes_single(),", Single Balancer = ",BalElementC(),")"))
                                          hc = hc_xAxis(hc,categories = filtData_Util()$years)
                                          for (i in 2: dim(filtData_Util())[2]){
                                            hc = hc_add_series(hc, name = colnames(filtData_Util())[i],data = round(filtData_Util()[[i]],4))
                                          }
                                          hc
                                        })
                                      }
                                    }
                                  } 
                                }
                              }
                              })
               })
  
  
  ## Table
  observeEvent(input$commodity,
               {
                 output$tableTitle <- renderText({
                   suppressWarnings(paste0(input$country," (M49=", printcountry(),") - ",input$commodity," (CPC=",printCodes_single(),", Single Balancer = ",BalElementC(),")" ))
                 })
                 
                 if(input$commodity==""){
                   output$tableSingle <- renderDataTable({emptyCPCtable})
                 }else{
                   if(dim(filteredData_code())[1]==0){
                     output$tableSingle <- renderDataTable({emptyCPCtable})
                   }else{
                     output$tableSingle <- renderDataTable({datatable(filteredData_Table(),options = list(filter = FALSE,searching = FALSE,pageLength = 15,paging = FALSE))
                     }, include.rownames=FALSE)
                   }
                 }
               })
  
  ## code/tree
  named_code = reactive({
    namedCPC[CPCcode%in%printCodes_single()]
  })
  
  output$codeTitle <- renderText({"Code of selected commodity"})
  output$code <- renderDataTable({named_code() })
  
  
  # OUTPUT Tree
  
  code_primary = reactive({
    oldUtil[commodity==input$primary,unique(commodity_Code)]
  })
  
  codes_tree = reactive({
    getChildren(commodityTree = tree,
                parentColname = "measuredItemParentCPC",
                childColname = "measuredItemChildCPC",
                topNodes = code_primary())
  }
  )
  
  BalElementT <- reactive({
    crudeBalEl[measuredItemSuaFbs%in%codes_tree()&geographicAreaM49==printcountry()][,c(3,5),with=FALSE]
  })
  
  BalElementT2 <- reactive({
    setnames(BalElementT(),colnames(BalElementT()),c("commodity_Code","BalancingElement"))
  })
  
  filteredData_tree <- reactive({
    oldSua[geographicAreaM49==printcountry()
           &commodity_Code%in%codes_tree(),c(3,6:8,4),with=FALSE]
  })
  
  filteredData_tree2 <- reactive({
    merge(filteredData_tree(),BalElementT2(),by="commodity_Code")
    
  })
  
  
  
  filteredData_treeT <- reactive({
    oldSua[geographicAreaM49==printcountry()
           &commodity_Code%in%codes_tree(),c(3,5,6:8,4),with=FALSE]
  })
  
  ## plot
  
  values <- reactiveValues(np=NA)
  observeEvent(input$supUtilAll,
               {
                 observeEvent(input$primary,
                              {output$plotTitle_tree <- renderText({
                                suppressWarnings(paste0(input$country," (M49=", printcountry(),") - Plots of commodities in the tree of " ,input$primary, " (CPC=",code_primary(),")" ))
                              })
                              values$np=filteredData_tree()[,length(unique(commodity_Code))]
                              
                              if(input$primary==""){
                                output$hchart <- renderUI({NULL})
                              }else{
                                if(values$np==0){
                                  output$hchart <- renderUI({NULL})
                                }else{
                                  output$hchart <- renderUI({
                                    plot_output_list <- lapply(1:values$np, function(i) {
                                      plotname <- paste("hc", i, sep="")
                                      highchartOutput(plotname,width = "600px",height = "500px")
                                    })
                                    do.call(tagList, plot_output_list)
                                  })
                                }
                                
                                for (h in 1:values$np){
                                  local({
                                    my_i <- h
                                    plotname <- paste0("hc", my_i)
                                    
                                    j=reactive({
                                      filteredData_tree()[,unique(commodity_Code)][my_i]
                                    })
                                    filteredData_treePlot_name <- reactive({
                                      filteredData_tree()[commodity_Code==j(),unique(commodity)]
                                    })
                                    filteredData_treePlot_BE <- reactive({
                                      filteredData_tree2()[commodity_Code==j(),unique(BalancingElement)]
                                    })
                                    filteredData_treePlot_sub1 <- reactive({
                                      tbl_df(filteredData_tree()[commodity_Code==j(),c(1,2,5),with=FALSE])
                                    })
                                    filteredData_treePlot_sub <- reactive({
                                      dcast(filteredData_treePlot_sub1(),years ~ Element, value.var="Value")
                                    })
                                    
                                    filtDataUtil_tree_sub <- reactive({  
                                      if(dim(filteredData_tree())[1]!=0){
                                        filteredData_treePlot_sub()[,c(1,which(colnames(filteredData_treePlot_sub())%in%utilizations))]
                                      }
                                    })
                                    
                                    filtDataSup_tree_sub <- reactive({  
                                      if(dim(filteredData_tree())[1]!=0){
                                        filteredData_treePlot_sub()[,c(1,which(colnames(filteredData_treePlot_sub())%in%supplies))]
                                      }  
                                    })
                                    
                                    hc=list()
                                    if(input$supUtilAll=="Supplies and Utilizations"){
                                      output[[plotname]] <- renderHighchart({
                                        hc[[my_i]]=highchart()
                                        hc[[my_i]] = hc_title(hc[[my_i]], text = paste0(filteredData_treePlot_name()," (CPC=", j(),", Single Balancer = ", filteredData_treePlot_BE(),")"))
                                        hc[[my_i]] = hc_xAxis(hc[[my_i]],categories = filteredData_treePlot_sub()$years)
                                        
                                        for (i in 2:dim(filtDataUtil_tree_sub())[2]){
                                          hc[[my_i]] = hc_add_series(hc[[my_i]], name = colnames(filtDataUtil_tree_sub())[i],data = round(filtDataUtil_tree_sub()[[i]],4))
                                        }
                                        hc[[my_i]]
                                        
                                        for (i in 2:dim(filtDataSup_tree_sub())[2]){
                                          hc[[my_i]] = hc_add_series(hc[[my_i]], name = colnames(filtDataSup_tree_sub())[i],data = round(filtDataSup_tree_sub()[[i]],4),dashStyle= "LongDash")
                                        }
                                        hc[[my_i]]
                                      })
                                    }else{
                                      if(input$supUtilAll=="Only Supplies"){
                                        output[[plotname]] <- renderHighchart({
                                          hc[[my_i]]=highchart()
                                          hc[[my_i]] = hc_title(hc[[my_i]], text = paste0(filteredData_treePlot_name()," (CPC=", j(),", Single Balancer = ", filteredData_treePlot_BE(),")"))
                                          hc[[my_i]] = hc_xAxis(hc[[my_i]],categories = filteredData_treePlot_sub()$years)
                                          for (i in 2:dim(filtDataSup_tree_sub())[2]){
                                            hc[[my_i]] = hc_add_series(hc[[my_i]], name = colnames(filtDataSup_tree_sub())[i],data = round(filtDataSup_tree_sub()[[i]],4),dashStyle= "LongDash")
                                          }
                                          hc[[my_i]]
                                        })
                                      }else{
                                        if(input$supUtilAll=="Only Utilizations"){
                                          output[[plotname]] <- renderHighchart({
                                            hc[[my_i]]=highchart()
                                            hc[[my_i]] = hc_title(hc[[my_i]], text = paste0(filteredData_treePlot_name()," (CPC=", j(),", Single Balancer = ", filteredData_treePlot_BE(),")"))
                                            hc[[my_i]] = hc_xAxis(hc[[my_i]],categories = filteredData_treePlot_sub()$years)
                                            
                                            for (i in 2:dim(filtDataUtil_tree_sub())[2]){
                                              hc[[my_i]] = hc_add_series(hc[[my_i]], name = colnames(filtDataUtil_tree_sub())[i],data = round(filtDataUtil_tree_sub()[[i]],4))
                                            }
                                            hc[[my_i]]
                                          })
                                        }
                                      }
                                    }
                                  })
                                  
                                }
                              }
                              })
               })
  
  
  ## Table
  
  observeEvent(input$primary,
               {
                 output$tableTreeTitle <- renderText({suppressWarnings(paste0(input$country," (M49=", printcountry(),") - Tables of commodities in the tree of " ,input$primary, " (CPC=",code_primary(),")" ))})
                 
                 values$np=filteredData_tree()[,length(unique(commodity_Code))]
                 
                 if(input$primary==""){
                   output$tablesTitles <- renderUI({NULL})
                   output$tables <- renderUI({NULL})
                 }else{
                   output$tables <- renderUI({
                     tables_output_list <- lapply(1:values$np, function(i) {
                       tableName <- paste("tab", i, sep="")
                       dataTableOutput(tableName)
                     })
                     do.call(tagList, tables_output_list)
                   })
                 }
                 
                 for (h in 1:values$np){
                   local({
                     my_i <- h
                     titleName <- paste0("title", my_i)
                     tableName <- paste0("tab", my_i)
                     
                     j=reactive({
                       filteredData_tree()[,unique(commodity_Code)][my_i]
                     })
                     filteredData_treeTable_name <- reactive({
                       filteredData_tree()[commodity_Code==j(),unique(commodity)]
                     })
                     filteredData_treePlot_BE <- reactive({
                       filteredData_tree2()[commodity_Code==j(),unique(BalancingElement)]
                     })
                     filteredData_treeTable_sub1 <- reactive({
                       tbl_df(filteredData_treeT()[commodity_Code==j(),c(1:3,6),with=FALSE])
                     })
                     
                     filteredData_treeTable_subValue <- reactive({
                       dcast(filteredData_treeTable_sub1(),years ~ Element, value.var="Value")
                     })
                     filteredData_treeTable_subS3 <- reactive({
                       dcast(filteredData_treeTable_sub1(),years ~ Element, value.var="Status")
                     })
                     filteredData_treeTable_subS <- reactive({
                       setnames(filteredData_treeTable_subS3(),c("years",paste0(colnames(filteredData_treeTable_subS3())[2:length(colnames(filteredData_treeTable_subS3()))],"_S")))
                     })
                     
                     filteredData_treeTable_subS2 <- reactive({
                       data.table(cbind(filteredData_treeTable_subValue(),filteredData_treeTable_subS()[,2:length(filteredData_treeTable_subS()[1,])]))
                     })
                     
                     filteredData_treeTable_sub <- reactive({
                       filteredData_treeTable_subS2()[,c("years",sort(colnames(filteredData_treeTable_subS2()[,2:length(colnames(filteredData_treeTable_subS2())),with=FALSE]))),with=FALSE]
                     })
                     
                     output[[tableName]] <-
                       renderDataTable({
                         datatable(filteredData_treeTable_sub(),
                                   caption = htmltools::tags$caption(
                                     style = 'caption-side: top; text-align: center; color: black ;',
                                     htmltools::h5(paste0(filteredData_tree()[,unique(commodity)][my_i]," (CPC=",filteredData_tree()[,unique(commodity_Code)][my_i],", Single Balancer = ", filteredData_treePlot_BE(),")" ))
                                   ),options = list(filter = FALSE,searching = FALSE,pageLength = 15,paging = FALSE))
                       })
                   })
                 }
                 
               })
  
  ## code/tree
  named_tree = reactive({
    namedCPC[CPCcode%in%codes_tree()]
  })
  
  output$treeTitle <- renderText({paste0("Tree of ", input$primary)})
  output$tree <- renderDataTable({ datatable(named_tree(),options = list(filter = FALSE,searching = FALSE,pageLength = 15,paging = FALSE)) })
  
  
  ## Bubble plot by Tree
  code_commodityBP = reactive({
    oldSua[commodity==input$commodityCB,unique(commodity_Code)]
  })
  
  
  code_primaryBP = reactive({
    oldSua[commodity==input$primaryCB,unique(commodity_Code)]
  })
  
  codes_treeBP = reactive({
    getChildren(commodityTree = tree,
                parentColname = "measuredItemParentCPC",
                childColname = "measuredItemChildCPC",
                topNodes = code_primaryBP())
  })
  
  colPlot = sample(plotCol,1)
  colPlot2 = sample(plotCol,1)
  
  balTable <- reactive({
    crudeBalEl[measuredItemSuaFbs==code_commodityBP()]
  })
  
  
  output$CommoditySingleBalancer <- renderDataTable({
    datatable(balTable()[,c(1,2,5),with=FALSE],colnames=c("M49","Country","BalancingElement"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; color: black ;',
                htmltools::h4(paste0("Single Balancer Table for commodity ",input$commodityCB, " (CPC=",code_commodityBP(),")"))
              ),filter = "top" ,options = list(paging = FALSE))
  })
  
  
  balTableTree <- reactive({
    crudeBalEl[measuredItemSuaFbs%in%codes_treeBP()]
  })
  
  
  output$TreeSingleBalancer <- renderDataTable({
    datatable(balTableTree(),colnames=c("M49","Country","commodity_Code","commodity","BalancingElement"),
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: center; color: black ;',
                htmltools::h4(paste0("Single Balancer Table for commodity ",input$commodityCB, " (CPC=",code_commodityBP(),")"))
              ),filter = "top" ,options = list(paging = FALSE))
  })
  
  
  
}

shinyApp(ui = ui, server = server)


