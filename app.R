#
# перед запуском приложения необходимо построить граф, для этого зпуситть скрипт ABD.R
#

library(shiny)
library("visNetwork")
library(timevis)
library(igraph)

library(shinydashboard)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)




data_timeline <- read.csv('timeline.csv', sep = ';')
row.names(data_timeline) <- c(1:nrow(data_timeline))



data <- data.frame(
  id      = 1:7, #row.names(data_timeline), #data_timeline$порядковый_номер_пути,
  content = data_timeline$вершина,
  start   = data_timeline$дата_откр_счета,
  end     = data_timeline$дата_реорг
  # style       = c ("color: red;", "color: blue;", "color: red;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;"),
  # className   = c ("red_point", "blue_point", "red_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point")
)


# data <- data.frame(
#   id      = 1:4,
#   content = c("Item one", "Item two",
#               "Ranged item", "Item four"),
#   start   = c("2016-01-10", "2016-01-11",
#               "2016-01-20", "2016-02-14 15:00:00"),
#   end     = c("2016-01-25", "2016-01-21", "2016-02-04", NA)
# )

bank_df<- read.csv('Банк_Открытие_реорг.csv', sep = ";", stringsAsFactors=FALSE)



# строим граф

actors <- data.frame(name=unique(c(bank_df$Организация.предок)))



relations <- data.frame(from=bank_df$Организация.предок[c(1:nrow(bank_df)-1)],
                        to=bank_df$Организация.потомок[c(1:nrow(bank_df)-1)])

g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)





E(g)$name <- bank_df$Дата.преобразования [c(1:nrow(bank_df)-1)]

requested_node<- "ПАО ХАНТЫ-МАНСИЙСКИЙ БАНК ОТКРЫТИЕ ИНН: 8601000666 ИД: 16846 Рег.: 1971"
# вершины, из которых можно попасть в искомую
in_list<-subcomponent (g, requested_node, mode = "in")


data_gr <- toVisNetworkData(g)
data_sub_gr <- toVisNetworkData(induced.subgraph(graph=g,vids=in_list))


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),

    # menuItem("Widgets", icon = icon("th"), tabName = "widgets",
    #          badgeLabel = "new", badgeColor = "green"),
    
    selectInput(inputId = "tax_payer",
                label = "выбрать НП",
                choices = as.character(bank_df[,1]),
                selected = bank_df[2,1]),
    
    dateRangeInput("dates", label = h3("Диапазон  дат"))
  )
)


# Define UI for app that draws a histogram ----
ui <- #fluidPage(
  
  dashboardPage(
  dashboardHeader(title = "АБД 2.0"),
  sidebar,
  dashboardBody(
    
 
    
  
  navbarPage("Анализ банковских документов",
             tabPanel("реорганизация банка 'Открытие'",
                      
                      # App title ----
                    #  titlePanel("все данные"),
                      
                    
                      #mainPanel(
                    h4("На основе исходных данных"),
                  
                        
                        
                        hr(),
                        #fluidRow(column(4, verbatimTextOutput("value"))),
                        
                      #  titlePanel("Реорганизация компаний"),
                        
                        
                        
                        fluidRow(
                          column(6,
                                 
                                 
                                 box(
                                   title = "Полная схема реорганизации Банка 'Открытие' ", status = "primary", width = 12, solidHeader = TRUE,
                                   #h4("Полная схема реорганизации"),
                                   collapsible = TRUE,
                                 visNetwork(#main="Полная схема реорганизации", submain="Все, связанные с реорганизацией НП ", 
                                            nodes = data_gr$nodes, edges = data_gr$edges, width="100%", height="400px", 
                                            #background="#eeefff"
                                            )%>% 
                                   visEdges(arrows = "to")%>%
                                   visIgraphLayout(),
                                 )),
                                 
                         # ),
                          
                         column(6, 
                                 
                                 box(
                                   title = "Выбранный НП и предшествующие реорганизации", status = "warning",  width = 12, solidHeader = TRUE,
                                 #h4("Выбранный НП и предшествующие реорганизации"),
                                 collapsible = TRUE,
                                 visNetworkOutput("network"),
                                 
                                 # visNetwork(main="Выбранный НП и предшестующие реорганизации", nodes = data_sub_gr$nodes, edges = data_sub_gr$edges, width="100%", height="400px", background="#C4FCEF")%>% 
                                 #   visEdges(arrows = "to"),
                                 
                                 ),
                                 
                         
                          
                        ),
                        
                        
                        
                        br(),
                        br(),
                        hr(),
                        tags$head(
                          tags$style(HTML("
                        .red_point  { border-color: red; background-color: coral;   }
                        .blue_point { border-color: blue; background-image: linear-gradient(to right top, #d16ba5, #ce6fb2, #c973be, #c178cb, #b87ed7, #a28ee9, #899df7, #6cabff, #37c3ff, #00d8ff, #19ebff, #5ffbf1); }
                        "))),
                        
                        
                        # кастомизация сообщения об ошибке
                        
                        tags$head(
                          tags$style(HTML(" .shiny-output-error-validation {
                            color: red; font-size: 24px;
                                 } "))
                        ),
                        
                          # visNetwork(main="Выбранный НП и предшестующие реорганизации", nodes = data_sub_gr$nodes, edges = data_sub_gr$edges, width="100%", height="400px", background="#C4FCEF")%>% 
                          #   visEdges(arrows = "to"),
                          
                        ),
                        
                        
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    br(),
                    br(),
                   
                    
                    h4 ("Перечень всех предшествующих реорганизаций с указанием дат для отправки запроса по оборотам в БД по каждому интересующему НП"),
                    
                    #      DT::dataTableOutput("all_paths"),
                    dataTableOutput("all_paths"),
                    #  DT::dataTableOutput("table2"),
                    DT::dataTableOutput("table3"),
                    
                    box(
                      title = "Выбранный НП и предшествующие реорганизации",  status = "warning",  width = 12, solidHeader = TRUE, 
                      collapsible = TRUE,
                      #h4("Выбранный НП и предшествующие реорганизации"),
                      
                      
                      
                      timevisOutput("timeline",  width = "100%", height = "auto"),
                      
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    

                #DT::dataTableOutput("table4"),
                      )
                ),
                 
             tabPanel("исходные данные по банку Открытие",    
                      DT::dataTableOutput("table")
                      ),
             
             tabPanel("данные из боевой БД", 
                      
                      selectInput(inputId = "all_tax_payer",
                                  label = "выбрать НП",
                                  choices = vertex_length,
                                  selected = '26097804'),
                      
                      visNetworkOutput("network_all")
                      
                      #DT::dataTableOutput("table")
                      )
             
             
  ),
                      
                     
             
             
  
            
  ),
  
  
  
  tags$style(
    ".redBg { background: red; }
      .blueBg { background: blue; }
      .greenBg { background: green; }
      .orangeBg { background: orange; }"
  ),
  
  #  )
)

#)




# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  


  

  otkr_reorganisation<- read.csv('Банк_Открытие_реорг.csv', sep =';')
  
  
  
  output$table <-  DT::renderDataTable({ otkr_reorganisation})
  
  
  # рендеринг графа
  output$network <- renderVisNetwork({
  
  #строим подграф на основе выбранной вершины
  data_sub_gr <- toVisNetworkData(induced.subgraph(graph=g,vids=subcomponent (g, input$tax_payer, mode = "in")))
  
  visNetwork(#main="Выбранный НП и предшестующие реорганизации", 
             nodes = data_sub_gr$nodes, edges = data_sub_gr$edges, width="100%", height="400px"
             #, background="#C4FCEF"
             ) %>% 
    visEdges(arrows = "to") 
  })
  
 
  
  # для полного графа
  output$network_all <- renderVisNetwork({
    
    #строим подграф на основе выбранной вершины
    data_sub_gr_all <- toVisNetworkData(induced.subgraph(graph=graph_all,vids=subcomponent (graph_all, input$all_tax_payer, mode = "in")))
    
    visNetwork(#main="Выбранный НП и предшестующие реорганизации", 
      nodes = data_sub_gr_all$nodes, edges = data_sub_gr_all$edges, width="100%", height="400px"
      #, background="#C4FCEF"
    ) %>% 
      visEdges(arrows = "to") 
  }) 
  
  
  
  
  
  
  
  
    
    output$all_paths <- reactive({
    
 #     for (i in (1:length(in_list))){
      
     # paste(all_simple_paths(g, from = 'ОАО БАНК ОТКРЫТИЕ ИНН: 7744003399 ИД: 16957 Рег.: 2179', to =input$tax_payer))
    
     
      
      # вершины, из которых можно попасть в искомую (для банка Открытие)
      in_list<-subcomponent (g, input$tax_payer, mode = "in")
      
      in_list_all<-subcomponent (graph_all, input$all_tax_payer, mode = "in")
      
      
      all_paths<- list()
      
      for (i in (1:length(in_list))){
        
        all_paths<-append( all_paths, all_simple_paths(g, from = in_list[i], to = input$tax_payer))
        
        
      }
      
     #paste(all_paths)
      
   
      
     
      
      validate(

        need( try(length(all_paths) != 0), 'у выбранного НП нет "родительских" компаний ')
      )
      
      
      
      
      
      
      
      
      
      
      
      
      all_unique_paths<- list()
      
      for (i in (1:length(all_paths))){
        for (j in (1:length(all_paths))){
          
          all_unique_paths<- append(all_unique_paths, ifelse(all(all_paths[[i]]  %in% all_paths[[j]] == TRUE), "", list(names(as.list(all_paths[[i]] )))))
          
        }
      }
      
      
      
      
      # формируем табл попарных сравнений входит ли путь в графе в какой-либо другой путь полностью
      # если входит, то его нужно игнорировать, чтобы получить только список всех уникальных путей
      all_unique_paths_df <- data.frame(matrix(ncol = length(all_paths), nrow = length(all_paths)))
      
      path_nodes <- list()
      
      for (i in (1:length(all_paths))){
        path_nodes <- append (path_nodes, list(all_paths[[i]]$name))
      }
      
      colnames( all_unique_paths_df) <- path_nodes
      rownames( all_unique_paths_df) <- path_nodes
      
      for (i in (1:length(all_paths))){
        for (j in (1:length(all_paths))){
          
          all_unique_paths_df [i,j]<- ifelse(all(all_paths[[i]]  %in% all_paths[[j]] == TRUE), 1, 0)
          
        }
      }
      
      # вводим результирующую колонку для подсчета ситуаций, когда путь входить куда-либо еще кроме как в самого себя
      # если входит, то он не будет участовать в дальнейших расчетах
      all_unique_paths_df$col_sum<-rowSums(all_unique_paths_df)
      
      # получили уникальные пути
      all_unique_paths <- all_paths [all_unique_paths_df$col_sum<2]
      
      
      output$table2<-  DT::renderDataTable({ all_unique_paths_df})
      
      
      all_nodes <- data.frame()
      for (i in (1:length(all_unique_paths))){
        
        all_nodes<-rbind.data.frame(all_nodes, cbind(as.data.frame(all_unique_paths[[i]]$name, stringsAsFactors=F), c(rep(i,length(all_unique_paths[[i]]$name)))))
        
      }
      
      colnames(all_nodes)<- (c("вершина", "порядковый номер пути"))
      
      #all_nodes<- ifelse (length(all_paths)==0, df, all_nodes)
      
      # df <- data.frame(id = numeric(0), jobs = numeric(0))
      # newrow <- data.frame(id=3, jobs=4)
      # df <- rbind(df, newrow)
      # 
      # all_nodes<- ifelse (nrow(all_nodes)==0, df, all_nodes)
      
      #output$table3<-  DT::renderDataTable({  all_nodes})
      
      
      output$table3 <-  if(is.null(dim(all_nodes))){
         DT::renderDataTable({df})
      }
      else{
        DT::renderDataTable(  all_nodes , filter = 'top')
      }
      
      
      
      
      
      
      
   
     
      
      
      
      
      
      
      
      
      
      
      all_nodes$дата_реорганизации <-NA
      
      for (i in (2:nrow(all_nodes))){
        all_nodes$дата_реорганизации [i] <- ifelse(all_nodes$`порядковый номер пути`[i] ==all_nodes$`порядковый номер пути`[i-1],
                                                   E(g)$name [get.edge.ids(g, c(all_nodes[i-1,1], all_nodes[i,1]))] ,
                                                   NA)
        
        
      }
      
      all_nodes$дата_реорганизации<- c(all_nodes$дата_реорганизации [2:nrow(all_nodes)], NA)
      
      library(lubridate)
      
      all_nodes$дата_реорганизации <- dmy (all_nodes$дата_реорганизации)
      
      # генерируем случайные даты открытия счета
      
      all_nodes$дата_откр_счета <- as.Date(all_nodes$дата_реорганизации, format = c("%d.%m.%Y"))- sample(seq(1:1500), nrow(all_nodes))
      
      all_nodes<- all_nodes [c(1,2,4,3)]
      
      
      # присваиваем дату открытия, там, где ее нет
      all_nodes$дата_откр_счета[is.na(all_nodes$дата_откр_счета)]<- min(all_nodes$дата_реорганизации,na.rm =TRUE)
      
      all_nodes$дата_реорганизации[is.na(all_nodes$дата_реорганизации)]<- Sys.Date()
      
      # определяем для каждого пути (колонка "порядковый номер пути") какие результаты по каким компаниям нужно вывести на UI
      
      start_date <- "2009.09.10"
      end_date <- "2013.09.20"
      
      
      
      all_nodes$показывать_UI <-NA
      
      for (i in (1:nrow(all_nodes))){
        
        all_nodes$показывать_UI [i] <- ifelse(all_nodes$`порядковый номер пути`[i] ==all_nodes$`порядковый номер пути`[i-1] &&
                                                as.Date(start_date, tryFormats = c("%Y.%m.%d")) < as.Date(all_nodes$дата_реорганизации [i], tryFormats = c("%Y.%m.%d")) ,
                                              1,
                                              NA)
        
      }
      
      
      
      
      all_nodes$дата_нач_диапазона <-NA
      all_nodes$дата_конец_диапазона <-NA
      
      all_nodes$дата_нач_диапазона <- as.Date( all_nodes$дата_нач_диапазона , "%m/%d/%y")
      all_nodes$дата_конец_диапазона <- as.Date( all_nodes$дата_конец_диапазона , "%m/%d/%y")
      
      library (anytime)
      
      for (i in (2:nrow(all_nodes))) {
        
        all_nodes$дата_нач_диапазона [i] <- anydate(ifelse (all_nodes$`порядковый номер пути` [i] == all_nodes$`порядковый номер пути`[i-1] &&
                                                              as.Date(start_date, format = c("%Y.%m.%d")) < (as.Date(all_nodes$дата_реорганизации [i], format = c("%Y.%m.%d")) || all_nodes$дата_откр_счета [i]),
                                                            as.Date(start_date, format = c("%Y.%m.%d")),
                                                            min(c(all_nodes$дата_реорганизации [i],  all_nodes$дата_откр_счета [i])) ))
        
        
        all_nodes$дата_конец_диапазона [i] <- anydate(ifelse (all_nodes$`порядковый номер пути` [i] == all_nodes$`порядковый номер пути`[i-1] &&
                                                                as.Date(end_date, format = c("%Y.%m.%d")) > as.Date(all_nodes$дата_реорганизации [i], format = c("%Y.%m.%d")),
                                                              all_nodes$дата_реорганизации [i],
                                                              as.Date(end_date, format = c("%Y.%m.%d"))))
        
      
        
      }
      
     
      
      
      output$table4<-  DT::renderDataTable(all_nodes, filter = 'top')
      
      all_nodes_unique<- all_nodes[!duplicated(all_nodes$вершина),]
      
      data <- data.frame(
        id      = 1:nrow(all_nodes_unique), #row.names(data_timeline), #data_timeline$порядковый_номер_пути,
        content = all_nodes_unique$вершина,
        start   = all_nodes_unique$дата_откр_счета,
        end     = all_nodes_unique$дата_реорг
        # style       = c ("color: red;", "color: blue;", "color: red;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;", "color: blue;"),
        # className   = c ("red_point", "blue_point", "red_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point", "blue_point")
      )
      
      
      
      output$timeline <- renderTimevis({
        timevis(data)
      })
      
#      }
      
     
    
    })
  
 
  
  
    
    
    
    
  
  
  
  
  
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



