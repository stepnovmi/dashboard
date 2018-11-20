# Подгрузка библиотек. Закомменчены библиотеки с гитхаба

library(shiny)
library(shinyWidgets)
library(plyr)
library(shinythemes)
library(shinydashboard)
library(extrafont)
 #install.packages("devtools")
#devtools::install_github("dreamRs/shinyWidgets",force = TRUE)
####################### Начало UI-файла #######################
shinyUI(dashboardPage(dashboardHeader(
  
  
  title = "Social Media Dashboard",
                                      titleWidth = 250

   
  
  
  ),
                            dashboardSidebar( width = 250,sidebarMenu(
                     
                              
                      
                       tags$br(),
 ####################### UI-часть группового фильтра базы + оформление через CSS #######################                      
                       selectizeGroupUI(
                         id = "my-filters",
                         btn_label = tags$b(tags$span(style="color:#f2f5f5", "Сбросить фильтры")),
                         inline = FALSE,
                         params = list(
                           Sex = list(inputId = "Sex", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Пол:")),
                           Age_Range = list(inputId = "Age_Range", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Возраст:")),
                           CitySize = list(inputId = "CitySize", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Размер города:")),
                           FedDist = list(inputId = "FedDist", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Федеральный округ:")),
                           Family_Status = list(inputId = "Family_Status", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Семейное положение:")),
                           Children = list(inputId = "Children", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Наличие детей:")),
                           Education = list(inputId = "Education", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Образование:")),
                           JobStatus = list(inputId = "JobStatus", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Работа:")),
                           JobSphere = list(inputId = "JobSphere", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Сфера работы:")),
                           Main_SN_profile = list(inputId = "Main_SN_profile", title = tags$span(style="font-family: 'Calibri'; font-weight:normal","Площадка:"))
                         )
                       )
                       
                       
                     )),
                     dashboardBody(
                       tags$head(
                        ####################### Элемент CSS-таблицы для настройки цветовой гаммы #######################
                         tags$style(HTML(".main-header .logo{background-color: #C23C33 !important;}
                                          .main-header .navbar{background-color: #51778F !important;}
                                          .main-sidebar {background-color: grey !important;}
                                          .content-wrapper {background-color: #f2f5f5 !important;}
                                          .bg-aqua {background-color: #7792A6 !important; }
                                          .tabbable > .nav > li[class=active]    > a {background-color: #C23C33; color:white; border-top-color: #C23C33}
                                          .tabbable > .nav > li    > a {background-color: #7792A6; color:white; border-top-color: #7792A6}
.btn.disabled {
    background-color: red !important;}
                                         }
                                         "))
                         ),
                         
                       splitLayout(
                         ####################### Отображение текущей выборки и кнопка выгрузки в Excel #######################
                           infoBoxOutput("chosen2", width = 8),
                           div(downloadButton('DD1', 'Download to Excel'), style = "float:right")
                           
                       
                       ),
                       
                       tabsetPanel(type = "pills",
                       ####################### Вкладки с графиками #######################
                       tabPanel("Общая информация",
                                wellPanel(style = "border: 1px solid silver;font-family: 'Calibri'; background-color: #ffffff",
                                                                "Размер выборки составляет ", tags$b("1677"), " пользователей, проживающих в России в возрасте от ", tags$b("13 до 64 лет."),
                                tags$br(),
                                tags$br(),
                                "Количество проанализированных аккаунтов - ",
                                tags$b("5492"),
                                tags$br(),
                                "Vkontakte - ", tags$b("1661"),
                                tags$br(),
                                "Instagram - ",tags$b("1612"),
                                tags$br(),
                                "Odnoklassniki - ",tags$b("1038"),
                                tags$br(),
                                "Facebook - ",tags$b("1181"),
                                tags$br(),
                                tags$br(),
                                "Обратите внимание, что ", tags$b("данные отображают информацию, предоставленную пользователеями на их страничках в социальных сетях."), 
                                tags$br(),
                                "Такая информация может быть искажена пользователями.")),
                       tabPanel("Демография",style = "border: 1px solid silver;font-family: 'Calibri';;background-color: #ffffff",
                                
                                splitLayout(style = "border: 0px",
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("Age_Range2"), spin = "circle",color = "#7792A6")),
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("Sex2"), spin = "circle",color = "#7792A6"))
                                ),
                                splitLayout(style = "border: 0px",
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("FedDist2"), spin = "circle",color = "#7792A6")),
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("CitySize2"), spin = "circle",color = "#7792A6"))
                                ),
                                splitLayout(style = "border: 0px",
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("Family_Status2"), spin = "circle",color = "#7792A6")),
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("Children2"), spin = "circle",color = "#7792A6"))
                                ),
                                
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("JobStatus2"), spin = "circle",color = "#7792A6"))
                                  
                                
                       ),
                       tabPanel("Интересы",style = "border: 1px solid silver;font-family: 'Calibri';background-color: #ffffff",
                                
                                splitLayout(style = "border: 0px",
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("health_level2"), spin = "circle",color = "#7792A6")),
                                  wellPanel(style = "border: 0px; background-color: #ffffff",
                                            addSpinner(plotOutput("Life_attitude2"), spin = "circle",color = "#7792A6"))
                                  
                                ),
                                verticalLayout(wellPanel(style = "border: 0px; background-color: #ffffff",
                                                         addSpinner(plotOutput("interest_sphere2",height = "1000px"), spin = "circle",color = "#7792A6"))
                                ),
                                verticalLayout(wellPanel(style = "border: 0px; background-color: #ffffff",
                                                         addSpinner(plotOutput("brands_used2",height = "500px"), spin = "circle",color = "#7792A6"))
                                ),
                                verticalLayout(wellPanel(style = "border: 0px; background-color: #ffffff",
                                                         addSpinner(plotOutput("places2",height = "500px"), spin = "circle",color = "#7792A6"))
                                )
                       ))
                      
                     )
                     
                     
                     
                   ))
                #   hr(),
                  # 
                   
                   
