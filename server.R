############### Подгрузка библиотек. Закомменчены библиотеки с гитхаба ###############

library(shiny)

library(data.table)

library(ggplot2)

library(dplyr)

library(plyr)

library(ggthemr)

library(ggthemes)

library(xlsx)

library(r2excel)

library(shinydashboard)



#detach("package:XLConnect", unload=TRUE)
#devtools::install_github("kassambara/r2excel")
#devtools::install_github('cttobin/ggthemr')
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Применение фильтра - Server-часть
  
  theme_few(base_size = 12, base_family = "")
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = df,
    vars = c("Education","Sex", "Age_Range", "CitySize","FedDist","Family_Status","Children","JobStatus","JobSphere","Main_SN_profile")
  )
  


  ############### Замер текущего размера выборки ###############
  output$chosen2 <- renderInfoBox({
    a = "Текущая выборка"
    b = nrow(res_mod())
    c = "users"
    d = "aqua"
    if (nrow(res_mod()) < 20){
      a = "Нерепрезентативная выборка!"
      b = nrow(res_mod())
      c = "exclamation-circle"
      d = "red"
    }
    
    infoBox(
      a, b, icon = icon(c),color = d)
  })
  
 
  
  ############### Построение графиков ###############
  
  output$Sex2 = renderPlot({
    
    ggplot(data=psx()) + ggtitle("Пол") +
      geom_bar(aes(x="", y=psx()$per, fill=psx()$Var1), stat="identity", width = 1)+
      coord_polar("y", start=0)+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),legend.title=element_blank())+
      theme(legend.text = element_text(size = 15),legend.title = element_blank())+
      geom_text(aes(x=1, y = cumsum(psx()$per) - psx()$per/2, label=psx()$label),size = 5)+scale_fill_manual(values = my.cols)
      
    },bg = "#ffffff"
      )
  
  output$JobStatus2 = renderPlot({
    
    
    ggplot(data=pjs()) + ggtitle("Занятость") +
      geom_bar(aes(x="", y=pjs()$per, fill=pjs()$Var1), stat="identity", width = 0.9)+
      coord_polar("y", start=0)+
      theme_void()+
      theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),legend.title=element_blank()
      )+
      theme(legend.text = element_text(size = 15),legend.title = element_blank())+theme(plot.background = element_rect(fill = '#ffffff', colour = '#ffffff'))+
      geom_text(aes(x=1, y = cumsum(pjs()$per) - pjs()$per/2, label=pjs()$label),size = 5)+scale_fill_manual(values = my.cols)
    },bg = "#ffffff")
  
  output$Age_Range2 = renderPlot({
    
    ggplot(data=par()) + ggtitle("Возраст") +
      geom_bar(aes(reorder(par()$Var1, par()$per), par()$per,fill=par()$Var1), stat="identity", width = 0.9)+
      #coord_polar("y", start=0)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust=0.5,size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=par()$Var1, y = par()$per+0.02, label=par()$label),size = 5)+ coord_flip()+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$FedDist2 = renderPlot({
   
    ggplot(data=pfd()) + ggtitle("Федеральный округ") +
      geom_bar(aes(reorder(pfd()$Var1, pfd()$per), pfd()$per,fill=pfd()$Var1), stat="identity", width = 0.9)+
      #coord_polar("y", start=0)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust=0.5,size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pfd()$Var1, y = pfd()$per+0.02, label=pfd()$label),size = 5)+ coord_flip()+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$CitySize2 = renderPlot({
   
    ggplot(data=pcs()) + ggtitle("Размер города") +
      geom_bar(aes(reorder(pcs()$Var1, pcs()$per), pcs()$per,fill=pcs()$Var1), stat="identity", width = 0.9)+
      #coord_polar("y", start=0)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(hjust=0.5,size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pcs()$Var1, y = pcs()$per+0.02, label=pcs()$label),size = 5)+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$Family_Status2 = renderPlot({
    
    ggplot(data=pfs()) + ggtitle("Семейное положение") +
      geom_bar(aes(reorder(pfs()$Var1, pfs()$per), pfs()$per,fill=pfs()$Var1), stat="identity", width = 0.9)+
      #coord_polar("y", start=0)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.08,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust=0.5,size = 15),
        panel.grid.major = element_blank(),
       # axis.text.x = element_text(hjust=0.5,size = 12),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pfs()$Var1, y = pfs()$per+0.03, label=pfs()$label),size = 5)+ coord_flip()+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$Children2 = renderPlot({
    
    ggplot(data=pch()) + ggtitle("Наличие детей") +
      geom_bar(aes(reorder(pch()$Var1, pch()$per), pch()$per,fill=pch()$Var1), stat="identity", width = 0.9)+
      #coord_polar("y", start=0)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(hjust=0.5,size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pch()$Var1, y = pch()$per+0.02, label=pch()$label), size = 5)+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$health_level2 = renderPlot({
    
    ggplot(data=php()) + ggtitle("Значение здоровья") +
      geom_bar(aes(x="", y=php()$per, fill=php()$Var1), stat="identity", width = 1)+
      coord_polar("y", start=0)+
      theme_void()+
      
      theme(plot.title = element_text(hjust = 0.65,size = 20, face = "bold"))+
      theme(legend.text = element_text(size = 15),legend.title = element_blank())+
      geom_text(aes(x=1, y = cumsum(php()$per) - php()$per/2, label=php()$label), size = 5)+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$interest_sphere2 = renderPlot({
    
    ggplot(data=pinterests()) + ggtitle("Сфера интересов") +
      geom_bar(aes(reorder(pinterests()$df2, pinterests()$per), pinterests()$per,fill=pinterests()$df2), stat="identity", width = 0.9)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.35,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        
        axis.text.y = element_text(hjust=1,size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pinterests()$df2, y = pinterests()$per+0.015, label=pinterests()$label),size = 5)+ coord_flip()+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$Life_attitude2 = renderPlot({
    
    ggplot(data=pla()) + ggtitle("Жизненные ценности") +
      geom_bar(aes(reorder(pla()$Var1, pla()$per), pla()$per,fill=pla()$Var1), stat="identity", width = 0.9)+
      #coord_polar("y", start=0)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.05,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y = element_text(hjust=1,size = 15),
        axis.text.x = element_blank(),
                panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pla()$Var1, y = pla()$per+0.02, label=pla()$label),size = 5)+ coord_flip()+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$brands_used2 = renderPlot({
    
    ggplot(data=pbrands()) + ggtitle("Категории упоминаемых брендов") +
      geom_bar(aes(reorder(pbrands()$df3, pbrands()$per), pbrands()$per,fill=pbrands()$df3), stat="identity", width = 0.8)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.35,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust=1,size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pbrands()$df3, y = pbrands()$per+0.0022, label=pbrands()$label),size = 5)+ coord_flip()+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  output$places2 = renderPlot({
   
    ggplot(data=pplaces()) + ggtitle("Места посещений") +
      geom_bar(aes(reorder(pplaces()$df4, pplaces()$per), pplaces()$per,fill=pplaces()$df4), stat="identity", width = 0.8)+
      theme_minimal()+
      theme(
        plot.title = element_text(hjust = 0.25,size = 20, face = "bold"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust=1,size = 15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
      )+
      geom_text(aes(x=pplaces()$df4, y = pplaces()$per+0.02, label=pplaces()$label),size = 5)+ coord_flip()+scale_fill_manual(values = my.cols)},bg = "#ffffff")
  
  ############### Реактивные функции для сохранения выборки для вывода графиков и выгрузки ###############
  
  psx <- reactive({
    sx = as.data.frame(table(res_mod()$Sex)/nrow(res_mod()))
    sx <- sx %>% 
      mutate(per=`Freq`/sum(`Freq`)) %>% 
      arrange(desc(Var1))
    sx$label <- scales::percent(sx$per,accuracy=1)
    sx = subset(sx, Freq>0)
    sx = subset(sx, label != "0%")
    
  })
 ?scale_fill_brewer()
  pjs <- reactive({
    js = as.data.frame(table(res_mod()$JobStatus)/nrow(res_mod()))
    js <- js %>% 
      mutate(per=`Freq`/sum(`Freq`)) %>% 
      arrange(desc(Var1))
    js$label <- scales::percent(js$per,accuracy=1)
    js = subset(js, Freq>0)
    js = subset(js, label != "0%")
  })
  
  pch <- reactive({
  ch = as.data.frame(table(res_mod()$Children)/nrow(res_mod()))
  ch<- ch %>% 
    mutate(per=`Freq`/sum(`Freq`)) %>% 
    arrange(desc(Var1))
  ch$label <- scales::percent(ch$per,accuracy=1)
  ch = subset(ch, Freq>0)
  ch = subset(ch, label != "0%")
  })
  
  pfs <- reactive({
  fs = as.data.frame(table(res_mod()$Family_Status)/nrow(res_mod()))
  fs<- fs %>% 
    mutate(per=`Freq`/sum(`Freq`)) %>% 
    arrange(desc(Var1))
  fs$label <- scales::percent(fs$per,accuracy=1)
  fs = subset(fs, Freq>0)
  fs = subset(fs, label != "0%")
  })
  
  pcs <- reactive({
  cs = as.data.frame(table(res_mod()$CitySize)/nrow(res_mod()))
  cs<- cs %>% 
    mutate(per=`Freq`/sum(`Freq`)) %>% 
    arrange(desc(Var1))
  cs$label <- scales::percent(cs$per,accuracy=1)
  cs = subset(cs, Freq>0)
  cs = subset(cs, label != "0%")
  })
  
  pfd <- reactive({
  fd = as.data.frame(table(res_mod()$FedDist)/nrow(res_mod()))
  fd <- fd %>% 
    mutate(per=`Freq`/sum(`Freq`)) %>% 
    arrange(desc(Var1))
  fd$label <- scales::percent(fd$per,accuracy=1)
  fd = subset(fd, Freq>0)
  fd = subset(fd, label != "0%")
  })
  
  par <- reactive({
  ar = as.data.frame(table(res_mod()$Age_Range)/nrow(res_mod()))
  ar <- ar %>% 
    mutate(per=`Freq`/sum(`Freq`)) %>% 
    arrange(desc(Var1))
  ar$label <- scales::percent(ar$per,accuracy=1)
  ar = subset(ar, Freq>0)
  ar = subset(ar, label != "0%")
})
  
  
  
  pplaces <- reactive({
    places=data.frame(place = character(),
                      label = integer(), stringsAsFactors = FALSE)
    for (i in 203:231){
      df4 = df[i]#res_mod()
      df4 = as.data.frame(table(df4), stringsAsFactors = FALSE)
      df4 <- df4 %>% 
        mutate(per=`Freq`/sum(`Freq`)) %>% 
        arrange(desc(`df4`))
      
      df4$df4[df4$df4 == 1] = names(df[i])
      df4 = subset(df4, df4 != "0")
      places=rbind(places,df4[1,c(1,3)])
    }
    places$df4 = sub("bin_places_","", places$df4)
    places$df4 = gsub("."," ", places$df4,fixed=TRUE)
    places = places %>% arrange(desc(per))
    places$label <- scales::percent(places$per,accuracy=1)
    places = subset(places, df4 != "0")
    places = subset(places, df4 != "н д")
    places = subset(places, df4 != "Армия")
    places = subset(places, label != "0%")
  })
  
  pbrands <- reactive({
    brands=data.frame(brand = character(),
                      label = integer(), stringsAsFactors = FALSE)
    for (i in 75:92){
      df3 = res_mod()[i]
      df3 = as.data.frame(table(df3), stringsAsFactors = FALSE)
      df3 <- df3 %>% 
        mutate(per=`Freq`/sum(`Freq`)) %>% 
        arrange(desc(`df3`))
      
      df3$df3[df3$df3 == 1] = names(df[i])
      df3 = subset(df3, df3 != "0")
      brands=rbind(brands,df3[1,c(1,3)])
    }
    brands$df3 = sub("bin_int_","", brands$df3)
    brands$df3 = gsub("."," ", brands$df3,fixed=TRUE)
    brands = brands %>% arrange(desc(per))
    brands$label <- scales::percent(brands$per,accuracy=1)
    brands = subset(brands, df3 != "0")
    brands = subset(brands, label != "0%")
  })
  
  pla <- reactive({
    la = as.data.frame(table(res_mod()$Life_attitude)/nrow(res_mod()))
    la <- la %>% 
      mutate(per=`Freq`/sum(`Freq`)) %>% 
      arrange(desc(Var1))
    la$label <- scales::percent(la$per,accuracy=1)
    la = subset(la, Freq>0)
    la = subset(la, label != "0%")
  })
  
  pinterests <- reactive({
    interests=data.frame(interest = character(),
                         label = integer(), stringsAsFactors = FALSE)
    for (i in 124:202){
      df2 = res_mod()[i]
      df2 = as.data.frame(table(df2), stringsAsFactors = FALSE)
      df2 <- df2 %>% 
        mutate(per=`Freq`/sum(`Freq`)) %>% 
        arrange(desc(`df2`))
      
      df2$df2[df2$df2 == 1] = names(df[i])
      df2 = subset(df2, df2 != "0")
      interests=rbind(interests,df2[1,c(1,3)])
      
    }
    interests$df2 = sub("bin_int_","", interests$df2)
    interests$df2 = gsub("."," ", interests$df2,fixed=TRUE)
    interests = interests %>% arrange(desc(per))
    interests$label <- scales::percent(interests$per,accuracy=1)
    interests = subset(interests, df2 != "0")
    interests = subset(interests, label != "0%")
  })
  
  php <- reactive({
    hp = as.data.frame(table(res_mod()$health_level)/nrow(res_mod()))
    hp <- hp %>% 
      mutate(per=`Freq`/sum(`Freq`)) %>% 
      arrange(desc(Var1))
    hp$label <- scales::percent(hp$per,accuracy=1)
    hp = subset(hp, Freq>0)
    hp = subset(hp, label != "0%")
  })
  
  ############### Чистим файлы для вставки в выгрузку ###############
  
  esx <- reactive ({
    esx = psx()
    esx = esx [c(1,4)]
    names(esx)=c("Пол", "Доля, %")
    esx = as.data.frame(esx)
  })
  
  ejs <- reactive ({
    ejs = pjs()
    ejs = ejs [c(1,4)]
    names(ejs)=c("Занятость", "Доля, %")
    ejs = as.data.frame(ejs)
  })
  
  ech <- reactive ({
    ech = pch()
    ech = ech [c(1,4)]
    names(ech)=c("Наличие детей", "Доля, %")
    ech = as.data.frame(ech)
  })
  
  efs <- reactive ({
    efs = pfs()
    efs = efs [c(1,4)]
    names(efs)=c("Семейное положение", "Доля, %")
    efs = as.data.frame(efs)
  })
  
  ecs <- reactive ({
    ecs = pcs()
    ecs = ecs [c(1,4)]
    names(ecs)=c("Размер Города", "Доля, %")
    ecs = as.data.frame(ecs)
  })
  
  efd <- reactive ({
    efd = pfd()
    efd = efd [c(1,4)]
    names(efd)=c("Федеральный округ", "Доля, %")
    efd = as.data.frame(efd)
  })
  
  ear <- reactive ({
    ear = par()
    ear = ear [c(1,4)]
    names(ear)=c("Возраст", "Доля, %")
    ear = as.data.frame(ear)
  })
  
  
  
  eplaces <- reactive ({
    eplaces = pplaces()
    eplaces = eplaces [c(1,3)]
    names(eplaces)=c("Места посещений", "Доля, %")
    eplaces = as.data.frame(eplaces)
  })
  
  ebrands <- reactive ({
    ebrands = pbrands()
    ebrands = ebrands [c(1,3)]
    names(ebrands)=c("Упоминаемые бренды", "Доля, %")
    ebrands = as.data.frame(ebrands)
  })
  
  ela <- reactive ({
    ela = pla()
    ela = ela [c(1,4)]
    names(ela)=c("Жизненные ценности", "Доля, %")
    ela = as.data.frame(ela)
  })
  
  einterests <- reactive ({
    einterests = pinterests()
    einterests = einterests [c(1,3)]
    names(einterests)=c("Сфера интересов", "Доля, %")
    einterests = as.data.frame(einterests)
  })
  
  ehp <- reactive ({
    ehp = php()
    ehp = ehp [c(1,4)]
    names(ehp)=c("Значение здоровья", "Доля, %")
    ehp = as.data.frame(ehp)
  })
  ############### Выгрузка в Эксель ###############
  
  output$DD1 <- downloadHandler(
    filename = function() { "SMData.xlsx" },
    content = function(file) {
      wb <- createWorkbook(type="xlsx")
      sheet <- createSheet(wb, sheetName = "Демография")
      
      xlsx.addTable(wb, sheet, ear(), startCol=2, startRow = 2,row.names = F)
      xlsx.addTable(wb, sheet, esx(), startCol=5, startRow = 2,row.names = F)
      
      xlsx.addTable(wb, sheet, efd(), startCol=2, startRow = 11,row.names = F)
      xlsx.addTable(wb, sheet, ecs(), startCol=5, startRow = 11,row.names = F)
      
      xlsx.addTable(wb, sheet, ech(), startCol=2, startRow = 23,row.names = F)
      xlsx.addTable(wb, sheet, efs(), startCol=5, startRow = 23,row.names = F)
      
      xlsx.addTable(wb, sheet, ejs(), startCol=2, startRow = 32,row.names = F)
      
      
      sheet <- createSheet(wb, sheetName = "Интересы")
      
      xlsx.addTable(wb, sheet, eplaces(), startCol=2, startRow = 2, row.names = F)
      
      xlsx.addTable(wb, sheet, ebrands(), startCol=5, startRow = 2, row.names = F)
      
      xlsx.addTable(wb, sheet, ela(), startCol=8, startRow = 2, row.names = F)
      
      xlsx.addTable(wb, sheet, einterests(), startCol=11, startRow = 2, row.names = F)
      
      xlsx.addTable(wb, sheet, ehp(), startCol=14, startRow = 2, row.names = F)
      
      saveWorkbook(wb, file)
      }
  )
}
