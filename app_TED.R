library(shinydashboard)
library(shiny)
library(cowplot)


#Lecture des fichier
data_TED_CAN_2011_2020_modif <- read.csv("TED-Contract_award_notices_2011-2020(modifié).csv", sep=";", header = T)
data_TED_CN_2011_2020_modif <- read.csv("TED-Contract_notices_2011-2020(modifié).csv", encoding="UTF-8", sep=";")

#Converstion des données
data_TED_CAN_2011_2020_modif$YEAR = as.double(data_TED_CAN_2011_2020_modif$YEAR)
data_TED_CAN_2011_2020_modif$VALUE_EURO_FIN_2 = as.double(data_TED_CAN_2011_2020_modif$VALUE_EURO_FIN_2)
data_TED_CAN_2011_2020_modif$Count_pme = as.double(data_TED_CAN_2011_2020_modif$Count_pme)


data_TED_CN_2011_2020_modif$YEAR = as.double(data_TED_CN_2011_2020_modif$YEAR)
data_TED_CN_2011_2020_modif$VALUE_EURO_FIN_2 = as.double(data_TED_CN_2011_2020_modif$VALUE_EURO_FIN_2)


ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "AA&AM Dashboard"),
  dashboardSidebar(
    #Onglets
    sidebarMenu(
      menuItem("Lecture des données des AA", tabName = "readData_AA", icon = icon("readme")),
      menuItem("Lecture des données des AM", tabName = "readData_AM", icon = icon("readme")),
      menuItem("Visualisations des données des AA", tabName = "visualization_AA", icon = icon("poll")),
      menuItem("Visualisations des données des AM", tabName = "visualization_AM", icon = icon("poll")),
      menuItem("Visualisations concentrer sur les PME en générale", tabName = "PME_visualization_gen", icon = icon("poll")),
      menuItem("Visualisations concentrer sur les PME par année", tabName = "PME_visualization_A", icon = icon("poll"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "readData_AA",
              h1("Lecture des données des AA"),
              h3("File preview"),
              
              dataTableOutput(outputId = "preview_AA")
              
      ),
      tabItem(tabName = "readData_AM",
              h1("Lecture des données des AM"),
              h3("File preview"),
              dataTableOutput(outputId = "preview_AM")
              
      ),
      tabItem(tabName = "visualization_AA",
              h1("Visualisations des données des AA"),
              
              #Widget
              selectInput("choicedate_AA",
                          
                          label = h2("Choisir la date"),
                          
                          choices = unique(data_TED_CAN_2011_2020_modif$YEAR),
                          
                          selected = 1),
              
              
              plotOutput(outputId = "viz_AA")
              
              
      ),
      
      
      tabItem(tabName = "visualization_AM",
              h1("Visualisations des données des AM"),
              
              #Widget
              selectInput("choicedate_AM",
                          
                          label = h2("Choisir la date"),
                          
                          choices = unique(data_TED_CN_2011_2020_modif$YEAR),
                          
                          selected = 1),
              
             
              plotOutput(outputId = "viz_AM")
      ),
      tabItem(tabName = "PME_visualization_gen",
              h1("Visualisations pour les PME en générale"),
              
              #Widget
              selectInput("choicedata_PME",
                          
                          label = h2("Choisir entre :"),
                          
                          choices = c('Procédure'=1, 'Type de contrat'=2,'Pays'=3,'Activité principale'=4),
                          
                          selected = 1),
              
              
              plotOutput(outputId = "viz_PME_g")
      ),
      tabItem(tabName = "PME_visualization_A",
             h1("Visualisations pour les PME en fonction de l'année choisie"),
             
             #Widget
             selectInput("choicedate_PME",
                          
                         label = h2("Choisir une année"),
                          
                          choices = unique(data_TED_CAN_2011_2020_modif$YEAR),
                          
                          selected = 1),
              
              
              plotOutput(outputId = "viz_PME_a")
      )
    )
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$preview_AA <-  renderDataTable({
    
    #Affichage de l'entête des données
    head(data_TED_CAN_2011_2020_modif) 
    
  }, options = list(scrollX = TRUE , dom = 't'))
  #--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  output$preview_AM <-  renderDataTable({
    
    #Affichage de l'entêtes des données
    head(data_TED_CN_2011_2020_modif)
    
  }, options = list(scrollX = TRUE , dom = 't'))
  #-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
  output$viz_AA <- renderPlot({
    
    #Visualisation pour les avis d'attribution en fonction des pays de l'UE, du type de la procédure, du secteur d'activité de la CAE, et du type de contrats.
    
    pA1 = ggplot(data_TED_CAN_2011_2020_modif, aes(x=data_TED_CAN_2011_2020_modif$ISO_COUNTRY_CODE[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_AA], y=data_TED_CAN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_AA])) + geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$ISO_COUNTRY_CODE[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_AA]),data = ~ subset(., data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_AA), stat='identity') + xlab('Pays') + ylab('Valeurs')+ theme(plot.background = element_rect(fill = "#9999FF"))+ ggtitle("Valeur des avis attribués pour chaque pays de l'UE en fonction de l'année choisie")+ scale_fill_discrete(name="Code Pays")
    
    pA2 = ggplot(data_TED_CAN_2011_2020_modif, aes(x="",y=data_TED_CAN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_AA])) + geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$TOP_TYPE[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_AA]), data = ~ subset(., YEAR == input$choicedate_AA), position = "fill", stat = "identity")+ ylab('Valeurs')+ ggtitle("Valeur des avis attribués par type de procédure utilisés pour ces avis en fonction de l'année choisie")+ scale_fill_discrete(name="Type de procédure")+ theme(plot.background = element_rect(fill = "#9999FF"))+ coord_polar("y", start = 0)
    
    pA3 = ggplot(data_TED_CAN_2011_2020_modif, aes(x="",y=data_TED_CAN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_AA]))+ geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$MAIN_ACTIVITY[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_AA]), data = ~ subset(., YEAR == input$choicedate_AA), position = "fill", stat = "identity")+ ylab('Valeurs')+ ggtitle("Valeur des avis attribués par secteur d'activité des CAE pour l'année choisie")+ scale_fill_discrete(name="Type d'activité")+ theme(plot.background = element_rect(fill = "#9999FF"))+ coord_polar("y", start = 0)
    
    pA4 = ggplot(data_TED_CAN_2011_2020_modif, aes(x=data_TED_CAN_2011_2020_modif$TYPE_OF_CONTRACT[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_AA],y=data_TED_CAN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_AA])) + geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$TYPE_OF_CONTRACT[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_AA]), data = ~ subset(., YEAR == input$choicedate_AA), stat = "identity")+xlab('Type de contrat')+ylab('Valeurs')+ ggtitle("Valeur des avis attribués par type de contrat pour l'année choisie")+ scale_fill_discrete(name="Type de contrats")+ theme(plot.background = element_rect(fill = "#9999FF"))
    
    plot_grid(pA1, pA2, pA3, pA4)
    
    
  }
  
  )
  output$viz_AM <- renderPlot({
    
    #Visualisation pour les avis de marché en fonction des pays de l'UE, du type de la procédure, du secteur d'activité de la CAE, et du type de contrats.
    
    pM1 = ggplot(data_TED_CN_2011_2020_modif, aes(x=data_TED_CN_2011_2020_modif$ISO_COUNTRY_CODE[data_TED_CN_2011_2020_modif$YEAR==input$choicedate_AM], y=data_TED_CN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CN_2011_2020_modif$YEAR == input$choicedate_AM])) + geom_bar(aes(fill = data_TED_CN_2011_2020_modif$ISO_COUNTRY_CODE[data_TED_CN_2011_2020_modif$YEAR == input$choicedate_AM]),data = ~ subset(., data_TED_CN_2011_2020_modif$YEAR == input$choicedate_AM), stat='identity') + xlab('Pays') + ylab('Valeurs')+ theme(plot.background = element_rect(fill = "#9999FF"))+ ggtitle("Valeur des avis attribués pour chaque pays de l'UE en fonction de l'année choisie")+ scale_fill_discrete(name="Code Pays")
    
    pM2 = ggplot(data_TED_CN_2011_2020_modif, aes(x="",y=data_TED_CN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CN_2011_2020_modif$YEAR==input$choicedate_AM])) + geom_bar(aes(fill = data_TED_CN_2011_2020_modif$TOP_TYPE[data_TED_CN_2011_2020_modif$YEAR == input$choicedate_AM]), data = ~ subset(., YEAR == input$choicedate_AM), position = "fill", stat = "identity")+ ylab('Valeurs')+ ggtitle("Valeur des avis de marchés par type de procédure utilisé pour ces mêmes avis en fonction de l'année choisie")+ scale_fill_discrete(name="Type de procédure")+ theme(plot.background = element_rect(fill = "#9999FF"))+ coord_polar("y", start = 0)
    
    pM3 = ggplot(data_TED_CN_2011_2020_modif, aes(x=data_TED_CN_2011_2020_modif$MAIN_ACTIVITY[data_TED_CN_2011_2020_modif$YEAR == input$choicedate_AM],y=data_TED_CN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CN_2011_2020_modif$YEAR==input$choicedate_AM])) + geom_bar(aes(fill = data_TED_CN_2011_2020_modif$MAIN_ACTIVITY[data_TED_CN_2011_2020_modif$YEAR == input$choicedate_AM]), data = ~ subset(., YEAR == input$choicedate_AM), stat = "identity")+xlab('Activités') +ylab('Valeurs')+ ggtitle("Valeur des avis de marchés par secteur d'activité des CAE pour l'année choisie")+ scale_fill_discrete(name="Type d'activité")+ theme(plot.background = element_rect(fill = "#9999FF"))
    
    pM4 = ggplot(data_TED_CN_2011_2020_modif, aes(x="",y=data_TED_CN_2011_2020_modif$VALUE_EURO_FIN_2[data_TED_CN_2011_2020_modif$YEAR==input$choicedate_AM])) + geom_bar(aes(fill = data_TED_CN_2011_2020_modif$TYPE_OF_CONTRACT[data_TED_CN_2011_2020_modif$YEAR == input$choicedate_AM]), data = ~ subset(., YEAR == input$choicedate_AM), position = "fill", stat = "identity")+ ylab('Valeurs')+ggtitle("Valeur des avis de marchés par type de contrat pour l'année choisie")+ scale_fill_discrete(name="Type de contrats")+ theme(plot.background = element_rect(fill = "#9999FF"))+ coord_polar("y", start = 0)
    
    plot_grid(pM1, pM2, pM3, pM4)
    
    
  }
  
  )
  output$viz_PME_g <- renderPlot({
    
    #Visualisation pour les PME en générale en fonction des pays de l'UE, du type de la procédure, du secteur d'activité de la CAE, et du type de contrats. 
    
    if (input$choicedata_PME==3){
    ggplot(data_TED_CAN_2011_2020_modif, aes(x=ISO_COUNTRY_CODE, y=Count_pme)) + geom_bar(aes(fill = ISO_COUNTRY_CODE), stat='identity') + xlab('Pays') + ylab('Nombre des PME')+ theme(plot.background = element_rect(fill = "#9999FF"))+ ggtitle("Nombre de PME impliqué dans les contrats pour chaque pays de l'UE")+ scale_fill_discrete(name="Code Pays")
    }
    
    else if ( input$choicedata_PME==1){
    ggplot(data_TED_CAN_2011_2020_modif, aes(x="",y=Count_pme)) + geom_bar(aes(fill = TOP_TYPE), position = "fill", stat = "identity")+ ylab('Nombre de PME')+ ggtitle("Nombre de PME impliqué dans les avis en fonction du type de procédure utilisés")+ scale_fill_discrete(name="Type de procédure")+ theme(plot.background = element_rect(fill = "#9999FF"))
    }
    else if (input$choicedata_PME==4){
    ggplot(data_TED_CAN_2011_2020_modif, aes(x="",y=Count_pme))+ geom_bar(aes(fill = MAIN_ACTIVITY), position = "fill", stat = "identity")+ ylab('Nombre de PME')+ ggtitle("Nombre de PME impliqué dans les avis en fonction du secteur d'activité des CAE")+ scale_fill_discrete(name="Type d'activité")+ theme(plot.background = element_rect(fill = "#9999FF"))
    }
    else if (input$choicedata_PME==2){
    ggplot(data_TED_CAN_2011_2020_modif, aes(x=TYPE_OF_CONTRACT,y=Count_pme)) + geom_bar(aes(fill = TYPE_OF_CONTRACT), stat = "identity")+xlab('Type de contrat') +ylab('Nombre de PME')+ ggtitle("Nombre de PME par type de contrat")+ scale_fill_discrete(name="Type de contrats")+ theme(plot.background = element_rect(fill = "#9999FF"))
    }

    
  }
  
  )
  output$viz_PME_a <- renderPlot({
    
    #Visualisation pour les PME par années en fonction des pays de l'UE, du type de la procédure, du secteur d'activité de la CAE, et du type de contrats.
    
    plot_pme_1 = ggplot(data_TED_CAN_2011_2020_modif, aes(x=data_TED_CAN_2011_2020_modif$ISO_COUNTRY_CODE[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_PME], y=data_TED_CAN_2011_2020_modif$Count_pme[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_PME])) + geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$ISO_COUNTRY_CODE[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_PME]),data = ~ subset(., data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_PME), stat='identity') + xlab('Pays') + ylab('Valeurs')+ theme(plot.background = element_rect(fill = "#9999FF"))+ ggtitle("Nombre de PME impliqué dans les contrats pour chaque pays de l'UE en fonction de l'année choisie")+ scale_fill_discrete(name="Code Pays")
    
    plot_pme_2 = ggplot(data_TED_CAN_2011_2020_modif, aes(x=data_TED_CAN_2011_2020_modif$TOP_TYPE[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_PME],y=data_TED_CAN_2011_2020_modif$Count_pme[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_PME])) + geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$TOP_TYPE[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_PME]), data = ~ subset(., YEAR == input$choicedate_PME), stat = "identity")+xlab('Type de contrat')+ylab('Valeurs')+ ggtitle("Nombre de PME impliqué dans les avis en fonction du type de procédure utilisés et de l'année choisie")+ scale_fill_discrete(name="Type de procédure")+ theme(plot.background = element_rect(fill = "#9999FF"))
    
    plot_pme_3 = ggplot(data_TED_CAN_2011_2020_modif, aes(x="",y=data_TED_CAN_2011_2020_modif$Count_pme[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_PME]))+ geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$MAIN_ACTIVITY[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_PME]), data = ~ subset(., YEAR == input$choicedate_PME), position = "fill", stat = "identity")+ ylab('Valeurs')+ ggtitle("Nombre de PME impliqué dans les avis en fonction du secteur d'activité des CAE et de l'année choisie")+ scale_fill_discrete(name="Type d'activité")+ theme(plot.background = element_rect(fill = "#9999FF"))
    
    plot_pme_4 = ggplot(data_TED_CAN_2011_2020_modif, aes(x="",y=data_TED_CAN_2011_2020_modif$Count_pme[data_TED_CAN_2011_2020_modif$YEAR==input$choicedate_PME])) + geom_bar(aes(fill = data_TED_CAN_2011_2020_modif$TYPE_OF_CONTRACT[data_TED_CAN_2011_2020_modif$YEAR == input$choicedate_PME]), data = ~ subset(., YEAR == input$choicedate_PME), position = "fill", stat = "identity")+ ylab('Valeurs')+ ggtitle("Nombre de PME par type de contrat en fonction de l'année choisie")+ scale_fill_discrete(name="Type de contrats")+ theme(plot.background = element_rect(fill = "#9999FF"))
    
   plot_grid(plot_pme_1, plot_pme_2, plot_pme_3, plot_pme_4)
    
    
  }
  
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
