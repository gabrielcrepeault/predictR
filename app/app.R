## Load necessary packages
# setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(nlme)
library(MASS)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(car)
library(ggplot2)
library(dplyr)
library(alr3)
library(DT)

## HEADER
header <- dashboardHeader(title = 'PredictR',
                          ## Le dropdown est à travailler encore
                          dropdownMenu(type = 'notifications',
                                       headerText = 'Voici les modèles enregistrés',
                                       notificationItem('Ceci est un test')
                          ))

## SIDEBAR
sidebar <- dashboardSidebar(
  h4('Menu'),
  sidebarMenu(
    menuItem(
      'Analyse des données',
      tabName = 'data_analysis',
      icon = icon('database')
    ),
    menuItem(
      'Régression linéaire',
      tabName = 'regression_lineaire',
      icon = icon('coffee')
    ),
    menuItem(
      'Modèles linéaire généralisé',
      tabName = 'glm',
      icon = icon('beer')
    )
  ),
  h4('Base de données'),
  fileInput('file_input',
            label = 'Sélectionnez un fichier',
            buttonLabel = 'Parcourir',
            placeholder = 'Aucun fichier'),
  prettySwitch("bool_head", label = 'Le fichier a une entête',
               value = T, fill = T, status = 'success'),
  prettyCheckboxGroup('sep', label = 'Séparateurs de valeurs',
                      choiceNames = c('Virgule', 'point-virgule', 'tabulation'),
                      choiceValues = c(",", ";", "\t"), selected = ',',
                      status = 'success'
  ),
  
  icon('github'),
  a('Lien vers Code Source',
    href = 'https://github.com/gabrielcrepeault/predictR')
) 

## BODY
body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "data_analysis",
      fluidRow(box(title = 'Résumé des données',
                   width = 12,
                   collapsible = T, collapsed = T,
                   verbatimTextOutput('data_summary')
      )),
      fluidRow(box(title = 'Tableau des données',
                   width = 12,
                   collapsible = T, collapsed = T,
                   DT::dataTableOutput('data_table')
      )),
      
      
      
      fluidRow(box(title = 'Visualisation graphique bivariée',
                   width = 12, collapsible = T, collapsed = T,
                   dropdownButton(
                     h3("Changer les variables"),
                     uiOutput('select_reponse'),
                     uiOutput('select_exogene'),
                     
                     circle = TRUE, status = "success",
                     icon = icon("gear"), width = "300px",
                     
                     tooltip = tooltipOptions(title = "Cliquez pour changer les paramètres!")
                   ),
                   plotOutput('custom_plot')
      ))
    ),
    tabItem( ## Régression linéaire ####
             tabName = 'regression_lineaire',
             box(title = 'Modèles à analyser',
                 color = 'blue',
                 collapse = T, width = 12,
                 fluidRow(
                   column(6, textAreaInput('formula_fit_1', 'Entrez la formule du modèle 1')),
                   column(6, textAreaInput('formula_fit_2', 'Entrez la formule du modèle 2'))
                 )
             ),
             fluidRow( # avec les verbatim, il faut wrap dans fluidrow
               box(title = 'Sommaire de R',
                   status = 'info', solidHeader = T,
                   collapsible = T, collapsed = T,
                   width = 12,
                   fluidRow(
                     column(6, verbatimTextOutput('summary_lm_1')),
                     column(6, verbatimTextOutput('summary_lm_2'))
                   )  
               )),
             fluidRow( # avec les verbatim, il faut wrap dans fluidrow
               box(title = 'ANOVA',
                   status = 'info', solidHeader = T,
                   collapsible = T, collapsed = T,
                   width = 12,
                   fluidRow(
                     column(6, verbatimTextOutput('anova_lm_1')),
                     column(6, verbatimTextOutput('anova_lm_2'))
                   )  
               )),
             fluidRow( # avec les verbatim, il faut wrap dans fluidrow
               box(title = "Test pour manque d'ajustement (Lack-of-fit)",
                   status = 'info', solidHeader = T,
                   collapsible = T, collapsed = T,
                   width = 12,
                   fluidRow(
                     column(6, verbatimTextOutput('lack_of_fit_lm_1')),
                     column(6, verbatimTextOutput('lack_of_fit_lm_2'))
                   )  
               )),
             fluidRow( # avec les verbatim, il faut wrap dans fluidrow
               box(title = 'Graphique des résidus (Homoscédasticité)',
                   status = 'info', solidHeader = T,
                   collapsible = T, collapsed = T,
                   width = 12,
                   fluidRow(
                     column(6, plotOutput('plot_residus_lm_1')),
                     column(6, plotOutput('plot_residus_lm_2'))
                   )  
               )),
             fluidRow( # avec les verbatim, il faut wrap dans fluidrow
               box(title = 'QQ Plot (normalité)',
                   status = 'info', solidHeader = T,
                   collapsible = T, collapsed = T,
                   width = 12,
                   fluidRow(
                     column(6, plotOutput('plot_qqplot_lm_1')),
                     column(6, plotOutput('plot_qqplot_lm_2'))
                   )  
               ))
    ),
    tabItem( # GLM-ui ####
             tabName = 'glm',
             tabsetPanel(
               tabPanel('Analyse individuelle',
                        fluidRow(
                          tabBox(
                            tabPanel(title = 'Modèle', 
                                     textAreaInput('formula_glm_1', width = "300px",
                                                   'Entrez la formule du GLM'),
                                     selectInput('glm1_family', 'Choisir la famille',
                                                 choices = c('binomial',
                                                             'gaussian',
                                                             'Gamma',
                                                             'inverse.gaussian',
                                                             'poisson')),
                                     selectInput('glm1_link', 'Choisir la fonction de lien',
                                                 choices = c('logit',
                                                             'probit',
                                                             'identity',
                                                             'log',
                                                             'loglog')),
                                     actionBttn("glm_add", label = 'Ajouter le modèle',
                                                color = 'success', icon = icon('plus'))
                            ),
                            tabPanel(title = 'Variables disponibles',
                                     verbatimTextOutput("variables_name")
                            )
                          ),
                          valueBoxOutput('chi_square_glm'),
                          box(title = 'Analyse Drop1',
                              status = 'info', solidHeader = T,
                              verbatimTextOutput("drop1_glm_1"),
                              p('Test Chisq par défaut')
                          )),
                        fluidRow(
                          box(title = 'Summary de R', status = 'info',
                              solidHeader = T,
                              verbatimTextOutput('summary_glm_1')
                          ),
                          box(title = 'Analyse ANOVA sur un modèle', status = 'info',
                              solidHeader = T,
                              verbatimTextOutput('anova_glm_1')
                          )
                          
                        )
               ),
               tabPanel(title = 'Comparaison',
                        box(title = "Modèles à comparer",
                            width = 12,
                            uiOutput('glm_compare')
                        )
               )
             )
    )
  )
)

ui <- dashboardPage(skin = 'green', header = header,
                    sidebar = sidebar, body = body)

server <- function(input, output, session) {
  ## Stop the app when browser is closed
  session$onSessionEnded(stopApp)
  ## Increase max upload file limit to 100Mb
  options(shiny.maxRequestSize = 100 * 1024^2)
  
  # Custom function (needs to be in the server function?)
  simpleAnova <- function(object, ...) {
    # Fonction trouvée sur les internet : 
    # On l'utilise seulement dans la section régression linéaire
    # https://stats.stackexchange.com/questions/145790/anova-table-for-model-in-r
    # Compute anova table
    tab <- anova(object, ...)
    
    # Obtain number of predictors
    p <- nrow(tab) - 1
    
    # Add predictors row
    predictorsRow <- colSums(tab[1:p, 1:2])
    predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
    
    # F-quantities
    Fval <- predictorsRow[3] / tab[p + 1, 3]
    pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
    predictorsRow <- c(predictorsRow, Fval, pval)
    
    # Simplified table
    tab <- rbind(predictorsRow, tab[p + 1, ])
    row.names(tab)[1] <- "Régression"
    return(tab)
  }
  
  ## DATA_ANALYSIS
  dat <- reactive({
    read.table(input$file_input$datapath,
               header = input$bool_head, sep = input$sep)
  })
  
  output$data_summary <- renderPrint({
    summary(dat())
  })
  
  output$data_table <- DT::renderDataTable(dat(), filter = 'top')
  
  output$select_reponse <- renderUI({
    selectInput('var_endogene', 'Sélectionnez la variable endogène (Y)',
                choices = names(dat())[-1])
  })
  
  output$select_exogene <- renderUI({
    selectInput('var_exogene', 'Sélectionnez la variable explicative (X)',
                choices = names(dat())[-1])
  })
  
  output$custom_plot <- renderPlot({
    plot(as.formula(paste0(input$var_endogene, '~', input$var_exogene)), data = dat())
  })
  
  output$variables_name <- renderPrint(data.frame(Variables = names(dat())))
  
  ##REGRESSION_LINEAIRE
  ## Modèles
  lmfit1 <- reactive({
    lm(as.formula(input$formula_fit_1), data = dat())
  })
  lmfit2 <- reactive({
    lm(as.formula(input$formula_fit_2), data = dat())
  })
  glmfit1 <- reactive({
    glm(formula = as.formula(input$formula_glm_1),
        data = dat(), family = input$glm1_family)
  })
  
  
  ## Summary de R
  output$summary_lm_1 <- renderPrint({
    summary(lmfit1())
  })
  output$summary_lm_2 <- renderPrint({
    summary(lmfit2())
  })
  
  
  ## ANOVA de R
  output$anova_lm_1 <- renderPrint({
    simpleAnova(lmfit1())
  })
  output$anova_lm_2 <- renderPrint({
    simpleAnova(lmfit2())
  })
  
  ## LAck-of-fit de R
  output$lack_of_fit_lm_1 <- renderPrint({
    pureErrorAnova(lmfit1())
  })
  output$lack_of_fit_lm_2 <- renderPrint({
    pureErrorAnova(lmfit2())
  })
  
  ## Graph résidus vs y
  output$plot_residus_lm_1 <- renderPlot({
    plot(lmfit1()$fitted.values, lmfit1()$residuals)
  })
  output$plot_residus_lm_2 <- renderPlot({
    plot(lmfit2()$fitted.values, lmfit2()$residuals)
  })
  
  ## QQ-Plot
  output$plot_qqplot_lm_1 <- renderPlot({
    qqnorm(rstandard(lmfit1()))
  })
  output$plot_qqplot_lm_2 <- renderPlot({
    qqnorm(rstandard(lmfit2()))
  })
  
  
  #### GLM ####
  output$summary_glm_1 <- renderPrint({
    summary(glmfit1())
  })
  output$anova_glm_1 <- renderPrint({
    anova(glmfit1())
  })
  output$drop1_glm_1 <- renderPrint({
    drop1(glmfit1(), test = 'Chisq')
  })
  
  output$chi_square_glm <- renderValueBox({
    chi <- sum(residuals.glm(glmfit1(), type = 'pearson')^2)
    valueBox(value = chi, subtitle = 'X^2',
             icon = icon('')
    )
  })
  
  
  
  output$anova_glm_multiple <- renderPrint({
    do.call(anova,
            fit_list$fit[match(input$glm_fits_selected,
                               unlist(fit_list$formula))])
  })
  
  ## Ajout de formule GLM à une liste pour comparaison
  fit_list <- reactiveValues(count = 0, fit = NULL)
  observeEvent(input$glm_add,{ ## POUR AJOUTER LES MODÈLES TESTÉS DANS LA BASE
    fit_list$count <- fit_list$count + 1
    fit_list$fit[[fit_list$count]] <- glmfit1()
    fit_list$formula <- c(isolate(fit_list$formula), isolate(input$formula_glm_1))
  })
  
  output$glm_compare <- renderUI({
    tagList(
      fluidRow(
        column(6,
               selectizeInput(
                 inputId = "glm_fits_selected",
                 label = "Sélectionnez les modèles à considérer",
                 choices = fit_list$formula,
                 multiple = TRUE
               )
        ),
        column(6, verbatimTextOutput('anova_glm_multiple'))
      )
    ) # Fin du taglist
  })
  
} ## FIN DE LA CONFIGURATION


## Lancer l'application
shinyApp(ui, server)