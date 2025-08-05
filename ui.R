# Load the mathJax library
#library(mathJax)
#withMathJax()



# Define the dashboard header
header = dashboardHeader(title = paste("Decision Support Tool: Comparing Treatment Approaches for Acute Malnutrition"),
                          titleWidth = 800
                          #tags$li(class="dropdown",
                          #        tags$a(href='https://github.com/LSkrip', icon('github'), "Source Code", target="_blank"))
          )


#######################################################
### Sidebar ###
#######################################################

# Sidebar (icons from https://fontawesome.com/icons)
sidebar = dashboardSidebar(
  
  div(style="text-align:center",
      hr(),
      h3("Menu"),
      hr()
  ),
  
  sidebarMenu(id = "inTabset",
    menuItem("Introduction", tabName = "introduction", icon = shiny::icon("home")),
    menuItem("Define objective", tabName = "objective", icon = shiny::icon("user-md")),
    menuItem("Describe context", tabName = "context", icon = shiny::icon("tachometer-alt")),
    menuItem("Generate evidence", tabName = "results", icon = shiny::icon("paper-plane")),
    menuItem("Complete survey", tabName = "survey", icon = shiny::icon("comments")),
    menuItem(" ", tabName = "results2")
  ) # Closes sidebarMenu
  
) # closes Sideboard

## Define the Body
body = dashboardBody(
  
  #initiate telemetry
  use_telemetry(),
  
  # customise CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  # Tailor box color to IDM branding?
  tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#0173A1;
                    background:#0173A1
                    }

                    .box.box-solid.box-primary{
                    border-bottom-color:#000000;
                    border-left-color:#000000;
                    border-right-color:#000000;
                    border-top-color:#000000;
                    background:#50C0E4
                    }
                    .info-box {min-height: 45px;} .info-box-icon {height: 50px; line-height: 50px;} .info-box-content {padding-top: 2.5px; padding-bottom: 2.5px;}

                    ")),
  
  ###  Model
  # Scenarios
  tabItems(
  tabItem(tabName = "objective",
          fluidPage(
            div(style="text-align:center",
                shinydashboard::box(width = "100%", status = "primary", solidHeader = TRUE,
                                    h4(HTML(paste(icon("user-md"), "Define your primary objective"))))
            ),
            
            column(width = 12,
                   shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                       radioButtons("ph_objective",
                                                    label = span(tagList(icon('chart-bar'), "Which of the following best describes your primary programmatic objective?")),
                                                    choices = c("Improve cost efficiency of nutrition program", "Reach more children with a fixed budget",
                                                    #"Consider alternative approaches when product supply is limited"
                                                    "Understand amount of RUTF needed for programming across protocols"
                                                    ), 
                                                    inline = FALSE),
                                       hr(), 
                                       radioButtons("approach_based_on",
                                                    label = span(tagList(icon('chart-bar'), "Which treatment approach is currently being used in your setting?")),
                                                    choices = c("Standard protocol (most often in use per national policies): multiple products, weight-based dosing for SAM, fixed dosing for MAM" = "Standard",
                                                               "ComPAS protocol: Single product, MUAC-based dosing for both SAM and MAM, two standardized doses" = "ComPAS",
                                                                "OptiMA protocol: Single product, MUAC- and weight-based dosing for both SAM and MAM, phased dosing" = "OptiMA",
                                                                "MANGO protocol: SAM treatment only, Single product, dosing per national protocol for weeks 1-2, followed by standardized, reduced dosing in week 3 to discharge" = "MANGO"),
                                                          inline = FALSE),
                                       #conditionalPanel(
                                         #condition = "input.approach_based_on == 'Standard'",
                                         radioButtons("standard_mam_product",
                                                      label = span(tagList(icon('chart-bar'), "Under the Standard protocol, which product is used for MAM in your setting?")),
                                                      choices = c("Ready-to-use Supplementary Food (RUSF)" = "RUSF",
                                                                  "Fortified Blended Food (FBF), such as Super Cereal Plus and Corn Soy Blend" = "CSB"), 
                                                                  inline = FALSE)
                                         #)
                                       ),
                   
                   div(style="text-align:center",
                       shinydashboard::box(width = "100%", status = "primary", solidHeader = TRUE,
                                           h4(HTML(paste(icon("user-md"), "About the protocols"))))
                   ),
                   shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                       
                                       #HTML(paste("The", tags$code("dose optimization"), "model allows you to consider the following treatment approaches:")), br(),br(),
                                       
                                       p(tags$a(href="https://www.acutemalnutrition.org/en/Simplified-Approaches-Introduction", target="_blank", "What are simplified approaches?")), 
                                       
                                       tableOutput('DosingDetailsTable'),
                                       #br(),
                                       # "(i) Single product, uniform dosing approach;", br(), 
                                       # "(ii) Multiple products, uniform dosing approach", br(), br(),
                                       #HTML(paste("The effectiveness and efficiency of the new treatment approach are compared to that of the current standard")),
                                       
                                       #hr(),
                                       
                                       HTML(paste("To read more about the treatment protocols and available evidence, please see the following:",br(),br(),
                                                  "1. MANGO: Kangas, S. T., et al. (2022). Effectiveness of Acute Malnutrition Treatment at Health Center and Community Levels with a Simplified, Combined Protocol in Mali: An Observational Cohort Study. Nutrients, 14(22), 4923.",
                                                  br())),
                                       p(tags$a(href="https://www.mdpi.com/2072-6643/14/22/4923", icon("chrome"), target="_blank", "Link to article")),
                                       HTML(paste("2. ComPAS: Bailey J, et al. (2020) A simplified, combined protocol versus standard treatment for acute malnutrition in children 6–59 months (ComPAS trial): A cluster-randomized controlled non-inferiority trial in Kenya and South Sudan. PLoS Med 17(7): e1003192.",
                                                  br())),
                                       p(tags$a(href="https://doi.org/10.1371/journal.pmed.1003192", icon("chrome"), target="_blank", "Link to article")),
                                       HTML(paste("3. OptiMA: Cazes, C., er al. (2021). Optimising Malnutrition Treatment in Children 6–59 Months-OptiMA-DRC: Primary Outcome of a Randomised Control Trial in Democratic Republic of Congo. Current Developments in Nutrition, 5(Suppl 2), 111.",
                                                  br())),
                                       p(tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8181218/", icon("chrome"), target="_blank", "Link to article"))
                                       
                   )
            )
            
          )  # Closes fluidPage
          
          
  ), # Closes tabPanel
  
  #define info icon
    tabItem(tabValue = "contextValue",
            tabName = "context",
            fluidPage(
              
              #defining info icon
              tags$head(
                tags$style(HTML("
                   .info-icon {
                    display: inline-block;
                    border-radius: 50%;
                    border: 1px solid #2c3e50;
                    width: 14px;
                    height: 14px;
                    text-align: center;
                    font-size: 10px;
                    margin-left: 5px;
                    cursor: help;
                }
              "))
              ),
              
              #inputs
              div(style="text-align:center",
                  shinydashboard::box(width = "100%", status = "primary", solidHeader = TRUE,
                      h4(HTML(paste(icon("tachometer-alt"), "Enter context-specific information"))))
              ),
              column(width = 12,
                     shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                         selectInput("countryChoice",
                                                     label = span(tagList(icon('globe-africa'), 'Country')),
                                                     choices = c("Select from list", 'Bangladesh', 'Burkina Faso','DRC','Ethiopia', 'Ghana', 'Malawi','Mali', 'Mozambique', 'Nepal', 'Niger', 'Pakistan', 'Sierra Leone', 'Somalia','South Sudan', 'Uganda', 'Other'),
                                                     selected = "",
                                                     multiple = FALSE),
                         hr(),
                         textOutput("selected_country"),
                         br()

                     )
            ),
            
            column(width = 12,
                   shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                       numericInput("n_pop",
                                                    label = span(tagList(icon('user'), 'Estimated number of SAM children in the catchment')),
                                                    min=0, value = 5000),
                                     
                                       numericInput("n_pop_m",
                                                    label = span(tagList(icon('user'), 'Estimated number of MAM children in the catchment')),
                                                    min=0, value = 5000),
                                       
                                       sliderInput("deteriorate_prop_m",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "Proportion of MAM who deteriorate to SAM (enter range)"#, 
                                                                      #tags$span(class = "info-icon", "i", id = "deteriorate_prop_m_info"))
                                                                      )),
                                                   min=0, max=1, step = .01, value = c(0.2,0.35)),
                                     
                                       conditionalPanel(
                                         condition = "input.ph_objective != 'Understand amount of RUTF needed for programming across protocols'",
                                       radioButtons("mamorsam", 
                                                    label = tagList(
                                                      "Would you like to investigate programmatic needs and results when treating SAM only or both SAM and MAM children?"
                                                      #tags$span(class = "info-icon", "i", id = "mamorsam_info")
                                                    ),
                                                    choices = c("Both SAM and MAM",
                                                                "Just SAM")
                                       ),
                                       hr(),
                                       # Add conditional panels that show when a specific option is selected
                                       conditionalPanel(
                                         condition = "input.mamorsam == 'Just SAM'",
                                         div(class = "info-box",
                                             h4("Information about treating just SAM"),
                                             p("Selecting this option will automatically set MAM coverage to zero; this will make all other MAM treatment parameters redundant. Changing MAM parameters will have no effect on the results."),
                                             hr()
                                         )
                                       ),
                                       
                                       
                                       conditionalPanel(
                                         condition = "input.mamorsam == 'Both SAM and MAM'",
                                         div(class = "info-box",
                                             h4("Information about treating both SAM and MAM"),
                                             p("Combined approaches address the full spectrum of acute malnutrition with integrated protocols that may simplify the treatment pathway."),
                                             hr()
                                         )
                                       ))
                   )
            ),
              
              ###############################
              ### SAM-specific Parameters ###
              ###############################
            conditionalPanel(
              condition = "input.ph_objective == 'Understand amount of RUTF needed for programming across protocols' && input.approach_based_on == 'Standard'",
              column(width = 12,
                     shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                         div(style="text-align:center",
                                             
                                             h4(tagList(icon('tachometer-alt'), 'SAM-specific model parameters')),
                                             actionButton("toggle_sam1", "Show Contents", width = "100%")
                                         ),
                                         # hr(),
                                         
                                         conditionalPanel(
                                           condition = "input.toggle_sam1 % 2 == 1",
                                           
                                           hr(),
                                           div(style="text-align:center",
                                               h4(tagList(icon('user'), 'Currently observed treatment inputs for SAM'))
                                           ),
                                           
                                           hr(),
                                           
                                           
                                           # numericInput("n_pop",
                                           #              label = span(tagList(icon('user'), 'Estimated number of SAM children')),
                                           #              min=0, value = 5000),
                                           
                                           sliderInput("cov_s",
                                                       label = span(tagList(icon('user'), 'Treatment coverage (proportion) for SAM children')),
                                                       min=0, max=1, step = 0.01, value = 0.5),
                                           
                                           sliderInput("weight.adm.s",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "Average weight on admission for SAM patients, in kg")),
                                                       min=1, max=21, step = .1, value = 7),
                                           sliderInput("muac.adm.s",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "Average MUAC on admission for SAM patients, in mm")),
                                                       min=100, max=120, step = 1, value = 112),
                                           hr(),
                                           div(style="text-align:center",
                                               h4(tagList(icon('user'), 'Currently observed treatment outcomes for SAM')),
                                               p(paste('Please adjust to reflect the treatment outcomes observed in your setting under the current protocol indicated in the previous tab.'))
                                           ),
                                           
                                           hr(),
                                           
                                           sliderInput("Recov_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion recovered", 
                                                                          tags$span(class = "info-icon", "i", id = "recovprop_info"))),
                                                       #min=0, max=1, step = .001, value = 0.438),
                                                       min=0, max=1, step = .001, value = 0.783),
                                           sliderInput("Def_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion defaulted", 
                                                                          tags$span(class = "info-icon", "i", id = "propdefaulted_info"))),
                                                       #min=0, max=1, step = .001, value = 0.307),
                                                       min=0, max=1, step = .001, value = 0.029),
                                           sliderInput("Dea_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion deceased",
                                                                          tags$span(class = "info-icon", "i", id = "deaprop_info"))),
                                                       #min=0, max=1, step = .001, value = 0.01),
                                                       min=0, max=1, step = .001, value = 0.031),
                                           sliderInput("Hosp_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion hospitalized",
                                                                          tags$span(class = "info-icon", "i", id = "hospprop_info"))),
                                                       #min=0, max=1, step = .001, value = 0.011),
                                                       min=0, max=1, step = .001, value = 0.0),
                                           sliderInput("NevRecov_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion never recovered",
                                                                          tags$span(class = "info-icon", "i", id = "nevrecovprop_info"))),
                                                       #min=0, max=1, step = .001, value = 0.147),
                                                       min=0, max=1, step = .001, value = 0.118),
                                           br(),
                                           HTML(paste("Note: The ", tags$strong("Proportion transferred"), " slider will update automatically to reflect the difference between 1 (i.e., 100%) and
                                     the sum of the other five outcome categories.")),
                                           br(),br(),
                                           sliderInput("Transf_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion transferred", 
                                                                          tags$span(class = "info-icon", "i", id = "transfprop_info"))),
                                                       min=0, max=1, step = .001, value = 0.014),
                                           
                                           hr(),
                                           div(style="text-align:center",
                                               h4(tagList(icon('user'), 'SAM Children who relapse')) 
                                           ),
                                           
                                           hr(),
                                           sliderInput("relapse_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "Among SAM children who recover after treatment, the proportion who relapse to SAM (enter range)", 
                                                                          tags$span(class = "info-icon", "i", id = "relapseprop_info"))),
                                                       min=0, max=1, step = .01, value = c(0.015,0.136)),
                                           hr(),
                                           sliderInput("relapse_prop_sam_to_mam",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "Among SAM children who recover after treatment, the proportion who relapse to MAM (enter range)", 
                                                                          tags$span(class = "info-icon", "i", id = "relapseprop_info"))),
                                                       min=0, max=1, step = .01, value = c(0.0725,0.226)),
                                           br(),br(),
                                           sliderInput("relapse_treat_prop",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "Among those who relapse, the proportion who seek treatment again", 
                                                                          tags$span(class = "info-icon", "i", id = "relapsetreat_info"))),
                                                       min=0, max=1, step = .1, value = 1),
                                           br(),br(),
                                           sliderInput("recov_change",
                                                       label = HTML(paste(shiny::icon('sliders-h'), "Among those who relapse and seek treatment again, reduction in recovery", 
                                                                          tags$span(class = "info-icon", "i", id = "recov_change_info"))),
                                                       min=0, max=1, step = .1, value = 0.15)
                                         )
                     )
              )),
            
            conditionalPanel(
              condition = "input.ph_objective != 'Understand amount of RUTF needed for programming across protocols' || input.approach_based_on != 'Standard'",
              column(width = 6,
                     shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                         div(style="text-align:center",
                           
                               h4(tagList(icon('tachometer-alt'), 'SAM-specific model parameters')),
                             actionButton("toggle_sam1", "Show Contents", width = "100%")
                         ),
                        # hr(),
                         
                         conditionalPanel(
                           condition = "input.toggle_sam1 % 2 == 1",
                         
                           hr(),
                           div(style="text-align:center",
                               h4(tagList(icon('user'), 'Currently observed treatment inputs for SAM'))
                           ),
                           
                           hr(),
                           
                           
                         # numericInput("n_pop",
                         #              label = span(tagList(icon('user'), 'Estimated number of SAM children')),
                         #              min=0, value = 5000),
                         
                         sliderInput("cov_s",
                                     label = span(tagList(icon('user'), 'Treatment coverage (proportion) for SAM children')),
                                     min=0, max=1, step = 0.01, value = 0.5),
                         
                         sliderInput("weight.adm.s",
                                     label = HTML(paste(shiny::icon('sliders-h'), "Average weight on admission for SAM patients, in kg")),
                                     min=1, max=21, step = .1, value = 7),
                         sliderInput("muac.adm.s",
                                     label = HTML(paste(shiny::icon('sliders-h'), "Average MUAC on admission for SAM patients, in mm")),
                                     min=100, max=120, step = 1, value = 112),
                         hr(),
                         div(style="text-align:center",
                             h4(tagList(icon('user'), 'Currently observed treatment outcomes for SAM')),
                             p(paste('Please adjust to reflect the treatment outcomes observed in your setting under the current protocol indicated in the previous tab.'))
                         ),
                         
                         hr(),
                         
                         sliderInput("Recov_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion recovered", 
                                                        tags$span(class = "info-icon", "i", id = "recovprop_info"))),
                                     #min=0, max=1, step = .001, value = 0.438),
                                     min=0, max=1, step = .001, value = 0.783),
                         sliderInput("Def_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion defaulted", 
                                                        tags$span(class = "info-icon", "i", id = "propdefaulted_info"))),
                                     #min=0, max=1, step = .001, value = 0.307),
                                     min=0, max=1, step = .001, value = 0.029),
                         sliderInput("Dea_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion deceased",
                                                        tags$span(class = "info-icon", "i", id = "deaprop_info"))),
                                     #min=0, max=1, step = .001, value = 0.01),
                                     min=0, max=1, step = .001, value = 0.031),
                         sliderInput("Hosp_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion hospitalized",
                                                        tags$span(class = "info-icon", "i", id = "hospprop_info"))),
                                     #min=0, max=1, step = .001, value = 0.011),
                                     min=0, max=1, step = .001, value = 0.0),
                         sliderInput("NevRecov_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion never recovered",
                                                        tags$span(class = "info-icon", "i", id = "nevrecovprop_info"))),
                                     #min=0, max=1, step = .001, value = 0.147),
                                     min=0, max=1, step = .001, value = 0.118),
                         br(),
                         HTML(paste("Note: The ", tags$strong("Proportion transferred"), " slider will update automatically to reflect the difference between 1 (i.e., 100%) and
                                     the sum of the other five outcome categories.")),
                         br(),br(),
                         sliderInput("Transf_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "SAM Proportion transferred", 
                                                        tags$span(class = "info-icon", "i", id = "transfprop_info"))),
                                     min=0, max=1, step = .001, value = 0.014),
                        
                          hr(),
                         div(style="text-align:center",
                             h4(tagList(icon('user'), 'SAM Children who relapse')) 
                         ),
                         
                         hr(),
                         sliderInput("relapse_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "Among SAM children who recover after treatment, the proportion who relapse to SAM (enter range)", 
                                                        tags$span(class = "info-icon", "i", id = "relapseprop_info"))),
                                     min=0, max=1, step = .01, value = c(0.015,0.136)),
                         hr(),
                         sliderInput("relapse_prop_sam_to_mam",
                                     label = HTML(paste(shiny::icon('sliders-h'), "Among SAM children who recover after treatment, the proportion who relapse to MAM (enter range)", 
                                                        tags$span(class = "info-icon", "i", id = "relapseprop_info"))),
                                     min=0, max=1, step = .01, value = c(0.0725,0.226)),
                         br(),br(),
                         sliderInput("relapse_treat_prop",
                                     label = HTML(paste(shiny::icon('sliders-h'), "Among those who relapse, the proportion who seek treatment again", 
                                                        tags$span(class = "info-icon", "i", id = "relapsetreat_info"))),
                                     min=0, max=1, step = .1, value = 1),
                         br(),br(),
                         sliderInput("recov_change",
                                     label = HTML(paste(shiny::icon('sliders-h'), "Among those who relapse and seek treatment again, reduction in recovery", 
                                                        tags$span(class = "info-icon", "i", id = "recov_change_info"))),
                                     min=0, max=1, step = .1, value = 0.15)
                           )
                     )
                     ),
    
            
            ###############################
            ### MAM-specific parameters ###
            ###############################
            column(width = 6,
                   shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4(tagList(icon('tachometer-alt'), 'MAM-specific model parameters')),
                                           actionButton("toggle_mam1", "Show Contents", width = "100%")
                                       ),
                                       
                                       conditionalPanel(
                                         condition = "input.toggle_mam1 % 2 == 1",
                                       
                                       hr(),
                                       conditionalPanel(
                                         condition = "input.ph_objective != 'Understand amount of RUTF needed for programming across protocols'",
                                       div(style="text-align:center",
                                           h4(tagList(icon('user'), 'Currently observed treatment inputs for MAM'))
                                       ),
                                       
                                       hr(),
                                       sliderInput("cov_m",
                                                    label = span(tagList(icon('user'), 'Treatment coverage (proportion) for MAM children')),
                                                    min=0, max=1, step = 0.01, value = 0.5),
                                       
                                       hr()),
                                       div(style="text-align:center",
                                           h4(tagList(icon('user'), 'Currently observed treatment outcomes for MAM')),
                                           p(paste('Please adjust to reflect the treatment outcomes observed in your setting under the current protocol indicated in the previous tab.'))
                                       ),
                                       
                                       hr(),
                                       
                                       sliderInput("Recov_prop_m",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "MAM Proportion recovered", 
                                                                      tags$span(class = "info-icon", "i", id = "recovprop_info_m"))),
                                                   min=0, max=1, step = .001, value = 0.529),
                                       sliderInput("Def_prop_m",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "MAM Proportion defaulted", 
                                                                      tags$span(class = "info-icon", "i", id = "propdefaulted_info_m"))),
                                                   min=0, max=1, step = .001, value = 0.153),
                                       sliderInput("Dea_prop_m",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "MAM Proportion deceased",
                                                                      tags$span(class = "info-icon", "i", id = "deaprop_info_m"))),
                                                   #min=0, max=1, step = .001, value = 0.01),
                                                   min=0, max=1, step = .001, value = 0.056),
                                       sliderInput("Hosp_prop_m",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "MAM Proportion hospitalized",
                                                                      tags$span(class = "info-icon", "i", id = "hospprop_info_m"))),
                                                   #min=0, max=1, step = .001, value = 0.011),
                                                   min=0, max=1, step = .001, value = 0.0),
                                       sliderInput("NevRecov_prop_m",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "MAM Proportion never recovered",
                                                                      tags$span(class = "info-icon", "i", id = "nevrecovprop_info_m"))),
                                                   min=0, max=1, step = .001, value = 0.067),
                                       hr(),
                                       div(style="text-align:center",
                                           h4(tagList(icon('user'), 'MAM Children who relapse')) 
                                       ),
                                       
                                       hr(),
                                       sliderInput("relapse_prop_mam_to_mam",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "Among MAM children who recover after treatment, the proportion who relapse to MAM (enter range)", 
                                                                      tags$span(class = "info-icon", "i", id = "relapseprop_info"))),
                                                   min=0, max=1, step = .01, value = c(0.1,0.3)),
                                       hr(),
                                       sliderInput("relapse_prop_mam_to_sam",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "Among MAM children who recover after treatment, the proportion who relapse to SAM (enter range)", 
                                                                      tags$span(class = "info-icon", "i", id = "relapseprop_info"))),
                                                   min=0, max=1, step = .01, value = c(0,0.1)),
                                       br(),br(),
                                       sliderInput("relapse_treat_prop_mam",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "Among those who relapse, the proportion who seek treatment again", 
                                                                      tags$span(class = "info-icon", "i", id = "relapsetreat_info"))),
                                                   min=0, max=1, step = .1, value = 1),
                                       br(),br(),
                                       sliderInput("recov_change_mam",
                                                   label = HTML(paste(shiny::icon('sliders-h'), "Among those who relapse and seek treatment again, reduction in recovery", 
                                                                      tags$span(class = "info-icon", "i", id = "recov_change_info"))),
                                                   min=0, max=1, step = .1, value = 0.15)
                   ))
            )),
            
            
            column(width = 12,
                   shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4(tagList('Review assumptions for treatment outcomes across all protocols')),
                                           actionButton("toggle_treatment_outcome_assumptions", "Show Contents", width = "100%")
                                       ),
                                       conditionalPanel(
                                         condition = "input.toggle_treatment_outcome_assumptions % 2 == 1",
                                         p("Note: The values entered above will be used in place of those in the table for the protocol you have indicated as the current approach in your setting."),
                                         br(),
                                         fluidRow(tableOutput("assumptions_table")),
                                         hr()
                                       )
                                       )),
            #####################################
            ### Cost Inputs for Any Selection ###
            #####################################   
            conditionalPanel(
              condition = "input.ph_objective != 'Understand amount of RUTF needed for programming across protocols'",
            column(width = 12,
                   shinydashboard::box(width="100%", height = "100%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4(tagList(icon('money-bill-alt'), 'Cost parameters')),
                                           actionButton("toggle_cost", "Show Contents", width = "100%")
                                       ),
                                      # hr(),
                                       
                                       conditionalPanel(
                                         condition = "input.toggle_cost % 2 == 1",
                                       selectInput(inputId = "Currency", label = span(tagList(icon('money-bill-alt'),"Please indicate the currency you are using for your cost inputs.")),
                                                   choices = c("US Dollar" = "USD",
                                                               "Bangladeshi taka" = "BTaka",
                                                               "West African CFA Franc" = "WAFranc", 
                                                               "Congolese Franc" = "CFranc",
                                                               "Ethiopian Birr" = "EBirr",
                                                               "Ghanaian cedi" = "GCedi",
                                                               "Malawian kwacha" = "MKwacha",
                                                               "Mozambician metical" = "MMetical",
                                                               "Nepalese rupee" = "NRupee",
                                                               "Pakistani rupee" = "PRupee",
                                                               "Sierra leonean leone" = "SLLeone",
                                                               "Somali Shilling" = "SShilling",
                                                               "South Sudanese Pound" = "Pound",
                                                               "Other" = "Other"
                                                               ),
                                                   
                                                   multiple=FALSE),
                                       
                                       conditionalPanel(
                                         condition = "input.Currency == 'Other'",
                                         textInput(
                                           inputId = "OtherCurrency",
                                           label = "Please specify your currency:",
                                           placeholder = "e.g., Euro, Japanese Yen, etc."
                                         )),
                                       br(),
                                        numericInput("conv_rate",
                                                                  label = span(tagList(icon('user'), 'Current rate from chosen currency to 1 USD.')),
                                                                  min=0, value = 1),
                                       br(),
                                                     HTML(paste("Note: The conversion rate is used to convert inputs from the specified currency to USD, as the model is programmed to run in USD. However, please use any currency relevant to your setting and please use it consistently for all cost inputs. The model outputs will presented in the chosen currency.")),
                                       hr(),
                                       # 
                                       # numericInput("budget.program",
                                       #              label = span(tagList(icon('money-bill-alt'), 'Annual nutrition program budget allocated to treatment')),
                                       #              min=0, value = 100000), 
                                       
           numericRangeInput("rutf.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Per-sachet cost of RUTF",
                                                           tags$span(class = "info-icon", "i", id = "rutfcost_info"))),
                              #value = c(round(min(c(0.31*312.332/292.655 ,0.32*312.332/292.655,0.39*312.332/292.655,0.34*312.332/292.655,0.354*312.332/258.811 ,0.369*312.332/258.811)*1.15)*0.9,2), round(max(c(0.31*312.332/292.655 ,0.32*312.332/292.655,0.39*312.332/292.655,0.34*312.332/292.655,0.354*312.332/258.811 ,0.369*312.332/258.811)*1.15)*0.9,2))),
                              value = c(round(0.31*0.9,2), round(0.369*1.15,2))),
            
           # Add conditional panels that show relevant MAM product
           conditionalPanel(
             condition = "input.mamorsam == 'Both SAM and MAM' & input.standard_mam_product == 'RUSF'",
             numericRangeInput("rusf.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Per-sachet cost of RUSF",
                                                            tags$span(class = "info-icon", "i", id = "rusfcost_info"))),
                               value = c(round(min(c(0.31,0.32,0.39,0.34,0.354,0.369)*1.15)*0.9,2), round(max(c(0.31,0.32,0.39,0.34,0.354,0.369)*1.15)*0.9,2)))
           ),
           conditionalPanel(
             condition = "input.mamorsam == 'Both SAM and MAM' & input.standard_mam_product == 'CSB'",
             numericRangeInput("csb.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Per-sachet cost of FBF",
                                                           tags$span(class = "info-icon", "i", id = "csbcost_info"))),
                               value = c(round(min(c(2.17,4.23,1.36)*1.15),2), round(max(c(2.17,4.23,1.36)*0.9),2)))
           ),

            numericRangeInput("drug.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Cost of drugs for outpatient visits",
                                                           tags$span(class = "info-icon", "i", id = "drugcost_info"))),
                              value = c(round(0.14,2), round(0.24,2))),
            
            numericRangeInput("labor.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Labor costs for outpatient visits",
                                                            tags$span(class = "info-icon", "i", id = "laborcost_info"))),
                              value = c(round(0.3,2), round(3.5,2))),
            
            numericRangeInput("nonlabor.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Other nonlabor costs associated with outpatient visits, per month",
                                                               tags$span(class = "info-icon", "i", id = "nonlaborcost_info"))),
                              value = c(round(0.9*(1.2),2), round(1.1*(1.2),2))),
            
            numericRangeInput("transport.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Transportation and food costs for caregivers to bring child to each visit",
                                                                tags$span(class = "info-icon", "i", id = "transportcost_info"))), 
                              value = c(round(0.67,2), round(1.41,2))),
            
            numericRangeInput("opportunity.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Income lost or other opportunity costs for caregiver to bring child to each visit", 
                                                                  tags$span(class = "info-icon", "i", id = "oppcost_info"))), 
                              value = c(round(0.06,2), round(2.01,2))),
            
            numericRangeInput("inp.hosp.cost",label=HTML(paste(shiny::icon('money-bill-alt'),"Cost per day for inpatient stay, excluding consumables like medication and supplies", 
                                                               tags$span(class = "info-icon", "i", id = "inphospcost_info"))),
                              value = c(round(0.9*34.72,2), round(1.1*34.72,2))),
           
            numericRangeInput("meds.supplies",label=HTML(paste(shiny::icon('money-bill-alt'),"Cost of medicines and supplies consumed by a typical inpatient case of acute malnutrition", 
                                                              tags$span(class = "info-icon", "i", id = "medsupplies_info"))),
                             value = c(round(0.9*(9.51+1.53),2), round(1.1*(9.51+1.53),2))),
                   ))       
            )),
              
              # column(width = 12,
              #        shinydashboard::box(width="100%", height = "50px", solidHeader = TRUE,
              #                            div(style="text-align:center",
              #                                actionButton(inputId = "submitInfo2", label = "Run model", icon("paper-plane"), 
              #                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              #                            )
              #            )
              #        ),
            
            #information text
           # bsTooltip(id = "mamorsam_info", 
            #          title = "MAM = Moderate Acute Malnutrition, SAM = Severe Acute Malnutrition", 
             #         placement = "right"),
            bsTooltip(id = "rutfcost_info", title = "Cost of one Ready-to-Use Therapeutic Food sachet", placement="right"),
            bsTooltip(id = "rusfcost_info", title = "Cost of one Ready-to-Use Supplementary Food sachet", placement="right"),
           bsTooltip(id = "csbcost_info", title = "Cost of one Fortified Blended Food sachet", placement="right"),
            bsTooltip(id = "drugcost_info", title = "Cost of all medications used during treatment", placement="right"),
            bsTooltip(id = "laborcost_info", title = "Labour expenses associated with admitting patients for outpation treatment. WHO-CHOICE 2010 estimates, adjusted for inflation, used for country specific choices", placement="right"),
            bsTooltip(id = "nonlaborcost_info", title = "Operational expenses not related to personnel (equipment, utilities, and other indirect care costs).", placement="right"),
            bsTooltip(id = "transportcost_info", title = "Expenses related to transporting patients, distributing therapeutic foods and any healthcare worker travel.", placement="right"),
            bsTooltip(id = "oppcost_info", title = "Economic value of income and productivity lost from caregivers attending to a hospitalized malnourished child rather than performing their usual work or activities", placement="right"),
            bsTooltip(id = "inphospcost_info", title = "Expenses associated with one-day of inpatient care. WHO-CHOICE 2010 estimates, adjusted for inflation, used for country specific choices", placement="right"),
           bsTooltip(id = "medsupplies_info", title = "Expenses associated with consumables, such as antibiotics, needle, syringe, nasogastric tube, during in-patient stay", placement="right"),
            
            bsTooltip(id = "recovprop_info", title = "Proportion of patients who successfully completed the treatment program and achieved target health outcomes.", placement="right"),
            bsTooltip(id = "propdefaulted_info", title = "Proportion of patients who miss # follow up appointments without medical recommendation and do not complete full program", placement="right"),
            bsTooltip(id = "deaprop_info", title = "Proportion of patients who died during the course of treatment due to malnutrition or related complications.", placement="right"),
            bsTooltip(id = "hospprop_info", title = "Proportion of patients whose condition required inpatient hospital admission during outpatient treatment due to medical complications.", placement="right"),
            bsTooltip(id = "nevrecovprop_info", title = "Proportion of patients who completed the full treatment course but did not achieve target outcomes or nutritional status improvements.", placement="right"),
            bsTooltip(id = "transfprop_info", title = "Proportion of patients who were transferred to other country/region/facility (what level?) to continue their care.", placement="right"),
            bsTooltip(id = "relapseprop_info", title = "Proportion of patients who returned to a malnourished state after initially recovering from treatment.", placement="left"),
            bsTooltip(id = "relapsetreat_info", title = "Proportion of patients who experienced relapse and subsequently returned to receive additional treatment for malnutrition.", placement="left"),
           
           bsTooltip(id = "recovprop_info_m", title = "Proportion of patients who successfully completed the treatment program and achieved target health outcomes.", placement="right"),
           bsTooltip(id = "propdefaulted_info_m", title = "Proportion of patients who miss # follow up appointments without medical recommendation and do not complete full program", placement="right"),
           bsTooltip(id = "deaprop_info_m", title = "Proportion of patients who died during the course of treatment due to malnutrition or related complications.", placement="right"),
           bsTooltip(id = "hospprop_info_m", title = "Proportion of patients whose condition required inpatient hospital admission during outpatient treatment due to medical complications.", placement="right"),
           bsTooltip(id = "nevrecovprop_info_m", title = "Proportion of patients who completed the full treatment course but did not achieve target outcomes or nutritional status improvements.", placement="right"),
           bsTooltip(id = "recov_change_info", title = "Proportion reduction in recovery among patients who relapsed, relative to patients who recover without relapsing. A value of 0.15 means a 15% reduction in the chance of recovery, based on published findings from Casez et al. (2022) in the DRC.",
                                                             placement="left")
          # bsTooltip(id = "deteriorate_prop_m_info", title = "Proportion of patients whose wasting worsened to a SAM state.", placement = "right")

           
    ) # Closes fluidPage
    ), # Closes tabItem
    
  # Results
  tabItem(tabName = "results",
          div(style="text-align:center",
              shinydashboard::box(width = "100%", status = 'primary', solidHeader = TRUE,
                                  h4(HTML(paste(icon("paper-plane"), "Generate evidence"))))
          ),
          fluidPage(fluidRow(
            column(width = 12,
                   shinydashboard::box(title = NULL, width = "100%", height = "60%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           textOutput("RestateObjective"),
                                           hr(),
                                           conditionalPanel(
                                             condition = "input.ph_objective == 'Reach more children with a fixed budget'",
                                             div(class = "info-box",
                                                 #h("Fixed Budget"),
                                                 p("The results will be given for the budget specified, as well as half and double that budget.")),
                                                textOutput("selected_currency"),
                                             br(),
                                             numericInput("budget.program",
                                                          label = span(tagList(icon('money-bill-alt'), 'Annual nutrition program budget allocated to treatment')),
                                                          min=0, value = 100000),
                                             br()
                                           ),
                                           conditionalPanel(
                                             condition = "input.ph_objective == 'Understand amount of RUTF needed for programming across protocols'",
                                             div(class = "info-box",
                                                 p("The model results will allow you to compare RUTF needs when treating SAM children per the Standard protocol (at the coverage indicated
                                                   on the previous tab) versus when treating SAM and MAM children per the simplified protocols.")),
                                             br(),
                                             sliderInput("mam_cov_obj3",
                                                          label = span(tagList(icon('sliders-h'), 'Please specify a range of treatment coverage (proportion) for MAM children.')),
                                                          min=0, max=1, value = c(0.1,0.25)),
                                             br()
                                           ),
                                           actionButton(inputId = "submitInfo2", label = "Run model", icon("paper-plane"), 
                                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                           hr(),
                                           HTML(paste("Note: Model must be re-run after any input changes.")))),
                   br()
                   
            ),
          column(width=12,
                 shinydashboard::box(title = NULL, width = "100%", height="100%", solidHeader = FALSE,
                                     div(style="text-align:center",
                                         h4(HTML(paste(icon('chart-area'), 'Results'))),
                                         hr(),
                                         h5(htmlOutput("Interpretation_Primary"))
                                     ),
                                     
                                     hr(),
                                     
                                     column(width=12,
                                            plotOutput("Primary_Graph"),
                                            hr(),
                                            div(style="text-align:center",
                                                downloadButton('downloadPlot_Primary_Graph','Download Plot', icon=icon("download"), width="100%", class = "btn-info",
                                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                     )
                 )
          ),
          conditionalPanel(
            condition = "input.ph_objective != 'Understand amount of RUTF needed for programming across protocols'",
          fluidRow(
            column(width=12,
                   shinydashboard::box(title = NULL, width="100%", height="100%",
                                       solidHeader = TRUE,
                                       div(style="text-align:center",
                                           h4("Relative to Current Approach")
                                       ),
                                       h5(htmlOutput("Relative_to_Current"))
                   )),
            
            infoBoxOutput("StandardBox"),
            infoBoxOutput("ComPASBox"),
            infoBoxOutput("OptiMABox"),
            conditionalPanel(
              condition = "input.mamorsam == 'Just SAM'",
            infoBoxOutput("MANGOBox"))
          )),
          conditionalPanel(
            condition = "input.ph_objective == 'Understand amount of RUTF needed for programming across protocols'",
            fluidRow(
              column(width=12,
                     shinydashboard::box(title = NULL, width="100%", height="100%",
                                         solidHeader = TRUE,
                                         div(style="text-align:center",
                                             h4("Relative Needs, by Scenario"),
                                             p("A protocol with a 75% would mean that the RUTF needs are 25% less than the Standard approach, while a protocol with 125% would mean that the RUTF needs are 25% more.")
                                         ),
                                         h5(htmlOutput("Relative_to_Current"))
                     )),
              infoBoxOutput("StandardBox"),
              infoBoxOutput("ComPASBox"),
              infoBoxOutput("OptiMABox"),
              infoBoxOutput("MANGOBox")
            ),
              fluidRow(
              column(width=12,
                     shinydashboard::box(title = NULL, width="100%", height="100%",
                                         solidHeader = TRUE,
                                         h5(htmlOutput("Relative_to_Current2"))
                     )),
              
              infoBoxOutput("StandardBox2"),
              infoBoxOutput("ComPASBox2"),
              infoBoxOutput("OptiMABox2"),
              infoBoxOutput("MANGOBox2"),
            )
          ),
          fluidRow(
            column(width=12,
                   shinydashboard::box(title = NULL, width="100%", height="100%",
                                       solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4(HTML(paste(textOutput("Primary_Table_Title"))))
                                       ),
                                       h5(textOutput("Note_uncertainty")),
                                       DTOutput('Primary_tbl'),
                                       hr(),
                                       div(style="text-align:center",
                                           downloadButton('downloadDataTable_Primary','Download Data Table', icon=icon("download"), width="100%", class = "btn-info",
                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   ))),
          
          ##### For secondary data downloads
          fluidRow(
            shinydashboard::box(title = NULL, width = "100%", height="100%", solidHeader = FALSE,
                                div(style="text-align:center",
                                    h4("Select any outcome(s) below to see additional results of interest.", width = "100%")
                                    #h4(actionButton("toggle_results1", "Additional Results", width = "100%"))
                                ),
            conditionalPanel(
              condition = "input.ph_objective == 'Improve cost efficiency of nutrition program'",
              selectInput("additional_outcomes_select",
                          label = span(tagList(icon('chart-area'), 'Outcomes:')),
                          choices = 
                                 list("Select from list" = "select",
                                      'Total Costs of Program' = 'total_programmatic_costs',
                                      'Total Cartons of Product Needed' = 'total_product_needs'),
                                 selected = "",
                                 multiple = FALSE),
            conditionalPanel(
              condition = "input.additional_outcomes_select == 'total_programmatic_costs'",
              column(width=12,
                     shinydashboard::box(title = NULL, width = "100%", height="100%", solidHeader = FALSE,
                                         div(style="text-align:center",
                                             h4(HTML(paste(icon('chart-area'), textOutput("Secondary_Results_Title_Total_Program_Costs"))))
                                         ),
                                         
                                         hr(),
                                         
                                         column(width=12,
                                                plotOutput("Secondary_Graph_Total_Program_Costs"),
                                                hr(),
                                                div(style="text-align:center",
                                                    downloadButton('downloadPlot_Secondary_Graph_Total_Program_Costs','Download Plot', icon=icon("download"), width="100%", class = "btn-info",
                                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                         )
                     )
              ),
              column(width=12,
                     shinydashboard::box(title = NULL, width="100%", height="100%",
                                         solidHeader = FALSE,
                                         div(style="text-align:center",
                                             h4(HTML(paste(textOutput("Secondary_Table_Title_Total_Program_Costs"))))
                                         ),
                                         DTOutput('Secondary_tbl_Total_Program_Costs'),
                                         hr(),
                                         div(style="text-align:center",
                                             downloadButton('downloadDataTable_Secondary_Total_Program_Costs','Download Data Table', icon=icon("download"), width="100%", class = "btn-info",
                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                     ))),
            conditionalPanel(
              condition = "input.additional_outcomes_select == 'total_product_needs'",
              column(width=12,
                     shinydashboard::box(title = NULL, width = "100%", height="100%", solidHeader = FALSE,
                                         div(style="text-align:center",
                                             h4(HTML(paste(icon('chart-area'), textOutput("Secondary_Results_Title_Total_Product_Needs"))))
                                         ),
                                         
                                         hr(),
                                         
                                         column(width=12,
                                                plotOutput("Secondary_Graph_Product_All"),
                                                hr(),
                                                div(style="text-align:center",
                                                    downloadButton('downloadPlot_Secondary_Product_All','Download Plot', icon=icon("download"), width="100%", class = "btn-info",
                                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                         )
                     )
              ),
              column(width=12,
                     shinydashboard::box(title = NULL, width="100%", height="100%",
                                         solidHeader = FALSE,
                                         div(style="text-align:center",
                                             h4(HTML(paste(textOutput("Secondary_Table_Title_Total_Product"))))
                                         ),
                                         DTOutput('Secondary_tbl_Total_Product'),
                                         hr(),
                                         div(style="text-align:center",
                                             downloadButton('downloadDataTable_Secondary_Total_Product','Download Data Table', icon=icon("download"), width="100%", class = "btn-info",
                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                     )))
            ),
            conditionalPanel(
              condition = "input.ph_objective == 'Reach more children with a fixed budget'",
              selectInput("additional_outcomes_select", 
                          label = span(tagList(icon('chart-area'), 'Outcomes:')),
                          choices = 
                            list("Select from list" = "select",
                                 'Number of Sachets per Child Treated' = 'sachets_treated_child'), 
                                 #'Number of Children Treated' = 'treated_children'),
                          selected = "",
                          multiple = FALSE),
              conditionalPanel(
                condition = "input.additional_outcomes_select == 'sachets_treated_child'",
                column(width=12,
                       shinydashboard::box(title = NULL, width = "100%", height="100%", solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(icon('chart-area'), textOutput("Secondary_Results_Title_Children_Treated"))))
                                           ),
                                           
                                           hr(),
                                           
                                           column(width=12,
                                                  plotOutput("Secondary_Graph_Children_Treated"),
                                                  hr(),
                                                  div(style="text-align:center",
                                                      downloadButton('downloadPlot_Secondary_Graph_Children_Treated','Download Plot', icon=icon("download"), width="100%", class = "btn-info",
                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                           )
                       )
                ),
                column(width=12,
                       shinydashboard::box(title = NULL, width="100%", height="100%",
                                           solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(textOutput("Secondary_Table_Title_Children_Treated"))))
                                           ),
                                           DTOutput('Secondary_tbl_Children_Treated'),
                                           hr(),
                                           div(style="text-align:center",
                                               downloadButton('downloadDataTable_Children_Treated','Download Data Table', icon=icon("download"), width="100%", class = "btn-info",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       )))),
            conditionalPanel(
              condition = "input.ph_objective == 'Understand amount of RUTF needed for programming across protocols'",
              selectInput("additional_outcomes_select", 
                          label = span(tagList(icon('chart-area'), 'Outcomes:')),
                          choices = 
                            list("Select from list" = "select",
                                 'RUTF Needs for MAM versus SAM Treatment' = 'rutf_treated_child',
                                 'RUTF per Recovery' = 'rutf_recovered_child'), 
                          selected = "",
                          multiple = FALSE),
              conditionalPanel(
                condition = "input.additional_outcomes_select == 'rutf_treated_child'",
                column(width=12,
                       shinydashboard::box(title = NULL, width = "100%", height="100%", solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(icon('chart-area'), textOutput("Secondary_Results_Title_Children_Treated_RUTF"))))
                                           ),
                                           
                                           hr(),
                                           
                                           column(width=12,
                                                  plotOutput("Secondary_Graph_Children_Treated_RUTF"),
                                                  hr(),
                                                  div(style="text-align:center",
                                                      downloadButton('downloadPlot_Secondary_Graph_Children_Treated_RUTF','Download Plot', icon=icon("download"), width="100%", class = "btn-info",
                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                           )
                       )
                ),
                column(width=12,
                       shinydashboard::box(title = NULL, width="100%", height="100%",
                                           solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(textOutput("Secondary_Table_Title_Children_Treated_RUTF"))))
                                           ),
                                           DTOutput('Secondary_tbl_Children_Treated_RUTF'),
                                           hr(),
                                           div(style="text-align:center",
                                               downloadButton('downloadDataTable_Children_Treated_RUTF','Download Data Table', icon=icon("download"), width="100%", class = "btn-info",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       ))),
              conditionalPanel(
                condition = "input.additional_outcomes_select == 'rutf_recovered_child'",
                column(width=12,
                       shinydashboard::box(title = NULL, width = "100%", height="100%", solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(icon('chart-area'), textOutput("Secondary_Results_Title_Children_Recovered_RUTF"))))
                                           ),
                                           
                                           hr(),
                                           
                                           column(width=12,
                                                  plotOutput("Secondary_Graph_Children_Recovered_RUTF"),
                                                  hr(),
                                                  div(style="text-align:center",
                                                      downloadButton('downloadPlot_Secondary_Graph_Children_Recovered_RUTF','Download Plot', icon=icon("download"), width="100%", class = "btn-info",
                                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                           )
                       )
                ),
                column(width=12,
                       shinydashboard::box(title = NULL, width="100%", height="100%",
                                           solidHeader = FALSE,
                                           div(style="text-align:center",
                                               h4(HTML(paste(textOutput("Secondary_Table_Title_Children_Recovered_RUTF"))))
                                           ),
                                           DTOutput('Secondary_tbl_Children_Recovered_RUTF'),
                                           hr(),
                                           div(style="text-align:center",
                                               downloadButton('downloadDataTable_Children_Recovered_RUTF','Download Data Table', icon=icon("download"), width="100%", class = "btn-info",
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                       ))),
              
              ),
            )),
          fluidRow(
            column(width = 12,
                   shinydashboard::box(title = NULL, width = "100%", height = "60%", solidHeader = FALSE,
                                       div(style="text-align:center",
                                           h4("Use the side menu to navigate back and make changes. You can change your objective to view different types of evidence, 
                                              or you can update the parameters for your context to observe how it impacts the evidence.")
                                       )
                   )
            )
          ))#)
          
          
  )),
    
    # Introduction
    tabItem(tabName = "introduction",
            div(style="text-align:center",
                shinydashboard::box(width = "100%", status = "primary", solidHeader = TRUE,
                    h4(HTML(paste(icon("home"), "Introduction"))))
            ),

            fluidRow(
              column(width = 12,
                     shinydashboard::box(title = NULL, width = "100%", height = "60%", solidHeader = FALSE,
                         helpText(HTML(paste0(
                           # "This dashboard is intended to support decision-makers in exploring
                           # different approaches to treating acute malnutrition. 
                           #There is ongoing field evidence generation around the comparative effectiveness of the different protocols. 
                           #"Limited budgets, supply chain issues, and increased demand challenge nutrition programs globally. 
                           "'Simplified' dosing protocols that reduce the quantity of ready-to-use
                           foods needed to treat severe acute malnutrition (SAM) and/or moderate acute malnutrition (MAM)
                           have been introduced in recent years. Navigating all of the research evidence may be overwhelming. Or existing evidence 
                           may not feel relevant to your particular context.
                           We have developed a model to support stakeholders who may be considering
                           the cost and health implications of different protocols for acute malnutrition, accounting for context-specific needs." 
                           #Users of the app can enter country-specific information to tailor model outputs based on a public health objective."
                         ))),
                         hr(),
                         h4(HTML(paste(icon("user-md"), "Define an objective for which you would like evidence."))),
                         tags$ul(
                           tags$li("Specify a programmatic goal."),
                           tags$li("Identify the treatment approach currently being used, and access more information about alternative protocols.")
                         ),
                         h4(HTML(paste(icon("tachometer-alt"), "Enter context-specific information"))),
                         tags$ul(
                           tags$li("Input available costs and program outcomes for your setting."),
                           tags$li("Review model assumptions.")
                         ),
                         h4(HTML(paste(icon("paper-plane"), "Generate evidence"))),
                         tags$ul(
                           tags$li("Compare outcomes across treatment protocols."),
                           tags$li("Investigate alternative assumptions.")
                         ),
                         h4(HTML(paste(icon("comments"), "Complete survey"))),
                         tags$ul(
                           tags$li("Take a brief survey and provide feedback about your experience using the dashboard.")
                         )
                     )
              )
               # ,
              #
              # column(width = 7,
              #        box(title = "Select + to display the animated demonstration", width = "100%", height = "60%", solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE,
              #            hr(),
              #            img(src="demo-cropped.gif", align = "left", width='100%')
              #        )
              # )
              )
            
    ),
  
  #Survey Tab
  tabItem(tabName = "survey",
                  div(style="text-align:center",
                      shinydashboard::box(width = "100%", status = 'primary', solidHeader = TRUE,
                                          h4(HTML(paste(icon("paper-plane"), "Survey Information"))))
                  ),
                  fluidRow(
                    column(width = 12,
                           shinydashboard::box(title = NULL, width = "100%", height = "60%", solidHeader = FALSE,
                                               div(style="text-align:left",
              p("We ask you to complete the survey below once you have finished exploring the dashboard. Your feedback will help us optimise usability and outputs."),
              br(),
              p("The survey takes approximately 10 minutes to complete. To allow for linking data from your dashboard experience with the survey responses, 
                  we ask that you copy the code below and enter it at the top of the survey. This code is unique to you and not associated with any identifying 
                  information to maintain anonymity of your data. Access the survey using the link below."),
              br()
              ),
              div(style="text-align:center",
              h3(textOutput("user_anonymous_id"), style = "color: #337ab7; font-weight: bold;"),
              hr()#,
             # p(tags$a(href="https://redcap.am.lshtm.ac.uk/redcap/surveys/?s=PNEYR8EDHKN4LAKN"  , icon("file"), target="_blank", "Link to survey",style="color:black;font-size:15px;font-style:bold"))
              )
          )
                    )
                  )
          
) # Closes tabsetPanel
) # Closes tabitems
) # Closes dashboardBody



# Put them together into a dashboardPage
ui=dashboardPage(
  header,
  sidebar,
  body
)