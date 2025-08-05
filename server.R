server = shinyServer(function(input, output, session) {
  
  
  toListen <- reactive({
    list(input$Recov_prop,input$Hosp_prop,input$NevRecov_prop,input$Def_prop,input$Dea_prop)
  })
  
  #telemetry set up----

# #print("Attempting to connect to database...")
# conn <- dbConnect(
#   Postgres(),
#   host = "",
#   port = ,
#   dbname = "",
#   user = "",
#   password = "",
#   sslmode = "require"
# )
# #print("Database connection established successfully")
# 
# # Test connection with a simple query
# test_result <- dbGetQuery(conn, "SELECT 1 AS test")
# print(paste("Connection test result:", test_result$test))
# 
# # Get IP and generate username
# user_ip <- get_user_ip(session)
# user_anonymous_id <- generate_anonymous_username(user_ip)
# 
# # Make username available to UI
# output$user_anonymous_id <- renderText({
#   user_anonymous_id
# })
# 
# # Manual logging function with improved debugging
# log_to_database <- function(event_type, event_data = NULL) {
#   if (!is.null(conn) && dbIsValid(conn)) {
#     tryCatch({
# 
#       # Convert event_data to JSON safely
#       event_data_json <- '{}'
#       if (!is.null(event_data)) {
#         event_data_json <- tryCatch({
#           jsonlite::toJSON(event_data, auto_unbox = TRUE)
#         }, error = function(e) {
#           print(paste("JSON conversion error:", e$message))
#           return('{"error": "Failed to convert to JSON"}')
#         })
#       }
# 
#       # Create data frame for insertion
#       data <- data.frame(
#         session_id = session$token,
#         timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
#         event_type = event_type,
#         event_data = event_data_json,
#         username = user_anonymous_id,
#         stringsAsFactors = FALSE
#       )
# 
#       # Print data for debugging
#       print("Data to be inserted:")
#       print(data)
# 
#       # Insert into database
#       result <- dbAppendTable(conn, "telemetry_events", data)
#       print(paste("Database insert result:", result))
#     }, error = function(e) {
#       print(paste("Logging error:", e$message))
#     })
#   } else {
#     print("Database connection is not valid")
#   }
# }

  
  #dashboard starts----
  observeEvent(toListen(), {
    updateSliderInput(session, "Transf_prop", value = 1-sum(input$Recov_prop,input$Hosp_prop,input$NevRecov_prop,input$Def_prop,input$Dea_prop))
  })
  
  output$DosingDetailsTable <- renderTable(
    data.frame(Protocol = c('Standard','MANGO','ComPAS','OptiMA'),
               Details = c('Current national guidelines','Standard RUTF dose for Weeks 1-2 1 sachet/day RUTF for <7kg; 
                             \n2 sachets/day RUTF for >7kg from Week 3 - discharge',
                           'MUAC < 115 mm, two RUTF sachets per day, 
                             \n115 ≤ MUAC <125 mm, one RUTF sachet per day',
                           'MUAC < 115 or oedema: 170-200 kcal/kg/day RUTF 
                            \n115 < MUAC < 120: 125-190 kcal/kg/day RUTF
                            \n120 < MUAC < 125: 50-166 kcal/kg/day RUTF')),
    digits = 3,width="100%")
  
  output$assumptions_table <- renderTable(
    data.frame(Protocol = c(rep("ComPAS",2),rep("MANGO",2),rep("OptiMA",2),rep("Standard",2)),
               Population = rep(c("MAM","SAM"),length(c("ComPAS","MANGO","OptiMA","Standard"))),
               Recovered = c(0.694,0.519, #ComPAS
                             0.500,0.586, #MANGO
                             0.681,0.497, #OptiMA
                             0.783,0.529), #Standard
               Defaulted = c(0.061,0.095, #ComPAS
                             0.135,0.094, #MANGO
                             0.058,0.113, #OptiMA
                             0.029,0.153), #Standard
               Died = c(0.013,0.030, #ComPAS
                        0,0.008, #MANGO
                        0.021,0.056, #OptiMA
                        0.031,0.056), #Standard
               Hospitalized = c(0,0, #ComPAS
                                0.201,0.172, #MANGO
                                0,0, #OptiMA
                                0,0), #Standard
               Never_recovered = c(0.121,0.074, #ComPAS
                                   0.120,0.141, #MANGO
                                   0.132,0.080, #OptiMA
                                   0.118,0.067)) #Standard #Reference = c("","Kangas et al., PLOS Medicine, 2019","","")
  )
  
  output$selected_country <- renderText({
    paste("The information below is preset to reflect published values from ", input$countryChoice, " (e.g., WHO Choice cost data when available) or from countries in the 
      sub-Saharan Africa region, where studies on the simplified protocols have been done. Please update to more accurate programmatic inputs based on your own data wherever possible and then proceed to the Generate Evidence section using the side menu.")
  })
  
  output$selected_currency <- renderText({
    paste("Please enter the budget in the currency specified in the previous tab (", ifelse(!(input$Currency %in% "Other"), input$Currency, input$OtherCurrency), ").")
  })
  
  output$cost1 <- renderPrint(input$my_id)
  output$cost2 <- renderPrint(input$my_id)
  output$cost3 <- renderPrint(input$my_id)
  output$cost4 <- renderPrint(input$my_id)
  output$cost5 <- renderPrint(input$my_id)
  
  output$RestateObjective <- renderText({
    paste("Your objective: ", input$ph_objective)
  })
  
  output$value <- renderText({ input$plotvars })
  
  #text <- reactiveVal()
  #output$txt <- renderText({ text() })
  
  
  observeEvent(input$submitInfo, {
    updateTabItems(session = session, inputId = "inTabset", selected = "objective")
  })
  
  observeEvent(input$submitInfo2,
               ignoreNULL = T, {
                 
                 output$Primary_tbl <- renderDT({NULL})
                 output$Secondary_tbl <- renderDT({NULL})
                 
                 #CE and limited supply 
                 if(exists("ci.total.cost.rec.all_CLEANED2")) rm(ci.total.cost.rec.all_CLEANED2)
                 if(exists("ci.total.cost.rec.all_CLEANED")) rm(ci.total.cost.rec.all_CLEANED)
                 
                 
                 #CE and limited supply 
                 # if(exists("ci.sachet.all_CLEANED2")) rm(ci.sachet.all_CLEANED2)
                 # if(exists("ci.sachet.all_CLEANED")) rm(ci.sachet.all_CLEANED)
                 # if(exists("ci.sachet_CLEANED2")) rm(ci.sachet_CLEANED2)
                 # if(exists("ci.sachet_CLEANED3")) rm(ci.sachet_CLEANED3)
                 # if(exists("ci.sachet_CLEANED")) rm(ci.sachet_CLEANED)
                 # if(exists("ci.pop.sachet_CLEANED2")) rm(ci.pop.sachet_CLEANED2)
                 # if(exists("ci.pop.sachet_CLEANED3")) rm(ci.pop.sachet_CLEANED3)
                 # if(exists("ci.pop.sachet.all_CLEANED")) rm(ci.pop.sachet.all_CLEANED)
                 # if(exists("ci.pop.sachet_CLEANED")) rm(ci.pop.sachet_CLEANED)
                 # 
                
                 #fixed budged
                 # vars_to_clear <- c("cost.sch_asgiven", "cost.sch_half", "cost.sch_double", 
                 #                    "cost.sch", "costm", "costmnumber", "pop.sachet", "pop.sachet2",
                 #                    "ci.sachet.all", "ci.sachet", "ci.pop.sachet.all", "ci.pop.sachet",
                 #                    "ci.sachet.all_CLEANED", "ci.sachet.all_CLEANED2", 
                 #                    "ci.sachet_CLEANED", "ci.sachet_CLEANED2", "ci.sachet_CLEANED3",
                 #                    "ci.pop.sachet.all_CLEANED", "ci.pop.sachet.all_CLEANED2",
                 #                    "ci.pop.sachet_CLEANED", "ci.pop.sachet_CLEANED2", "ci.pop.sachet_CLEANED3")
                 # 
                 # for(var in vars_to_clear) {
                 #   if(exists(var, envir = .GlobalEnv)) {
                 #     rm(list = var, envir = .GlobalEnv)
                 #   }
                 # }
                 # 
                 # gc()
                 
                 showModal(
                   modalDialog(
                     title = "Model Running",
                     div(
                       style = "text-align: center;",
                       h4("Please wait while the model is running..."),
                       br(),
                       
                       div(
                         class = "progress",
                         div(
                           class = "progress-bar progress-bar-striped active", 
                           role = "progressbar",
                           style = "width: 100%"
                         )
                       ),
                       
                       br(),
                       p("This may take a while.")
                     ),
                     footer = NULL,
                     size = "m",
                     easyClose = FALSE
                   )
                 )
      
                 updateSelectInput(session=session, inputId="additional_outcomes_select", selected = "select")
                 if (input$approach_based_on %in% "Standard"){
                 df_costm = f_wrapper(
                   sev.wasted = input$n_pop,
                   mod.wasted = input$n_pop_m,
                   cov.s = input$cov_s,
                   cov.m = ifelse(input$mamorsam %in% "Just SAM", 0, input$cov_m),
                   # Weight in kg at admission
                   ## severe wasting
                   weight.adm.s = input$weight.adm.s,
                   muac.s = input$muac.adm.s,
                   treatDist.s = c(input$Recov_prop,input$Def_prop,input$Dea_prop,
                                 input$Hosp_prop,input$NevRecov_prop,input$Transf_prop,
                                 input$relapse_prop[1],input$relapse_prop[2],
                                 input$relapse_prop_sam_to_mam[1],input$relapse_prop_sam_to_mam[2],input$relapse_treat_prop,input$recov_change), #additionalRelapseParams
                   treatDist.m = c(input$Recov_prop_m,input$Def_prop_m,input$Dea_prop_m,
                                 input$Hosp_prop_m,input$NevRecov_prop_m,
                                 input$deteriorate_prop_m[1],input$deteriorate_prop_m[2],
                                 input$relapse_prop_mam_to_mam[1],input$relapse_prop_mam_to_mam[2],input$relapse_prop_mam_to_sam[1],input$relapse_prop_mam_to_sam[2], #additionalRelapseParams
                                 input$relapse_treat_prop_mam,input$recov_change_mam), #additionalRelapseParams
                   OptimaDist = c(0.681, 0.058, 0.021, 0, 0.132,
                                   0.497, 0.113, 0.056, 0, 0.08,
                                  0.111,0.111,0.01,0.01,0.24,0.24,0,0.1),
                   CompasDist = c(0.694, 0.061, 0.013, 0, 0.121,
                                   0.519, 0.095, 0.074, 0.03, 0,
                                  0.03,0.124,0.174,0.351,0.212,0.318,0.001,0.027),
                   MangoDist = c(0.5000000, 0.1350365, 0.1204380, 0.0000000, 0.2007299, 
                                 0.5859375, 0.0937500, 0.1406250, 0.0078125, 0.1718750,
                                 0.024,0.024,0.0725,0.226,0.1,0.3,0,0.1),
                   startingPoint = 0,
                   costDist.min=(1/input$conv_rate)*c(input$rutf.cost[1],input$rusf.cost[1],
                                  input$drug.cost[1],input$labor.cost[1],
                                  input$nonlabor.cost[1],input$transport.cost[1],
                                  input$opportunity.cost[1],input$inp.hosp.cost[1],input$meds.supplies[1],input$csb.cost[1]
                   ),
                   costDist.max=(1/input$conv_rate)*c(input$rutf.cost[2],input$rusf.cost[2],
                                  input$drug.cost[2],input$labor.cost[2],
                                  input$nonlabor.cost[2],input$transport.cost[2],
                                  input$opportunity.cost[2],input$inp.hosp.cost[2],input$meds.supplies[2],input$csb.cost[2]
                   ),
                   country.name=ifelse(input$countryChoice %in% "Select from list", "Ethiopia", input$countryChoice),
                   reducedRecovery=input$recov_change)}
                 
                 else if (input$approach_based_on %in% "OptiMA"){
                   df_costm = f_wrapper(
                     sev.wasted = input$n_pop,
                     mod.wasted = input$n_pop_m,
                     cov.s = input$cov_s,
                     cov.m = ifelse(input$mamorsam %in% "Just SAM", 0, input$cov_m),
                     # Weight in kg at admission
                     ## severe wasting
                     weight.adm.s = input$weight.adm.s,
                     muac.s = input$muac.adm.s,
                     OptimaDist = c(input$Recov_prop,input$Def_prop,input$Dea_prop,input$Hosp_prop,input$NevRecov_prop,
                                    input$Recov_prop_m,input$Def_prop_m,input$Dea_prop_m,input$Hosp_prop_m,input$NevRecov_prop_m,
                                    input$relapse_prop[1],input$relapse_prop[2], #additionalRelapseParams
                                    input$relapse_prop_sam_to_mam[1],input$relapse_prop_sam_to_mam[2], #additionalRelapseParams
                                    input$relapse_prop_mam_to_mam[1],input$relapse_prop_mam_to_mam[2],input$relapse_prop_mam_to_sam[1],input$relapse_prop_mam_to_sam[2], #additionalRelapseParams
                                    input$relapse_treat_prop,input$recov_change,input$relapse_treat_prop_mam,input$recov_change_mam), #additionalRelapseParams
                     treatDist.s = c(0.783,0.029,0.031,0,0.118, 
                                     1-sum(c(0.783,0.029,0.031,0,0.118)),
                                     0.015,0.136,0.0725,0.226,input$relapse_treat_prop),
                     treatDist.m = c(0.529,0.153,0.056,0,0.067,
                                     input$deteriorate_prop_m[1],input$deteriorate_prop_m[2],
                                     0.1,0.3,0,0.1),
                     CompasDist = c(0.694, 0.061, 0.013, 0, 0.121,
                                    0.519, 0.095, 0.074, 0.03, 0,
                                    0.03,0.124,0.174,0.351,0.212,0.318,0.001,0.027),
                     MangoDist = c(0.5000000, 0.1350365, 0.1204380, 0.0000000, 0.2007299, 
                                   0.5859375, 0.0937500, 0.1406250, 0.0078125, 0.1718750,
                                   0.024,0.024,0.0725,0.226,0.1,0.3,0,0.1),
                     startingPoint = 0,
                     costDist.min=(1/input$conv_rate)*c(input$rutf.cost[1],input$rusf.cost[1],
                                    input$drug.cost[1],input$labor.cost[1],
                                    input$nonlabor.cost[1],input$transport.cost[1],
                                    input$opportunity.cost[1],input$inp.hosp.cost[1],input$meds.supplies[1],input$csb.cost[1]
                     ),
                     costDist.max=(1/input$conv_rate)*c(input$rutf.cost[2],input$rusf.cost[2],
                                    input$drug.cost[2],input$labor.cost[2],
                                    input$nonlabor.cost[2],input$transport.cost[2],
                                    input$opportunity.cost[2],input$inp.hosp.cost[2],input$meds.supplies[2],input$csb.cost[2]
                     ),
                     country.name=ifelse(input$countryChoice %in% "Select from list", "Ethiopia", input$countryChoice),
                     reducedRecovery=input$recov_change)}
                 
                 else if (input$approach_based_on %in% "MANGO"){
                   df_costm = f_wrapper(
                     sev.wasted = input$n_pop,
                     mod.wasted = input$n_pop_m,
                     cov.s = input$cov_s,
                     cov.m = ifelse(input$mamorsam %in% "Just SAM", 0, input$cov_m),
                     # Weight in kg at admission
                     ## severe wasting
                     weight.adm.s = input$weight.adm.s,
                     muac.s = input$muac.adm.s,
                     treatDist.s = c(0.783,0.029,0.031,0,0.118, 
                                     1-sum(c(0.783,0.029,0.031,0,0.118)),
                                     0.015,0.136,0.0725,0.226,input$relapse_treat_prop),
                     treatDist.m = c(0.529,0.153,0.056,0,0.067,
                                     input$deteriorate_prop_m[1],input$deteriorate_prop_m[2],
                                     0.1,0.3,0,0.1),
                     OptimaDist = c(0.681, 0.058, 0.021, 0, 0.132,
                                    0.497, 0.113, 0.056, 0, 0.08,
                                    0.111,0.111,0.01,0.01,0.24,0.24,0,0.1),
                     CompasDist = c(0.694, 0.061, 0.013, 0, 0.121,
                                    0.519, 0.095, 0.074, 0.03, 0,
                                    0.03,0.124,0.174,0.351,0.212,0.318,0.001,0.027),
                     MangoDist = c(input$Recov_prop,input$Def_prop,input$Dea_prop,input$Hosp_prop,input$NevRecov_prop,
                                    input$Recov_prop_m,input$Def_prop_m,input$Dea_prop_m,input$Hosp_prop_m,input$NevRecov_prop_m,
                                   input$relapse_prop[1],input$relapse_prop[2], #additionalRelapseParams
                                   input$relapse_prop_sam_to_mam[1],input$relapse_prop_sam_to_mam[2], #additionalRelapseParams
                                   input$relapse_prop_mam_to_mam[1],input$relapse_prop_mam_to_mam[2],input$relapse_prop_mam_to_sam[1],input$relapse_prop_mam_to_sam[2], #additionalRelapseParams
                                   input$relapse_treat_prop,input$recov_change,input$relapse_treat_prop_mam,input$recov_change_mam), #additionalRelapseParams
                     startingPoint = 0,
                     costDist.min=(1/input$conv_rate)*c(input$rutf.cost[1],input$rusf.cost[1],
                                    input$drug.cost[1],input$labor.cost[1],
                                    input$nonlabor.cost[1],input$transport.cost[1],
                                    input$opportunity.cost[1],input$inp.hosp.cost[1],input$meds.supplies[1],input$csb.cost[1]
                     ),
                     costDist.max=(1/input$conv_rate)*c(input$rutf.cost[2],input$rusf.cost[2],
                                    input$drug.cost[2],input$labor.cost[2],
                                    input$nonlabor.cost[2],input$transport.cost[2],
                                    input$opportunity.cost[2],input$inp.hosp.cost[2],input$meds.supplies[2],input$csb.cost[2]
                     ),
                     country.name=ifelse(input$countryChoice %in% "Select from list", "Ethiopia", input$countryChoice),
                     reducedRecovery=input$recov_change)}
                 
                 else if (input$approach_based_on %in% "ComPAS"){
                   df_costm = f_wrapper(
                     sev.wasted = input$n_pop,
                     mod.wasted = input$n_pop_m,
                     cov.s = input$cov_s,
                     cov.m = ifelse(input$mamorsam %in% "Just SAM", 0, input$cov_m),
                     # Weight in kg at admission
                     ## severe wasting
                     weight.adm.s = input$weight.adm.s,
                     muac.s = input$muac.adm.s,
                     treatDist.s = c(0.783,0.029,0.031,0,0.118, 
                                     1-sum(c(0.783,0.029,0.031,0,0.118)),
                                     0.015,0.136,0.0725,0.226,input$relapse_treat_prop),
                     treatDist.m = c(0.529,0.153,0.056,0,0.067,
                                     input$deteriorate_prop_m[1],input$deteriorate_prop_m[2],
                                     0.1,0.3,0,0.1),
                     OptimaDist = c(0.681, 0.058, 0.021, 0, 0.132,
                                    0.497, 0.113, 0.056, 0, 0.08,
                                    0.111,0.111,0.01,0.01,0.24,0.24,0,0.1),
                     CompasDist = c(input$Recov_prop,input$Def_prop,input$Dea_prop,input$Hosp_prop,input$NevRecov_prop,
                                    input$Recov_prop_m,input$Def_prop_m,input$Dea_prop_m,input$Hosp_prop_m,input$NevRecov_prop_m,
                                    input$relapse_prop[1],input$relapse_prop[2], #additionalRelapseParams
                                    input$relapse_prop_sam_to_mam[1],input$relapse_prop_sam_to_mam[2], #additionalRelapseParams
                                    input$relapse_prop_mam_to_mam[1],input$relapse_prop_mam_to_mam[2],input$relapse_prop_mam_to_sam[1],input$relapse_prop_mam_to_sam[2], #additionalRelapseParams
                                    input$relapse_treat_prop,input$recov_change,input$relapse_treat_prop_mam,input$recov_change_mam), #additionalRelapseParams
                     MangoDist <- c(0.5000000, 0.1350365, 0.1204380, 0.0000000, 0.2007299, 
                                    0.5859375, 0.0937500, 0.1406250, 0.0078125, 0.1718750,
                                    0.024,0.024,0.0725,0.226,0.1,0.3,0,0.1),
                     startingPoint = 0,
                     costDist.min=(1/input$conv_rate)*c(input$rutf.cost[1],input$rusf.cost[1],
                                    input$drug.cost[1],input$labor.cost[1],
                                    input$nonlabor.cost[1],input$transport.cost[1],
                                    input$opportunity.cost[1],input$inp.hosp.cost[1],input$meds.supplies[1],input$csb.cost[1]
                     ),
                     costDist.max=(1/input$conv_rate)*c(input$rutf.cost[2],input$rusf.cost[2],
                                    input$drug.cost[2],input$labor.cost[2],
                                    input$nonlabor.cost[2],input$transport.cost[2],
                                    input$opportunity.cost[2],input$inp.hosp.cost[2],input$meds.supplies[2],input$csb.cost[2]
                     ),
                     country.name=ifelse(input$countryChoice %in% "Select from list", "Ethiopia", input$countryChoice),
                     reducedRecovery=input$recov_change)}
                 
                 substrRight <- function(x, n){
                   substr(x, nchar(x)-n+1, nchar(x))
                 }
                 sev.wasted <- input$n_pop
                 mod.wasted <- input$n_pop_m
                 cov.s <- input$cov_s
                 cov.m <- ifelse(input$mamorsam %in% "Just SAM", 0, input$cov_m)
                 pop <- sev.wasted*cov.s
                 pop.m <- mod.wasted * cov.m
          
                 df_costm$totalcost.s.csb <- df_costm$totalcost.s
          costm <- data.table(melt(df_costm,measure.vars=c("totalcost.s","totalcost.s.csb","totalcost.s1","totalcost.s2","totalcost.s3",
                                                            "totalcost.m","totalcost.m1","totalcost.m2","totalcost.m3",
                                                            "totalcost.m.csb"),value.name = "value"))
       
          costm$variable2 <- ifelse(costm$variable %in% c("totalcost.s","totalcost.s.csb","totalcost.m","totalcost.m.csb"), "Standard",
                                    ifelse(costm$variable %in% c("totalcost.s1","totalcost.m1"), "OptiMA",
                                           ifelse(costm$variable %in% c("totalcost.s2","totalcost.m2"),"ComPAS","MANGO")))

          costm$variable22 <- ifelse(costm$variable %in% c("totalcost.s","totalcost.m"), "Standard",
                                     ifelse(costm$variable %in% c("totalcost.s.csb","totalcost.m.csb"),"Standard - CSB",
                                            ifelse(costm$variable %in% c("totalcost.s1","totalcost.m1"), "OptiMA",
                                                   ifelse(costm$variable %in% c("totalcost.s2","totalcost.m2"),"ComPAS","MANGO"))))


          substrRight <- function(x, n){
            substr(x, nchar(x)-n+1, nchar(x))
          }


          costm[,variable4 := paste(round(weight.adm.s), "kg")] # weight at admission
          costm[,variable5 := paste(round(muac.s), "mm")] #muac at admission
          costm$variable6 <- ifelse(costm$variable %in% c("totalcost.s","totalcost.s.csb","totalcost.s1",
                                                          "totalcost.s2","totalcost.s3"),"Severe Wasting",
                                    ifelse(costm$variable %in% c("totalcost.m",
                                                                 "totalcost.m1","totalcost.m2","totalcost.m3"),
                                           "Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB"))


          costm$cost.per.child  <- ifelse(costm$variable2=="Standard" &
                                            costm$variable6 == "Severe Wasting" ,costm$value/(pop*costm$alpha.s + pop*costm$alpha2.s*costm$phi.s +
                                                                                                costm$cov*costm$alpha.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam +
                                                                                                costm$cov*costm$phi.s*costm$alpha2.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam+
                                                                                                costm$cov*costm$alpha.s.prms*pop*costm$alpha.m*costm$sigma.m.to.sam +
                                                                                                costm$cov*costm$phi.s*costm$alpha2.s.prms*pop*costm$alpha.m*costm$sigma.m.to.sam) ,

                                          ifelse(costm$variable2=="Standard" &
                                                   costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB") ,costm$value/(pop*costm$alpha.m + pop*costm$alpha2.m*costm$phi.m +
                                                                                                                                                   costm$cov*costm$alpha.m.prmm*pop*costm$alpha.m*costm$sigma.m.to.mam +
                                                                                                                                                   costm$cov*costm$phi.m*costm$alpha2.m.prmm*pop*costm$alpha.m*costm$sigma.m.to.mam+
                                                                                                                                                   costm$cov*costm$alpha.m.prsm*pop*costm$alpha.s*costm$sigma.s.to.mam +
                                                                                                                                                   costm$cov*costm$phi.m*costm$alpha2.m.prsm*pop*costm$alpha.s*costm$sigma.s.to.mam) ,

                                                 ifelse(costm$variable2=="OptiMA"&
                                                          costm$variable6 == "Severe Wasting" ,costm$value/(pop*costm$alpha.s1 + pop*costm$alpha2.s1*costm$phi.s1  +
                                                                                                              costm$cov*costm$alpha.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                              costm$cov*costm$phi.s1*costm$alpha2.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                              costm$cov*costm$alpha.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam +
                                                                                                              costm$cov*costm$phi.s1*costm$alpha2.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam),
                                                        ifelse(costm$variable2=="OptiMA" &
                                                                 costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,costm$value/(pop*costm$alpha.m1 + pop*costm$alpha2.m1*costm$phi.m1 +
                                                                                                                                   costm$cov*costm$alpha.m1.prmm*pop*costm$alpha.m1*costm$sigma.m1.to.mam +
                                                                                                                                   costm$cov*costm$phi.m1*costm$alpha2.m1.prmm*pop*costm$alpha.m1*costm$sigma.m1.to.mam+
                                                                                                                                   costm$cov*costm$alpha.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam +
                                                                                                                                   costm$cov*costm$phi.m1*costm$alpha2.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam) ,

                                                               ifelse(costm$variable2=="ComPAS"&
                                                                        costm$variable6 == "Severe Wasting",costm$value/(pop*costm$alpha.s2 + pop*costm$alpha2.s2*costm$phi.s2  +
                                                                                                                           costm$cov*costm$alpha.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                           costm$cov*costm$phi.s2*costm$alpha2.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                           costm$cov*costm$alpha.s2.prms*pop*costm$alpha.m2*costm$sigma.m2.to.sam +
                                                                                                                           costm$cov*costm$phi.s2*costm$alpha2.s2.prms*pop*costm$alpha.m2*costm$sigma.m2.to.sam),

                                                                      ifelse(costm$variable2=="ComPAS" &
                                                                               costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,costm$value/(pop*costm$alpha.m2 + pop*costm$alpha2.m2*costm$phi.m2 +
                                                                                                                                                 costm$cov*costm$alpha.m2.prmm*pop*costm$alpha.m2*costm$sigma.m2.to.mam +
                                                                                                                                                 costm$cov*costm$phi.m2*costm$alpha2.m2.prmm*pop*costm$alpha.m2*costm$sigma.m2.to.mam+
                                                                                                                                                 costm$cov*costm$alpha.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam +
                                                                                                                                                 costm$cov*costm$phi.m2*costm$alpha2.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam) ,

                                                                             ifelse(costm$variable2=="MANGO"&
                                                                                      costm$variable6 == "Severe Wasting",
                                                                                    costm$value/(pop*costm$alpha.s3 + pop*costm$alpha2.s3*costm$phi.s3  +
                                                                                                   costm$cov*costm$alpha.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                                   costm$cov*costm$phi.s3*costm$alpha2.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                                   costm$cov*costm$alpha.s3.prms*pop*costm$alpha.m3*costm$sigma.m3.to.sam +
                                                                                                   costm$cov*costm$phi.s3*costm$alpha2.s3.prms*pop*costm$alpha.m3*costm$sigma.m3.to.sam),

                                                                                    costm$value/(pop*costm$alpha.m3 + pop*costm$alpha2.m3*costm$phi.m3 +
                                                                                                   costm$cov*costm$alpha.m3.prmm*pop*costm$alpha.m3*costm$sigma.m3.to.mam +
                                                                                                   costm$cov*costm$phi.m3*costm$alpha2.m3.prmm*pop*costm$alpha.m3*costm$sigma.m3.to.mam+
                                                                                                   costm$cov*costm$alpha.m3.prsm*pop*costm$alpha.s3*costm$sigma.s3.to.mam +
                                                                                                   costm$cov*costm$phi.m3*costm$alpha2.m3.prsm*pop*costm$alpha.s3*costm$sigma.s3.to.mam) )))))))
          
          costm$valueall <- rep(NA,dim(costm)[1])
          prot = names(table(costm$variable22))
          hh = length(as.numeric(names(table(costm$alpha.s))))
          for (i in 1:dim(costm)[1]){
            for (ii in 1:length(prot)){
              for (iii in 1:hh){
                costm$valueall[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$value[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
              }
            }
          }
          costm$cost.per.child_overall  <- ifelse(costm$variable2=="Standard" ,costm$valueall/(pop*costm$alpha.s + pop*costm$alpha2.s*costm$phi.s +
                                                                                                costm$cov*costm$alpha.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam +
                                                                                                costm$cov*costm$phi.s*costm$alpha2.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam+
                                                                                                costm$cov*costm$alpha.s.prms*pop*costm$alpha.m*costm$sigma.m.to.sam +
                                                                                                costm$cov*costm$phi.s*costm$alpha2.s.prms*pop*costm$alpha.m*costm$sigma.m.to.sam + 
                                                                                                                                                   pop*costm$alpha.m + pop*costm$alpha2.m*costm$phi.m +
                                                                                                                                                   costm$cov*costm$alpha.m.prmm*pop*costm$alpha.m*costm$sigma.m.to.mam +
                                                                                                                                                   costm$cov*costm$phi.m*costm$alpha2.m.prmm*pop*costm$alpha.m*costm$sigma.m.to.mam+
                                                                                                                                                   costm$cov*costm$alpha.m.prsm*pop*costm$alpha.s*costm$sigma.s.to.mam +
                                                                                                                                                   costm$cov*costm$phi.m*costm$alpha2.m.prsm*pop*costm$alpha.s*costm$sigma.s.to.mam) ,
                                                 
                                                 ifelse(costm$variable2=="OptiMA",costm$valueall/(pop*costm$alpha.s1 + pop*costm$alpha2.s1*costm$phi.s1  +
                                                                                                              costm$cov*costm$alpha.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                              costm$cov*costm$phi.s1*costm$alpha2.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                              costm$cov*costm$alpha.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam +
                                                                                                              costm$cov*costm$phi.s1*costm$alpha2.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam +
                                                                                                                                   pop*costm$alpha.m1 + pop*costm$alpha2.m1*costm$phi.m1 +
                                                                                                                                   costm$cov*costm$alpha.m1.prmm*pop*costm$alpha.m1*costm$sigma.m1.to.mam +
                                                                                                                                   costm$cov*costm$phi.m1*costm$alpha2.m1.prmm*pop*costm$alpha.m1*costm$sigma.m1.to.mam+
                                                                                                                                   costm$cov*costm$alpha.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam +
                                                                                                                                   costm$cov*costm$phi.m1*costm$alpha2.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam) ,
                                                               
                                                               ifelse(costm$variable2=="ComPAS"&
                                                                        costm$variable6 == "Severe Wasting",costm$valueall/(pop*costm$alpha.s2 + pop*costm$alpha2.s2*costm$phi.s2  +
                                                                                                                           costm$cov*costm$alpha.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                           costm$cov*costm$phi.s2*costm$alpha2.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                           costm$cov*costm$alpha.s2.prms*pop*costm$alpha.m2*costm$sigma.m2.to.sam +
                                                                                                                           costm$cov*costm$phi.s2*costm$alpha2.s2.prms*pop*costm$alpha.m2*costm$sigma.m2.to.sam +
                                                                                                                                                 pop*costm$alpha.m2 + pop*costm$alpha2.m2*costm$phi.m2 +
                                                                                                                                                 costm$cov*costm$alpha.m2.prmm*pop*costm$alpha.m2*costm$sigma.m2.to.mam +
                                                                                                                                                 costm$cov*costm$phi.m2*costm$alpha2.m2.prmm*pop*costm$alpha.m2*costm$sigma.m2.to.mam+
                                                                                                                                                 costm$cov*costm$alpha.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam +
                                                                                                                                                 costm$cov*costm$phi.m2*costm$alpha2.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam) ,
                                                                             
                                                                             #ifelse(costm$variable2=="MANGO", 
                                                                                    costm$valueall/(pop*costm$alpha.s3 + pop*costm$alpha2.s3*costm$phi.s3  +
                                                                                                   costm$cov*costm$alpha.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                                   costm$cov*costm$phi.s3*costm$alpha2.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                                   costm$cov*costm$alpha.s3.prms*pop*costm$alpha.m3*costm$sigma.m3.to.sam +
                                                                                                   costm$cov*costm$phi.s3*costm$alpha2.s3.prms*pop*costm$alpha.m3*costm$sigma.m3.to.sam+
                                                                                                   pop*costm$alpha.m3 + pop*costm$alpha2.m3*costm$phi.m3 +
                                                                                                   costm$cov*costm$alpha.m3.prmm*pop*costm$alpha.m3*costm$sigma.m3.to.mam +
                                                                                                   costm$cov*costm$phi.m3*costm$alpha2.m3.prmm*pop*costm$alpha.m3*costm$sigma.m3.to.mam+
                                                                                                   costm$cov*costm$alpha.m3.prsm*pop*costm$alpha.s3*costm$sigma.s3.to.mam +
                                                                                                   costm$cov*costm$phi.m3*costm$alpha2.m3.prsm*pop*costm$alpha.s3*costm$sigma.s3.to.mam) )))


          costm$number.recovered  <- ifelse(costm$variable2=="Standard" &
                                              costm$variable6 == "Severe Wasting" ,(pop*costm$alpha.s + pop*costm$alpha2.s*costm$phi.s +
                                                                                      costm$cov*costm$alpha.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam +
                                                                                      costm$cov*costm$phi.s*costm$alpha2.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam+
                                                                                      costm$cov*costm$alpha.s.prms*pop.m*costm$alpha.m*costm$sigma.m.to.sam +
                                                                                      costm$cov*costm$phi.s*costm$alpha2.s.prms*pop.m*costm$alpha.m*costm$sigma.m.to.sam) ,

                                            ifelse(costm$variable2=="Standard" &
                                                     costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB")  ,(pop.m*costm$alpha.m + pop.m*costm$alpha2.m*costm$phi.m +
                                                                                                                                          costm$cov*costm$alpha.m.prmm*pop.m*costm$alpha.m*costm$sigma.m.to.mam +
                                                                                                                                          costm$cov*costm$phi.m*costm$alpha2.m.prmm*pop.m*costm$alpha.m*costm$sigma.m.to.mam+
                                                                                                                                          costm$cov*costm$alpha.m.prsm*pop*costm$alpha.s*costm$sigma.s.to.mam +
                                                                                                                                          costm$cov*costm$phi.m*costm$alpha2.m.prsm*pop*costm$alpha.s*costm$sigma.s.to.mam) ,

                                                   ifelse(costm$variable2=="OptiMA"&
                                                            costm$variable6 == "Severe Wasting" ,(pop*costm$alpha.s1 + pop*costm$alpha2.s1*costm$phi.s1  +
                                                                                                    costm$cov*costm$alpha.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                    costm$cov*costm$phi.s1*costm$alpha2.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                    costm$cov*costm$alpha.s1.prms*pop.m*costm$alpha.m1*costm$sigma.m1.to.sam +
                                                                                                    costm$cov*costm$phi.s1*costm$alpha2.s1.prms*pop.m*costm$alpha.m1*costm$sigma.m1.to.sam),
                                                          ifelse(costm$variable2=="OptiMA" &
                                                                   costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,(pop.m*costm$alpha.m1 + pop.m*costm$alpha2.m1*costm$phi.m1 +
                                                                                                                         costm$cov*costm$alpha.m1.prmm*pop.m*costm$alpha.m1*costm$sigma.m1.to.mam +
                                                                                                                         costm$cov*costm$phi.m1*costm$alpha2.m1.prmm*pop.m*costm$alpha.m1*costm$sigma.m1.to.mam+
                                                                                                                         costm$cov*costm$alpha.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam +
                                                                                                                         costm$cov*costm$phi.m1*costm$alpha2.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam) ,

                                                                 ifelse(costm$variable2=="ComPAS"&
                                                                          costm$variable6 == "Severe Wasting",(pop*costm$alpha.s2 + pop*costm$alpha2.s2*costm$phi.s2  +
                                                                                                                 costm$cov*costm$alpha.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                 costm$cov*costm$phi.s2*costm$alpha2.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                 costm$cov*costm$alpha.s2.prms*pop.m*costm$alpha.m2*costm$sigma.m2.to.sam +
                                                                                                                 costm$cov*costm$phi.s2*costm$alpha2.s2.prms*pop.m*costm$alpha.m2*costm$sigma.m2.to.sam),

                                                                        ifelse(costm$variable2=="ComPAS" &
                                                                                 costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,(pop.m*costm$alpha.m2 + pop.m*costm$alpha2.m2*costm$phi.m2 +
                                                                                                                                       costm$cov*costm$alpha.m2.prmm*pop.m*costm$alpha.m2*costm$sigma.m2.to.mam +
                                                                                                                                       costm$cov*costm$phi.m2*costm$alpha2.m2.prmm*pop.m*costm$alpha.m2*costm$sigma.m2.to.mam+
                                                                                                                                       costm$cov*costm$alpha.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam +
                                                                                                                                       costm$cov*costm$phi.m2*costm$alpha2.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam) ,

                                                                               ifelse(costm$variable2=="MANGO"&
                                                                                        costm$variable6 == "Severe Wasting",
                                                                                      (pop*costm$alpha.s3 + pop*costm$alpha2.s3*costm$phi.s3  +
                                                                                         costm$cov*costm$alpha.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                         costm$cov*costm$phi.s3*costm$alpha2.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                         costm$cov*costm$alpha.s3.prms*pop.m*costm$alpha.m3*costm$sigma.m3.to.sam +
                                                                                         costm$cov*costm$phi.s3*costm$alpha2.s3.prms*pop.m*costm$alpha.m3*costm$sigma.m3.to.sam),

                                                                                      (pop.m*costm$alpha.m3 + pop.m*costm$alpha2.m3*costm$phi.m3 +
                                                                                         costm$cov*costm$alpha.m3.prmm*pop.m*costm$alpha.m3*costm$sigma.m3.to.mam +
                                                                                         costm$cov*costm$phi.m3*costm$alpha2.m3.prmm*pop.m*costm$alpha.m3*costm$sigma.m3.to.mam+
                                                                                         costm$cov*costm$alpha.m3.prsm*pop*costm$alpha.s3*costm$sigma.s3.to.mam +
                                                                                         costm$cov*costm$phi.m3*costm$alpha2.m3.prsm*pop*costm$alpha.s3*costm$sigma.s3.to.mam) )))))))


          costm$number.treated  <- ifelse(costm$variable2=="Standard" &
                                            costm$variable6 == "Severe Wasting" , (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                     costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                     costm$cov* pop.m * costm$alpha.m * costm$sigma.m.to.sam  + costm$cov * costm$tau.m * pop.m *costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m),

                                          ifelse(costm$variable2=="Standard" &
                                                   costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB")  ,(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                                                                        costm$cov *  pop.m * costm$alpha.m * costm$sigma.m.to.mam) ,

                                                 ifelse(costm$variable2=="OptiMA"&
                                                          costm$variable6 == "Severe Wasting" ,(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                  costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                  costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                  costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m),
                                                        ifelse(costm$variable2=="OptiMA" &
                                                                 costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                                       costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ,

                                                               ifelse(costm$variable2=="ComPAS"&
                                                                        costm$variable6 == "Severe Wasting",(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                               costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                               costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                               costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m),

                                                                      ifelse(costm$variable2=="ComPAS" &
                                                                               costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                                                     costm$cov *  pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam  ) ,

                                                                             ifelse(costm$variable2=="MANGO"&
                                                                                      costm$variable6 == "Severe Wasting",
                                                                                    (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                       costm$cov * costm$tau.m * (pop + pop.m*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam +
                                                                                       costm$cov* pop.m * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                       costm$cov * pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m),

                                                                                    (pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam +
                                                                                       costm$cov *  pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam) )))))))


          costm$cost.per.child.trt <- ifelse(costm$variable2=="Standard" &
                                               costm$variable6 == "Severe Wasting" , costm$value/(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                    costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                                    costm$cov* pop.m * costm$alpha.m * costm$sigma.m.to.sam  + costm$cov * pop.m * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m),

                                             ifelse(costm$variable2=="Standard" &
                                                      costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB")  ,costm$value/(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                                                                                       costm$cov *  pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam) ,

                                                    ifelse(costm$variable2=="OptiMA"&
                                                             costm$variable6 == "Severe Wasting" ,costm$value/(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                 costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                                 costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                                 costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m),
                                                           ifelse(costm$variable2=="OptiMA" &
                                                                    costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,costm$value/(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                                                      costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ,

                                                                  ifelse(costm$variable2=="ComPAS"&
                                                                           costm$variable6 == "Severe Wasting",costm$value/(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                              costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                                              costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                                              costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m),

                                                                         ifelse(costm$variable2=="ComPAS" &
                                                                                  costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,costm$value/(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                                                                    costm$cov *  pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam  ) ,

                                                                                ifelse(costm$variable2=="MANGO"&
                                                                                         costm$variable6 == "Severe Wasting",
                                                                                       costm$value/ (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                       costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam +
                                                                                                       costm$cov* pop.m * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                                       costm$cov * pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m),

                                                                                       costm$value/ (pop.m + costm$cov *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam +
                                                                                                       costm$cov *  pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam) )))))))


          costm$cost.per.sach  <- ifelse(costm$variable22=="Standard" & costm$variable6 == "Severe Wasting" ,costm$value/(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                            costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                          costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                                            costm$number.sachet.prms * (costm$cov* pop.m * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                                          costm$cov * pop.m * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,
                                         ifelse(costm$variable22=="Standard - CSB" & costm$variable6 == "Severe Wasting" ,costm$value/(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                                   costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                 costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                                                   costm$number.sachet.prms * (costm$cov* pop.m * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                                                 costm$cov * pop.m * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,

                                         ifelse(costm$variable22=="Standard" & costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB") ,costm$value/(costm$number.sachet.m * pop.m +
                                                                                                                                                                               costm$number.sachet.prsm * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                               costm$number.sachet.prmm * (costm$cov *  pop.m * costm$alpha.m * costm$sigma.m.to.mam )) ,

                                                ifelse(costm$variable22=="Standard - CSB" & costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB") ,costm$value/(costm$number.csb.m * pop.m +
                                                                                                                                                                                            costm$number.csb.prsm * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                                            costm$number.csb.prmm * (costm$cov *  pop.m * costm$alpha.m * costm$sigma.m.to.mam )) ,

                                                       ifelse(costm$variable22=="OptiMA"& costm$variable6 == "Severe Wasting" ,costm$value/(costm$number.sachet.s1 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                              costm$number.sachet.prss1 * (costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                             costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                              costm$number.sachet.prms1 * (costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                                                                                             costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m)),

                                                              ifelse(costm$variable22=="OptiMA" & costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,costm$value/(costm$number.sachet.m1 * pop.m +
                                                                                                                                                                    costm$number.sachet.prsm1 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                                                    costm$number.sachet.prmm1 * (costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ) ,


                                                                     ifelse(costm$variable22=="ComPAS"& costm$variable6 == "Severe Wasting",costm$value/(costm$number.sachet.s2 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                                                           costm$number.sachet.prss2 * (costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                                          costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam) +
                                                                                                                                                           costm$number.sachet.prms2 * (costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                                                                                                          costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m)),

                                                                            ifelse(costm$variable22=="ComPAS" & costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,costm$value/(costm$number.sachet.m2 * pop.m +
                                                                                                                                                                                  costm$number.sachet.prsm2 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam)   +
                                                                                                                                                                                  costm$number.sachet.prmm2 * (costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam)) ,

                                                                                   ifelse(costm$variable22=="MANGO"& costm$variable6 == "Severe Wasting", costm$value/(costm$number.sachet.s3 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                                                                         costm$number.sachet.prss3 * (costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                                                        costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam) +
                                                                                                                                                                         costm$number.sachet.prms3 * (costm$cov* pop.m * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                                                                                                                                        costm$cov * pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m) ),

                                                                                          costm$value/(costm$number.sachet.m3 * pop.m +
                                                                                                         costm$number.sachet.prsm3 * (costm$cov *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam) +
                                                                                                         costm$number.sachet.prmm3 * (costm$cov *  pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam)) )))))))))




          costm$number.sach  <- ifelse(costm$variable22=="Standard" & costm$variable6 == "Severe Wasting" ,(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                              costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                            costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                              costm$number.sachet.prms * (costm$cov* pop.m * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                            costm$cov * pop.m * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,
                                       ifelse(costm$variable22=="Standard - CSB" & costm$variable6 == "Severe Wasting" ,(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                     costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                   costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                                     costm$number.sachet.prms * (costm$cov* pop.m * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                                   costm$cov * pop.m * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,

                                       ifelse(costm$variable22=="Standard" & costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB") ,(costm$number.sachet.m * pop.m +
                                                                                                                                                                 costm$number.sachet.prsm * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                 costm$number.sachet.prmm * (costm$cov *  pop.m * costm$alpha.m * costm$sigma.m.to.mam )) ,

                                              ifelse(costm$variable22=="Standard - CSB" & costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB") ,(costm$number.csb.m * pop.m +
                                                                                                                                                                              costm$number.csb.prsm * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                              costm$number.csb.prmm * (costm$cov *  pop.m * costm$alpha.m * costm$sigma.m.to.mam )) ,

                                                     ifelse(costm$variable22=="OptiMA"& costm$variable6 == "Severe Wasting" ,(costm$number.sachet.s1 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                costm$number.sachet.prss1 * (costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                               costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                costm$number.sachet.prms1 * (costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                                                                               costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m)),

                                                            ifelse(costm$variable22=="OptiMA" & costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,(costm$number.sachet.m1 * pop.m +
                                                                                                                                                      costm$number.sachet.prsm1 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                                      costm$number.sachet.prmm1 * (costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ) ,


                                                                   ifelse(costm$variable22=="ComPAS"& costm$variable6 == "Severe Wasting",(costm$number.sachet.s2 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                                             costm$number.sachet.prss2 * (costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                            costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam) +
                                                                                                                                             costm$number.sachet.prms2 * (costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                                                                                            costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m)),

                                                                          ifelse(costm$variable22=="ComPAS" & costm$variable6 == "Moderate Wasting - RUSF/RUTF" ,(costm$number.sachet.m2 * pop.m +
                                                                                                                                                                    costm$number.sachet.prsm2 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam)   +
                                                                                                                                                                    costm$number.sachet.prmm2 * (costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam)) ,

                                                                                 ifelse(costm$variable22=="MANGO"& costm$variable6 == "Severe Wasting", (costm$number.sachet.s3 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                                                           costm$number.sachet.prss3 * (costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                                          costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam) +
                                                                                                                                                           costm$number.sachet.prms3 * (costm$cov* pop.m * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                                                                                                                          costm$cov * pop.m * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m) ),

                                                                                        0 )))))))))
          
          costm$variable6[which(costm$variable6 %in% c("Severe Wasting"))] <- "SAM"
          costm$variable6[which(costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB"))] <- "MAM"
          if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){
            costm <- costm[-which(costm$variable22 %in% "Standard - CSB"),]
            costm$variable22[which(costm$variable22 %in% "Standard")] <- "Standard - RUTF/RUSF"
          }
          if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){
            costm <- costm[-which(costm$variable22 %in% "Standard"),]
            costm$variable22[which(costm$variable22 %in% "Standard - CSB")] <- "Standard - RUTF/FBF"
          }
          if (input$mamorsam %in% "Just SAM"){
            costm <- costm[-which(costm$variable22 %in% "Standard - CSB"),]
          }
          ###   
          ### total cost.per.child recovered
          ### 
          
          ci.total.cost.rec.all <- data.frame(costm %>% group_by(variable22,variable4,variable5,cov) %>% 
                                                dplyr::summarise(mean.cr= mean(input$conv_rate*cost.per.child_overall),stdv.cr= sd(input$conv_rate*cost.per.child_overall )))
          
          ci.total.cost.rec <- data.frame(costm %>% group_by(variable22,variable4,variable5,variable6,cov) %>% 
                                            dplyr::summarise(mean.cr= mean(input$conv_rate*cost.per.child),stdv.cr= sd(input$conv_rate*cost.per.child )))
          
          if (input$mamorsam %in% "Just SAM"){
            ci.total.cost.rec.all <- ci.total.cost.rec[which(ci.total.cost.rec$variable6 %in% "SAM"),]
            #ci.total.cost.rec.all <- subset(ci.total.cost.rec.all, select = -c(variable4,variable5) )
          }
          
          #ci.total.cost.rec.all$variable22 <-factor(ci.total.cost.rec.all$variable22,levels=c("Standard - RUTF/RUSF","Standard - RUTF/FBF","OptiMA","ComPAS", "MANGO"))
          #ci.total.cost.rec$variable22 <-factor(ci.total.cost.rec$variable22,levels=c("Standard - RUTF/RUSF","Standard - RUTF/FBF","OptiMA","ComPAS", "MANGO"))
          
          ci.total.cost.rec.all_CLEANED2 = subset(ci.total.cost.rec.all, select = -c(variable4,variable5) )
          ci.total.cost.rec.all_CLEANED2 = ci.total.cost.rec.all_CLEANED2[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop),]
          ci.total.cost.rec.all_CLEANED <- data.frame(Protocol = ci.total.cost.rec.all_CLEANED2$variable22,
                                              #`Coverage of Treatment Post-Relapse` = ci.total.cost.rec.all_CLEANED2$variable7,
                                              `Cost per Recovered Child` = scales::dollar(ci.total.cost.rec.all_CLEANED2$mean.cr),
                                              `SD for Cost per Recovered Child` = scales::dollar(ci.total.cost.rec.all_CLEANED2$stdv.cr))
          
          ci.total.cost.rec_CLEANED2 = subset(ci.total.cost.rec, select = -c(variable4,variable5) )
          ci.total.cost.rec_CLEANED2 = ci.total.cost.rec_CLEANED2[which(ci.total.cost.rec_CLEANED2$cov %in% input$relapse_treat_prop),]
          ci.total.cost.rec_CLEANED3 <- data.frame(Protocol = ci.total.cost.rec_CLEANED2$variable22,
                                                      #`Coverage of Treatment Post-Relapse` = ci.total.cost.rec_CLEANED2$variable7,
                                                      `Cost per Recovered Child` = scales::dollar(ci.total.cost.rec_CLEANED2$mean.cr),
                                                      `SD for Cost per Recovered Child` = scales::dollar(ci.total.cost.rec_CLEANED2$stdv.cr),
                                                   `Malnutrition Status` = ci.total.cost.rec_CLEANED2$variable6)
          ci.total.cost.rec.all_CLEANED3 <- ci.total.cost.rec.all_CLEANED
          ci.total.cost.rec.all_CLEANED3$Malnutrition.Status <- rep("All",dim(ci.total.cost.rec.all_CLEANED3)[1])
          ci.total.cost.rec_CLEANED <- rbind(ci.total.cost.rec.all_CLEANED3,ci.total.cost.rec_CLEANED3)
          names(ci.total.cost.rec.all_CLEANED) <- c("Protocol","Cost per Recovered Child","SD for Cost per Recovered Child")
          names(ci.total.cost.rec_CLEANED) <- c("Protocol","Cost per Recovered Child","SD for Cost per Recovered Child","Malnutrition Status")
        
        ci.total.cost.rec.all_CLEANED2$variable6 <- rep("All",dim(ci.total.cost.rec.all_CLEANED2)[1])
        ce.df <- rbind(ci.total.cost.rec.all_CLEANED2,ci.total.cost.rec_CLEANED2)
        ce.df$Group <- ifelse(ce.df$variable6 %in% c("All","SAM"),ce.df$variable6,"MAM")
        ce.df$Group[which(ce.df$Group %in% c("SAM"))]<-"SAM"
        ###
        ### total programmatic cost
        ###
        
        costm_REV <- data.frame(value = rep(NA,dim(costm)[1]),variable22 = costm$variable22,cov = costm$cov)
        prot = names(table(costm$variable22))
        hh = length(as.numeric(names(table(costm$alpha.s))))
        for (i in 1:dim(costm_REV)[1]){
        for (ii in 1:length(prot)){
          for (iii in 1:hh){
          costm_REV$value[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$value[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
          }
        }
        }
        ci.total.cost.all <- data.frame(costm_REV %>% group_by(variable22,cov) %>% 
                                          dplyr::summarise(mean.tc = mean((input$conv_rate)*value/1000),stdv.tc = sd((input$conv_rate)*value/1000))) 
        ci.total.cost <- data.frame(costm %>% group_by(variable22,variable4,variable5,variable6,cov) %>% 
                                      dplyr::summarise(mean.tc = mean((input$conv_rate)*value/1000),stdv.tc = sd((input$conv_rate)*value/1000)))
        
        if (input$mamorsam %in% "Just SAM"){
          ci.total.cost.all <- ci.total.cost[which(ci.total.cost$variable6 %in% "SAM"),]
          ci.total.cost.all <- subset(ci.total.cost.all, select = -c(variable4,variable5) )
        }
        
        #ci.total.cost.all$variable22 <-factor(ci.total.cost.all$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
        #ci.total.cost$variable22 <-factor(ci.total.cost$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
        
        ci.total.cost.all_CLEANED2 = ci.total.cost.all[which(ci.total.cost.all$cov %in% input$relapse_treat_prop),]
        ci.total.cost.all_CLEANED <- data.frame(Protocol = ci.total.cost.all_CLEANED2$variable22,
                                            #`Coverage of Treatment Post-Relapse` = ci.total.cost.all_CLEANED2$variable7,
                                            `Program Costs` = scales::dollar(10^3*ci.total.cost.all_CLEANED2$mean.tc),
                                            `SD for Program Costs` = scales::dollar(10^3*ci.total.cost.all_CLEANED2$stdv.tc))
        
        ci.total.cost_CLEANED2 = subset(ci.total.cost, select = -c(variable4,variable5) )
        ci.total.cost_CLEANED2 = ci.total.cost_CLEANED2[which(ci.total.cost_CLEANED2$cov %in% input$relapse_treat_prop),]
        ci.total.cost_CLEANED3 <- data.frame(Protocol = ci.total.cost_CLEANED2$variable22,
                                                 #`Coverage of Treatment Post-Relapse` = ci.total.cost_CLEANED2$variable7,
                                                 `Program Costs` = scales::dollar(10^3*ci.total.cost_CLEANED2$mean.tc),
                                                 `SD for Program Costs` = scales::dollar(10^3*ci.total.cost_CLEANED2$stdv.tc),
                                                 `Malnutrition Status` = ci.total.cost_CLEANED2$variable6)
        ci.total.cost.all_CLEANED3 <- ci.total.cost.all_CLEANED
        ci.total.cost.all_CLEANED3$Malnutrition.Status <- rep("All",dim(ci.total.cost.all_CLEANED3)[1])
        ci.total.cost_CLEANED <- rbind(ci.total.cost.all_CLEANED3,ci.total.cost_CLEANED3)
        names(ci.total.cost.all_CLEANED) <- c("Protocol","Total Program Costs","SD for Total Program Costs")
        names(ci.total.cost_CLEANED) <- c("Protocol","Total Program Costs","SD for Total Program Costs","Malnutrition Status")
        
        ci.total.cost.all_CLEANED2$variable6 <- rep("All",dim(ci.total.cost.all_CLEANED2)[1])
        tc.df <- rbind(ci.total.cost.all_CLEANED2,ci.total.cost_CLEANED2)
        
        ###   
        ### total amount of product needed
        ###
        costm_prod_REV <- data.frame(number.sach = rep(NA,dim(costm)[1]),variable22 = costm$variable22,cov = costm$cov)
        prot = names(table(costm$variable22))
        hh = length(as.numeric(names(table(costm$alpha.s))))
        for (i in 1:dim(costm_prod_REV)[1]){
          for (ii in 1:length(prot)){
            for (iii in 1:hh){
              costm_prod_REV$number.sach[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$number.sach[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
            }
          }
        }
        ci.total.product.all <- data.frame(costm_prod_REV %>% group_by(variable22,cov) %>% 
                                             dplyr::summarise(mean.prod= mean(number.sach),stdv.prod= sd(number.sach)))
        
        ci.total.product <- data.frame(costm %>% group_by(variable22,variable4,variable5,variable6,cov) %>% 
                                         dplyr::summarise(mean.prod= mean(number.sach),stdv.prod= sd(number.sach)))
        
        if (input$mamorsam %in% "Just SAM"){
          ci.total.product.all <- ci.total.product[which(ci.total.product$variable6 %in% "SAM"),]
          ci.total.product.all <- subset(ci.total.product.all, select = -c(variable4,variable5) )
        }
        
        #ci.total.product.all$variable22 <-factor(ci.total.product.all$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
        #ci.total.product$variable22 <-factor(ci.total.product$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))

        ci.total.product.all_CLEANED2 = ci.total.product.all[which(ci.total.product.all$cov %in% input$relapse_treat_prop),]
        ci.total.product.all_CLEANED <- data.frame(Protocol = ci.total.product.all_CLEANED2$variable22,
                                                   #`Coverage of Treatment Post-Relapse` = ci.total.product.all_CLEANED2$variable7,
                                                   `Average Units of Product` = (ci.total.product.all_CLEANED2$mean.prod),
                                                   `SD for Units of Product` = (ci.total.product.all_CLEANED2$stdv.prod))
        
        ci.total.product_CLEANED2 = subset(ci.total.product, select = -c(variable4,variable5) )
        ci.total.product_CLEANED2 = ci.total.product_CLEANED2[which(ci.total.product_CLEANED2$cov %in% input$relapse_treat_prop),]
        ci.total.product_CLEANED3 <- data.frame(Protocol = ci.total.product_CLEANED2$variable22,
                                                #`Coverage of Treatment Post-Relapse` = ci.total.product_CLEANED2$variable7,
                                                `Average Units of Product` = (ci.total.product_CLEANED2$mean.prod),
                                                `SD for Units of Product` = (ci.total.product_CLEANED2$stdv.prod),
                                                #`Product` = ifelse(ci.total.product_CLEANED2$variable22 %in% "Standard - RUTF/RUSF","RUTF","RUTF/RUSF or CSB"),
                                                `Malnutrition Status` = ci.total.product_CLEANED2$variable6)
        ci.total.product.all_CLEANED3 <- ci.total.product.all_CLEANED
        ci.total.product.all_CLEANED3$Malnutrition.Status <- rep("All",dim(ci.total.product.all_CLEANED3)[1])
        ci.total.product_CLEANED <- rbind(ci.total.product.all_CLEANED3,ci.total.product_CLEANED3)
        names(ci.total.product.all_CLEANED) <- c("Protocol","Average Cartons of Product Needed","SD for Cartons of Product Needed")
        names(ci.total.product_CLEANED) <- c("Protocol","Average Cartons of Product Needed","SD for Cartons of Product Needed",#"Type of Product",
                                             "Malnutrition Status")
        
        ci.total.product.all_CLEANED2$variable6 <- rep("All",dim(ci.total.product.all_CLEANED2)[1])
        prod.df <- rbind(ci.total.product.all_CLEANED2,ci.total.product_CLEANED2)
        prod.df$Group <- ifelse(prod.df$variable6 %in% c("All","SAM"),prod.df$variable6,"MAM")
        prod.df$Group[which(prod.df$Group %in% c("SAM"))]<-"SAM"
        
        Currencyforoutputs <- ifelse(!(input$Currency %in% "Other"), input$Currency, input$OtherCurrency)
        CurrentApproach <- ifelse(input$approach_based_on %in% "Standard" & input$standard_mam_product %in% "CSB" & input$mamorsam %in% "Both SAM and MAM" , "Standard - RUTF/FBF", input$approach_based_on)
        CurrentApproach <- ifelse(input$approach_based_on %in% "Standard" & input$standard_mam_product %in% "RUSF" & input$mamorsam %in% "Both SAM and MAM", "Standard - RUTF/RUSF", CurrentApproach)
        observeEvent(input$ph_objective,       
                     ignoreNULL = F, { 
        if (input$ph_objective %in% "Improve cost efficiency of nutrition program") {
          output$Interpretation_Primary <- renderText({
            paste("The average cost per recovered child under the current protocol (", CurrentApproach, ") would be an estimated ",
                  scales::dollar(round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable2 %in% CurrentApproach)],0)), input$Currency, ".")
          })
          output$Relative_to_Current <-  renderText({
                "How this relates to other protocols is depicted here. A protocol with a 75% would mean that the cost per recovered child is 25% less than the current protocol, while a protocol with 125% would mean that the cost per recovered child is 25% more than the current protocol."
          })
          if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "ComPAS")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) <1){
            colorforCompas = "green"
            Compasicon = icon("angle-double-down", lib = "font-awesome")}
          else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "ComPAS")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) >1){
            colorforCompas = "red"
            Compasicon = icon("angle-double-up", lib = "font-awesome")}
          else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "ComPAS")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) == 1){
            colorforCompas = "blue"
            Compasicon = icon("equals", lib = "font-awesome")}
          
          if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "MANGO")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) <1){
            colorforMango = "green"
            Mangoicon = icon("angle-double-down", lib = "font-awesome")}
          else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "MANGO")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) >1){
            colorforMango = "red"
            Mangoicon = icon("angle-double-up", lib = "font-awesome")}
          else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "MANGO")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) == 1){
            colorforMango = "blue"
            Mangoicon = icon("equals", lib = "font-awesome")}
          
          if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "OptiMA")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) <1){
            colorforOptima = "green"
            Optimaicon = icon("angle-double-down", lib = "font-awesome")}
          else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "OptiMA")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) >1){
            colorforOptima = "red"
            Optimaicon = icon("angle-double-up", lib = "font-awesome")}
          else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "OptiMA")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) == 1){
            colorforOptima = "blue"
            Optimaicon = icon("equals", lib = "font-awesome")}
          
          if (input$mamorsam %in% "Just SAM"){
            if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) <1){
              colorforStandard = "green"
              Standardicon = icon("angle-double-down", lib = "font-awesome")}
            else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) >1){
              colorforStandard = "red"
              Standardicon = icon("angle-double-up", lib = "font-awesome")}
            else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) == 1){
              colorforStandard = "blue"
              Standardicon = icon("equals", lib = "font-awesome")}
          }
          if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){          
            if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) <1){
              colorforStandard = "green"
              Standardicon = icon("angle-double-down", lib = "font-awesome")}
            else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) >1){
              colorforStandard = "red"
              Standardicon = icon("angle-double-up", lib = "font-awesome")}
            else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) == 1){
              colorforStandard = "blue"
              Standardicon = icon("equals", lib = "font-awesome")}
          }
          if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){          
            if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/FBF")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) <1){
              colorforStandard = "green"
              Standardicon = icon("angle-double-down", lib = "font-awesome")}
            else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/FBF")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) >1){
              colorforStandard = "red"
              Standardicon = icon("angle-double-up", lib = "font-awesome")}
            else if (round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/FBF")],0)/round(ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)],0) == 1){
              colorforStandard = "blue"
              Standardicon = icon("equals", lib = "font-awesome")}
          }
          
          output$ComPASBox <- renderInfoBox({
            infoBox(
              "ComPAS", paste0(percent((ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "ComPAS")]/ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)]))), icon = Compasicon,
              color = colorforCompas, fill =TRUE#, width = 3
            )
          })
         if (input$mamorsam %in% "Just SAM") {
          output$MANGOBox　<- renderInfoBox({
            infoBox(
              "MANGO", paste0(percent((ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "MANGO")]/ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)]))), icon = Mangoicon,
              color = colorforMango, fill =TRUE#, width = 3
            )
          })
          }
          output$OptiMABox <-　renderInfoBox({
            infoBox(
              "OptiMA", paste0(percent((ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "OptiMA")]/ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)]))), icon = Optimaicon,
              color = colorforOptima, fill =TRUE#, width = 3
            )
          })
          if (input$mamorsam %in% "Just SAM"){
            output$StandardBox <- renderInfoBox({
              infoBox(
                "Standard", paste0(percent((ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard")]/ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)]))), icon = Standardicon,
                color = colorforStandard, fill =TRUE#, width = 3
              )
            })
          }
          if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){          
            output$StandardBox <- renderInfoBox({
              infoBox(
                "Standard", paste0(percent((ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/RUSF")]/ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)]))), icon = Standardicon,
                color = colorforStandard, fill =TRUE#, width = 3
              )
            })
          }
          if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){          
            output$StandardBox <- renderInfoBox({
              infoBox(
                "Standard", paste0(percent((ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% "Standard - RUTF/FBF")]/ci.total.cost.rec.all$mean.cr[which(ci.total.cost.rec.all$cov %in% input$relapse_treat_prop & ci.total.cost.rec.all$variable22 %in% CurrentApproach)]))), icon = Standardicon,
                color = colorforStandard, fill =TRUE#, width = 3
              )
            })
          }
          
          observeEvent(input$mamorsam,       
                       ignoreNULL = F, { 
                         if (input$mamorsam %in% "Just SAM") {
          #### Primary ####
          makePlot_CE_All <- function() {
            theme_set(theme_bw(15))
            ggplot(ci.total.cost.rec.all[ci.total.cost.rec.all$cov %in% input$relapse_treat_prop,] ,aes(x=variable22,y=mean.cr ,fill=variable22)) +  
              geom_bar(stat="identity", color="black", position=position_dodge()) +  
              geom_errorbar(aes(ymin=mean.cr -stdv.cr , ymax=mean.cr +stdv.cr ), width=.2, position=position_dodge(.9))+
              xlab("")+ ylab(paste("Total cost per child recovered (",Currencyforoutputs,")"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733","#88CCEE")) +
              ggtitle("Cost of Treatment per Recovered Child")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")+
              scale_y_continuous(labels = dollar)
          }
          
          output$Primary_Graph <- renderPlot({
            makePlot_CE_All()
          })# renderplot end
          output$downloadPlot_Primary_Graph <- downloadHandler(
            filename = function() {
              paste('plot-CostEfficiency-', Sys.Date(), '.pdf', sep='')
            },
            content = function(file) {
              
              #telemetry tracking
              log_to_database("download", list(
                input_id = "downloadPlot",
                download_type = "plot",
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              
              ggsave(file, makePlot_CE_All(), width = 6, height = 5, dpi = 300, units = "in")
            }
          )
          
          output$Primary_Table_Title <- renderText('Cost per Recovered Child, by Protocol')
          output$Note_uncertainty <- renderText(
            "Our model is built to randomly add some uncertainty to the cost inputs. We do this for 25 iterations. The Cost per Recovered Child
              in the table reflect the average results across the 25 iterations, and the standard deviation is also presented to reflect that
              the results should be interpreted as estimates within a range rather than exact values with certainty."
          )

          ci.total.cost.rec.all_CLEANED2 <- ci.total.cost.rec.all_CLEANED#[which(!(ci.total.cost.rec.all_CLEANED$Protocol %in% "Standard - CSB") ),]
          output$Primary_tbl = renderDT(
            ci.total.cost.rec.all_CLEANED2, options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
          )
          output$downloadDataTable_Primary <- downloadHandler(
            filename = function() {
              paste('data-CostEfficiency', Sys.Date(), '.csv', sep='')
            },
            content = function(con) {
              
              #telemetry tracking
              log_to_database("download", list(
                input_id = "downloadData",
                download_type = "data",
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              
              write.csv(ci.total.cost.rec.all_CLEANED2, con)
            }
          )
                         }
          
      if (input$mamorsam %in% "Both SAM and MAM") {
        
        makePlot_CE_All_SAM_MAM <- function() {
          theme_set(theme_bw(15))
          
          #grid.arrange(a00,a11,a22,ncol=3)
          ggplot(ce.df[which(!(ce.df$variable22 %in% "MANGO")),],aes(x=variable22,y=mean.cr,fill=variable22)) +  
            geom_bar(stat="identity", color="black", position=position_dodge()) +  
            geom_errorbar(aes(ymin=mean.cr - stdv.cr, ymax=mean.cr + stdv.cr), width=.2, position=position_dodge(.9))+
            xlab("")+ ylab(paste("Total cost per child recovered (",Currencyforoutputs,")"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
            scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~variable6,nrow=1) +
            ggtitle("Cost per Recovered Child")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")+
            scale_y_continuous(labels = dollar)
        }
        
        output$Primary_Graph <- renderPlot({
          makePlot_CE_All_SAM_MAM()
        })
        
        output$downloadPlot_Primary_Graph <- downloadHandler(
          filename = function() {
            paste('plot-CostEfficiency-', Sys.Date(), '.pdf', sep='')
          },
          content = function(file) {
            
            #telemetry tracking
            log_to_database("download", list(
              input_id = "downloadPlot",
              download_type = "plot",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            ))
            
            ggsave(file, makePlot_CE_All_SAM_MAM(), width = 9, height = 5, dpi = 300, units = "in")
          }
        )

        output$Primary_Table_Title <- renderText('Cost per Recovered Child, by Protocol')
        output$Note_uncertainty <- renderText(
          "Our model is built to randomly add some uncertainty to the cost inputs. We do this for 25 iterations. The Cost per Recovered Child
              in the table reflect the average results across the 25 iterations, and the standard deviation is also presented to reflect that
              the results should be interpreted as estimates within a range rather than exact values with certainty."
        )
        
        output$Primary_tbl = renderDT(
          ci.total.cost.rec_CLEANED[which(!(ci.total.cost.rec_CLEANED[,1] %in% "MANGO")),][order(ci.total.cost.rec_CLEANED$Protocol[which(!(ci.total.cost.rec_CLEANED[,1] %in% "MANGO"))]),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
        )
        output$downloadDataTable_Primary <- downloadHandler(
          filename = function() {
            paste('data-CostEfficiency', Sys.Date(), '.csv', sep='')
          },
          content = function(con) {
            
            #telemetry tracking
            log_to_database("download", list(
              input_id = "downloadData",
              download_type = "data",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            ))
            
            write.csv(ci.total.cost.rec_CLEANED[which(!(ci.total.cost.rec_CLEANED[,1] %in% "MANGO")),][order(ci.total.cost.rec_CLEANED$Protocol[which(!(ci.total.cost.rec_CLEANED[,1] %in% "MANGO"))]),], con)
          }
        )
      }
                       })
        
        }
                       else if (input$ph_objective %in% "Consider alternative approaches when product supply is limited") {
                         output$Interpretation_Primary <- renderText({gsub(pattern = "\\n", replacement = "<br/>", 
                                                                           paste("Given an estimated population of ", input$n_pop, " SAM children being treated with ", scales::percent(input$cov_s), " coverage and ", input$n_pop_m, " MAM children being treated with ",
                                                                                 scales::percent(input$cov_m), "coverage, an estimated",format(round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable2 %in% CurrentApproach)]/150,0), big.mark=","), "cartons of product would be needed to treat per the current (",CurrentApproach,") approach."
                                                                           ))
                         })
                         output$Relative_to_Current <-  renderText({gsub(pattern = "\\n", replacement = "<br/>", 
                                                                         paste("How the amount of product needed per the current protocol relates to needs for other protocols is depicted here. A protocol with a 75% would require 25% fewer cartons of product than the current protocol, while a protocol with 125% would be 25% more units of product than the current protocol. Note that the specific product (e.g., RUTF or RUSF) is not accounted for."
                                                                         ))
                         })
                         if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "ComPAS")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) <1){
                           colorforCompas = "green"
                           Compasicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "ComPAS")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) >1){
                           colorforCompas = "red"
                           Compasicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "ComPAS")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) == 1){
                           colorforCompas = "blue"
                           Compasicon = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "MANGO")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) <1){
                           colorforMango = "green"
                           Mangoicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "MANGO")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) >1){
                           colorforMango = "red"
                           Mangoicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "MANGO")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) == 1){
                           colorforMango = "blue"
                           Mangoicon = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "OptiMA")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) <1){
                           colorforOptima = "green"
                           Optimaicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "OptiMA")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) >1){
                           colorforOptima = "red"
                           Optimaicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "OptiMA")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) == 1){
                           colorforOptima = "blue"
                           Optimaicon = icon("equals", lib = "font-awesome")}
                         
                         if (input$mamorsam %in% "Just SAM"){
                           if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) <1){
                             colorforStandard = "green"
                             Standardicon = icon("angle-double-down", lib = "font-awesome")}
                           else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) >1){
                             colorforStandard = "red"
                             Standardicon = icon("angle-double-up", lib = "font-awesome")}
                           else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) == 1){
                             colorforStandard = "blue"
                             Standardicon = icon("equals", lib = "font-awesome")}
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){          
                           if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) <1){
                             colorforStandard = "green"
                             Standardicon = icon("angle-double-down", lib = "font-awesome")}
                           else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) >1){
                             colorforStandard = "red"
                             Standardicon = icon("angle-double-up", lib = "font-awesome")}
                           else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) == 1){
                             colorforStandard = "blue"
                             Standardicon = icon("equals", lib = "font-awesome")}
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){          
                           if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/FBF")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) <1){
                             colorforStandard = "green"
                             Standardicon = icon("angle-double-down", lib = "font-awesome")}
                           else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/FBF")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) >1){
                             colorforStandard = "red"
                             Standardicon = icon("angle-double-up", lib = "font-awesome")}
                           else if (round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/FBF")],0)/round(ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)],0) == 1){
                             colorforStandard = "blue"
                             Standardicon = icon("equals", lib = "font-awesome")}
                         }
                         
                         output$ComPASBox <- renderInfoBox({
                           infoBox(
                             "ComPAS", paste0(percent((ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "ComPAS")]/ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)]))), icon = Compasicon,
                             color = colorforCompas, fill =TRUE#, width = 3
                           )
                         })
                         if (input$mamorsam %in% "Just SAM") {
                         output$MANGOBox　<- renderInfoBox({
                           infoBox(
                             "MANGO", paste0(percent((ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "MANGO")]/ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)]))), icon = Mangoicon,
                             color = colorforMango, fill =TRUE#, width = 3
                           )
                         })
                         }
                         output$OptiMABox <-　renderInfoBox({
                           infoBox(
                             "OptiMA", paste0(percent((ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "OptiMA")]/ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)]))), icon = Optimaicon,
                             color = colorforOptima, fill =TRUE#, width = 3
                           )
                         })
                         if (input$mamorsam %in% "Just SAM"){
                           output$StandardBox <- renderInfoBox({
                             infoBox(
                               "Standard", paste0(percent((ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard")]/ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)]))), icon = Standardicon,
                               color = colorforStandard, fill =TRUE#, width = 3
                             )
                           })
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){          
                           output$StandardBox <- renderInfoBox({
                             infoBox(
                               "Standard", paste0(percent((ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/RUSF")]/ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)]))), icon = Standardicon,
                               color = colorforStandard, fill =TRUE#, width = 3
                             )
                           })
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){          
                           output$StandardBox <- renderInfoBox({
                             infoBox(
                               "Standard", paste0(percent((ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% "Standard - RUTF/FBF")]/ci.total.product.all$mean.prod[which(ci.total.product.all$cov %in% input$relapse_treat_prop & ci.total.product.all$variable22 %in% CurrentApproach)]))), icon = Standardicon,
                               color = colorforStandard, fill =TRUE#, width = 3
                             )
                           })
                         }
                         observeEvent(input$mamorsam,       
                                      ignoreNULL = F, { 
                                        if (input$mamorsam %in% "Just SAM") {
                                          #### Primary ####
                                          makePlot_Product_All <- function() {
                                            theme_set(theme_bw(15))
                                            ggplot(ci.total.product.all[ci.total.product.all$cov %in% input$relapse_treat_prop,] ,aes(x=variable22,y=mean.prod/150,fill=variable22)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  
                                              geom_errorbar(aes(ymin=(mean.prod -stdv.prod)/150 , ymax=(mean.prod +stdv.prod)/150 ), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab(paste("Total number of cartons"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733","#88CCEE")) +
                                              ggtitle("Cartons of Product to Treat SAM Children Per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")
                                          }
                                          
                                          output$Primary_Graph <- renderPlot({
                                            makePlot_Product_All()
                                          })# renderplot end
                                          output$downloadPlot_Primary_Graph <- downloadHandler(
                                            filename = function() {
                                              paste('plot-ProductNeeded-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_Product_All(), width = 6, height = 5, dpi = 300, units = "in")
                                            }
                                          )
                                          
                                          output$Primary_Table_Title <- renderText('Cartons of Product Needed, by Protocol')
                                          output$Note_uncertainty <- renderText(
                                            "Our model is built to randomly add some uncertainty to the cost inputs. We do this for 25 iterations. The Cartons of Product Needed
              in the table reflect the average results across the 25 iterations, and the standard deviation is also presented to reflect that
              the results should be interpreted as estimates within a range rather than exact values with certainty."
                                          )
                                          
                                          ci.total.product.all_CLEANED22 <- ci.total.product.all_CLEANED#[which(!(ci.total.product.all_CLEANED$Protocol %in% "Standard - CSB") ),]
                                          ci.total.product.all_CLEANED22[,2] <- format(round(ci.total.product.all_CLEANED22[,2]/150,0), big.mark=",")
                                          ci.total.product.all_CLEANED22[,3] <- format(round(ci.total.product.all_CLEANED22[,3]/150,0), big.mark=",")
                                          output$Primary_tbl = renderDT(
                                            ci.total.product.all_CLEANED22, options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Primary <- downloadHandler(
                                            filename = function() {
                                              paste('data-ProductNeeded', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(ci.total.product.all_CLEANED22, con)
                                            }
                                          )
                                        }
                                        
                                        if (input$mamorsam %in% "Both SAM and MAM") {
                                          
                                          makePlot_Product_All_SAM_MAM <- function() {
                                            theme_set(theme_bw(15))
                                            ggplot(prod.df[which(!(prod.df$variable22 %in% "MANGO")),],aes(x=variable22,y=mean.prod/150,fill=variable22)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  
                                              geom_errorbar(aes(ymin=(mean.prod - stdv.prod)/150, ymax=(mean.prod + stdv.prod)/150), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab(paste("Total number of cartons"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~variable6,nrow=1) +
                                              ggtitle("Cartons of Product to Treat Children,\nby Malnutrition Status and per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")
                                          }
                                          
                                          output$Primary_Graph <- renderPlot({
                                            makePlot_Product_All_SAM_MAM()
                                          })
                                          
                                          output$downloadPlot_CE <- downloadHandler(
                                            filename = function() {
                                              paste('plot-ProductNeeded-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_Product_All_SAM_MAM(), width = 9, height = 5, dpi = 300, units = "in")
                                            }
                                          )
                                          
                                          output$Primary_Table_Title <- renderText('Cartons of Product Needed, by Malnutrition Status and Protocol')
                                          output$Note_uncertainty <- renderText(
                                            "Our model is built to randomly add some uncertainty to the cost inputs. We do this for 25 iterations. The Cartons of Product Needed
              in the table reflect the average results across the 25 iterations, and the standard deviation is also presented to reflect that
              the results should be interpreted as estimates within a range rather than exact values with certainty."
                                          )
                                          
                                          ci.total.product_CLEANED[,2] <- format(round(ci.total.product_CLEANED[,2]/150,0), big.mark=",")
                                          ci.total.product_CLEANED[,3] <- format(round(ci.total.product_CLEANED[,3]/150,0), big.mark=",")
                                          output$Primary_tbl = renderDT(
                                            ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][order(ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][["Protocol"]]),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Primary <- downloadHandler(
                                            filename = function() {
                                              paste('data-ProductNeeded', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][order(ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][["Protocol"]]),], con)
                                            }
                                          )
                                        }
                                      })
                         
                       }
                       else if (input$ph_objective %in% "Reach more children with a fixed budget") {
                         
                         
                         #--  Run Fixed Budget Calc Model  --#  
                         cost.sch_asgiven <- f_DoseOpt_fixedbudget_calc(df_costm,
                                                                program.budget=input$budget.program*input$conv_rate,
                                                                scenario=1)
                         cost.sch_half <- f_DoseOpt_fixedbudget_calc(df_costm,
                                                                program.budget=input$budget.program*input$conv_rate/2,
                                                                scenario=2)
                         cost.sch_double <- f_DoseOpt_fixedbudget_calc(df_costm,
                                                                program.budget=input$budget.program*input$conv_rate*2,
                                                                scenario=3)
                         
                         cost.sch <- cbind(cost.sch_asgiven,cost.sch_half,cost.sch_double)
                         
                         
                         ### number of sachets consumed per child treated for a given budget
                         #### budget given
                         cost.sch$optimal.sachet.per.child.trt.s.givenbudget <- cost.sch$optimal.sachet.s.givenbudget/cost.sch$optimal.trt.s.givenbudget
                         cost.sch$optimal.sachet.per.child.trt.m.givenbudget <- cost.sch$optimal.sachet.m.givenbudget/cost.sch$optimal.trt.m.givenbudget
                         cost.sch$optimal.sachet.per.child.trt.s.csb.givenbudget <- cost.sch$optimal.sachet.per.child.trt.s.givenbudget
                         cost.sch$optimal.sachet.per.child.trt.m.csb.givenbudget <- cost.sch$optimal.sachet.m.csb.givenbudget/cost.sch$optimal.trt.m.csb.givenbudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s1.givenbudget <- cost.sch$optimal.sachet.s1.givenbudget/cost.sch$optimal.trt.s1.givenbudget
                         cost.sch$optimal.sachet.per.child.trt.m1.givenbudget <- cost.sch$optimal.sachet.m1.givenbudget/cost.sch$optimal.trt.m1.givenbudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s2.givenbudget <- cost.sch$optimal.sachet.s2.givenbudget/cost.sch$optimal.trt.s2.givenbudget
                         cost.sch$optimal.sachet.per.child.trt.m2.givenbudget <- cost.sch$optimal.sachet.m2.givenbudget/cost.sch$optimal.trt.m2.givenbudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s3.givenbudget <- cost.sch$optimal.sachet.s3.givenbudget/cost.sch$optimal.trt.s3.givenbudget
                         cost.sch$optimal.sachet.per.child.trt.m3.givenbudget <- cost.sch$optimal.sachet.m3.givenbudget/cost.sch$optimal.trt.m3.givenbudget
                         
                         #### budget half
                         cost.sch$optimal.sachet.per.child.trt.s.halfbudget <- cost.sch$optimal.sachet.s.halfbudget/cost.sch$optimal.trt.s.halfbudget
                         cost.sch$optimal.sachet.per.child.trt.m.halfbudget <- cost.sch$optimal.sachet.m.halfbudget/cost.sch$optimal.trt.m.halfbudget
                         cost.sch$optimal.sachet.per.child.trt.s.csb.halfbudget <- cost.sch$optimal.sachet.per.child.trt.s.halfbudget
                         cost.sch$optimal.sachet.per.child.trt.m.csb.halfbudget <- cost.sch$optimal.sachet.m.csb.halfbudget/cost.sch$optimal.trt.m.csb.halfbudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s1.halfbudget <- cost.sch$optimal.sachet.s1.halfbudget/cost.sch$optimal.trt.s1.halfbudget
                         cost.sch$optimal.sachet.per.child.trt.m1.halfbudget <- cost.sch$optimal.sachet.m1.halfbudget/cost.sch$optimal.trt.m1.halfbudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s2.halfbudget <- cost.sch$optimal.sachet.s2.halfbudget/cost.sch$optimal.trt.s2.halfbudget
                         cost.sch$optimal.sachet.per.child.trt.m2.halfbudget <- cost.sch$optimal.sachet.m2.halfbudget/cost.sch$optimal.trt.m2.halfbudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s3.halfbudget <- cost.sch$optimal.sachet.s3.halfbudget/cost.sch$optimal.trt.s3.halfbudget
                         cost.sch$optimal.sachet.per.child.trt.m3.halfbudget <- cost.sch$optimal.sachet.m3.halfbudget/cost.sch$optimal.trt.m3.halfbudget
                         
                         #### budget double
                         cost.sch$optimal.sachet.per.child.trt.s.doublebudget <- cost.sch$optimal.sachet.s.doublebudget/cost.sch$optimal.trt.s.doublebudget
                         cost.sch$optimal.sachet.per.child.trt.m.doublebudget <- cost.sch$optimal.sachet.m.doublebudget/cost.sch$optimal.trt.m.doublebudget
                         cost.sch$optimal.sachet.per.child.trt.s.csb.doublebudget <- cost.sch$optimal.sachet.per.child.trt.s.doublebudget
                         cost.sch$optimal.sachet.per.child.trt.m.csb.doublebudget <- cost.sch$optimal.sachet.m.csb.doublebudget/cost.sch$optimal.trt.m.csb.doublebudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s1.doublebudget <- cost.sch$optimal.sachet.s1.doublebudget/cost.sch$optimal.trt.s1.doublebudget
                         cost.sch$optimal.sachet.per.child.trt.m1.doublebudget <- cost.sch$optimal.sachet.m1.doublebudget/cost.sch$optimal.trt.m1.doublebudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s2.doublebudget <- cost.sch$optimal.sachet.s2.doublebudget/cost.sch$optimal.trt.s2.doublebudget
                         cost.sch$optimal.sachet.per.child.trt.m2.doublebudget <- cost.sch$optimal.sachet.m2.doublebudget/cost.sch$optimal.trt.m2.doublebudget
                         
                         cost.sch$optimal.sachet.per.child.trt.s3.doublebudget <- cost.sch$optimal.sachet.s3.doublebudget/cost.sch$optimal.trt.s3.doublebudget
                         cost.sch$optimal.sachet.per.child.trt.m3.doublebudget <- cost.sch$optimal.sachet.m3.doublebudget/cost.sch$optimal.trt.m3.doublebudget
                         
                         costm <- data.table(melt(cost.sch,measure.vars=c("optimal.sachet.per.child.trt.s.givenbudget" , "optimal.sachet.per.child.trt.s.csb.givenbudget", "optimal.sachet.per.child.trt.s1.givenbudget", "optimal.sachet.per.child.trt.s2.givenbudget","optimal.sachet.per.child.trt.s3.givenbudget", 
                                                                          "optimal.sachet.per.child.trt.m.givenbudget", "optimal.sachet.per.child.trt.m.csb.givenbudget", "optimal.sachet.per.child.trt.m1.givenbudget", "optimal.sachet.per.child.trt.m2.givenbudget", "optimal.sachet.per.child.trt.m3.givenbudget",  
                                                                          
                                                                          "optimal.sachet.per.child.trt.s.halfbudget" , "optimal.sachet.per.child.trt.s.csb.halfbudget", "optimal.sachet.per.child.trt.s1.halfbudget", "optimal.sachet.per.child.trt.s2.halfbudget","optimal.sachet.per.child.trt.s3.halfbudget", 
                                                                          "optimal.sachet.per.child.trt.m.halfbudget", "optimal.sachet.per.child.trt.m.csb.halfbudget", "optimal.sachet.per.child.trt.m1.halfbudget", "optimal.sachet.per.child.trt.m2.halfbudget", "optimal.sachet.per.child.trt.m3.halfbudget",    
                                                                          
                                                                          "optimal.sachet.per.child.trt.s.doublebudget" , "optimal.sachet.per.child.trt.s.csb.doublebudget", "optimal.sachet.per.child.trt.s1.doublebudget", "optimal.sachet.per.child.trt.s2.doublebudget","optimal.sachet.per.child.trt.s3.doublebudget", 
                                                                          "optimal.sachet.per.child.trt.m.doublebudget", "optimal.sachet.per.child.trt.m.csb.doublebudget", "optimal.sachet.per.child.trt.m1.doublebudget", "optimal.sachet.per.child.trt.m2.doublebudget", "optimal.sachet.per.child.trt.m3.doublebudget"),value.name = "value"))
                         
                         costm$variable2 <- ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s.givenbudget" , "optimal.sachet.per.child.trt.s.halfbudget" , "optimal.sachet.per.child.trt.s.doublebudget" , 
                                                                         "optimal.sachet.per.child.trt.m.givenbudget" , "optimal.sachet.per.child.trt.m.halfbudget" , "optimal.sachet.per.child.trt.m.doublebudget" ,
                                                                         "optimal.sachet.per.child.trt.s.csb.givenbudget", "optimal.sachet.per.child.trt.s.csb.halfbudget",  "optimal.sachet.per.child.trt.s.csb.doublebudget",
                                                                         "optimal.sachet.per.child.trt.m.csb.givenbudget", "optimal.sachet.per.child.trt.m.csb.halfbudget",  "optimal.sachet.per.child.trt.m.csb.doublebudget"), "Standard",
                                                   
                                                   ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s1.givenbudget" , "optimal.sachet.per.child.trt.s1.halfbudget" ,"optimal.sachet.per.child.trt.s1.doublebudget", 
                                                                                "optimal.sachet.per.child.trt.m1.givenbudget" ,"optimal.sachet.per.child.trt.m1.halfbudget" , "optimal.sachet.per.child.trt.m1.doublebudget"), "OptiMA",
                                                          
                                                          ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s2.givenbudget" , "optimal.sachet.per.child.trt.s2.halfbudget" , "optimal.sachet.per.child.trt.s2.doublebudget", 
                                                                                       "optimal.sachet.per.child.trt.m2.givenbudget" , "optimal.sachet.per.child.trt.m2.halfbudget" ,"optimal.sachet.per.child.trt.m2.doublebudget"),"ComPAS","MANGO")))
                         
                         
                         costm$variable3 <- ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s.givenbudget" , "optimal.sachet.per.child.trt.s.csb.givenbudget", "optimal.sachet.per.child.trt.s1.givenbudget", "optimal.sachet.per.child.trt.s2.givenbudget","optimal.sachet.per.child.trt.s3.givenbudget", 
                                                                         "optimal.sachet.per.child.trt.m.givenbudget", "optimal.sachet.per.child.trt.m.csb.givenbudget", "optimal.sachet.per.child.trt.m1.givenbudget", "optimal.sachet.per.child.trt.m2.givenbudget", "optimal.sachet.per.child.trt.m3.givenbudget"), "Given",
                                                   
                                                   ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s.halfbudget" , "optimal.sachet.per.child.trt.s.csb.halfbudget", "optimal.sachet.per.child.trt.s1.halfbudget", "optimal.sachet.per.child.trt.s2.halfbudget","optimal.sachet.per.child.trt.s3.halfbudget", 
                                                                                "optimal.sachet.per.child.trt.m.halfbudget", "optimal.sachet.per.child.trt.m.csb.halfbudget", "optimal.sachet.per.child.trt.m1.halfbudget", "optimal.sachet.per.child.trt.m2.halfbudget", "optimal.sachet.per.child.trt.m3.halfbudget"), "Half",
                                                          
                                                          ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s.doublebudget" , "optimal.sachet.per.child.trt.s.csb.doublebudget", "optimal.sachet.per.child.trt.s1.doublebudget", "optimal.sachet.per.child.trt.s2.doublebudget","optimal.sachet.per.child.trt.s3.doublebudget", 
                                                                                       "optimal.sachet.per.child.trt.m.doublebudget", "optimal.sachet.per.child.trt.m.csb.doublebudget", "optimal.sachet.per.child.trt.m1.doublebudget", "optimal.sachet.per.child.trt.m2.doublebudget", "optimal.sachet.per.child.trt.m3.doublebudget"),"Double","")))
                         
                         costm$variable22 <- ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s.givenbudget" , "optimal.sachet.per.child.trt.s.halfbudget" , "optimal.sachet.per.child.trt.s.doublebudget", 
                                                                          "optimal.sachet.per.child.trt.m.givenbudget" , "optimal.sachet.per.child.trt.m.halfbudget" , "optimal.sachet.per.child.trt.m.doublebudget"), "Standard",
                                                    
                                                    ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s.csb.givenbudget", "optimal.sachet.per.child.trt.s.csb.halfbudget",  "optimal.sachet.per.child.trt.s.csb.doublebudget",
                                                                                 "optimal.sachet.per.child.trt.m.csb.givenbudget", "optimal.sachet.per.child.trt.m.csb.halfbudget",  "optimal.sachet.per.child.trt.m.csb.doublebudget"),"Standard - CSB",
                                                           
                                                           ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s1.givenbudget" , "optimal.sachet.per.child.trt.s1.halfbudget" ,"optimal.sachet.per.child.trt.s1.doublebudget" , 
                                                                                        "optimal.sachet.per.child.trt.m1.givenbudget" ,"optimal.sachet.per.child.trt.m1.halfbudget" , "optimal.sachet.per.child.trt.m1.doublebudget"), "OptiMA",
                                                                  
                                                                  ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s2.givenbudget" , "optimal.sachet.per.child.trt.s2.halfbudget" , "optimal.sachet.per.child.trt.s2.doublebudget",
                                                                                               "optimal.sachet.per.child.trt.m2.givenbudget" , "optimal.sachet.per.child.trt.m2.halfbudget" ,"optimal.sachet.per.child.trt.m2.doublebudget"),"ComPAS","MANGO"))))
                         
                         
                         substrRight <- function(x, n){
                           substr(x, nchar(x)-n+1, nchar(x))
                         }
                         
                         costm[,variable4 := paste(round(weight.adm.s), "kg")] # weight at admission
                         costm[,variable5 := paste(round(muac.s), "mm")] #muac at admission
                         
                         costm$variable6 <- ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.s.givenbudget" , "optimal.sachet.per.child.trt.s1.givenbudget", "optimal.sachet.per.child.trt.s2.givenbudget","optimal.sachet.per.child.trt.s3.givenbudget", 
                                                                         "optimal.sachet.per.child.trt.s.halfbudget" , "optimal.sachet.per.child.trt.s1.halfbudget", "optimal.sachet.per.child.trt.s2.halfbudget","optimal.sachet.per.child.trt.s3.halfbudget", 
                                                                         "optimal.sachet.per.child.trt.s.doublebudget" , "optimal.sachet.per.child.trt.s1.doublebudget", "optimal.sachet.per.child.trt.s2.doublebudget","optimal.sachet.per.child.trt.s3.doublebudget",
                                                                         "optimal.sachet.per.child.trt.s.csb.givenbudget", "optimal.sachet.per.child.trt.s.csb.halfbudget",  "optimal.sachet.per.child.trt.s.csb.doublebudget"),"Severe Wasting",
                                                   
                                                   ifelse(costm$variable %in% c("optimal.sachet.per.child.trt.m.halfbudget",  "optimal.sachet.per.child.trt.m1.halfbudget", "optimal.sachet.per.child.trt.m2.halfbudget", "optimal.sachet.per.child.trt.m3.halfbudget",    
                                                                                "optimal.sachet.per.child.trt.m.givenbudget","optimal.sachet.per.child.trt.m1.givenbudget", "optimal.sachet.per.child.trt.m2.givenbudget", "optimal.sachet.per.child.trt.m3.givenbudget",  
                                                                                "optimal.sachet.per.child.trt.m.doublebudget","optimal.sachet.per.child.trt.m1.doublebudget", "optimal.sachet.per.child.trt.m2.doublebudget", "optimal.sachet.per.child.trt.m3.doublebudget"),
                                                          "Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB"))
                         
                         costm$variable6[which(costm$variable6 %in% c("Severe Wasting"))] <- "SAM"
                         costm$variable6[which(costm$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB"))] <- "MAM"
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){
                           costm <- costm[-which(costm$variable22 %in% "Standard - CSB"),]
                           costm$variable22[which(costm$variable22 %in% "Standard")] <- "Standard - RUTF/RUSF"
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){
                           costm <- costm[-which(costm$variable22 %in% "Standard"),]
                           costm$variable22[which(costm$variable22 %in% "Standard - CSB")] <- "Standard - RUTF/FBF"
                         }
                         if (input$mamorsam %in% "Just SAM"){
                           costm <- costm[-which(costm$variable22 %in% "Standard - CSB"),]
                         }
                         ### 
                         ### sachet 
                         ### 
                         pop.sachet2 <- data.frame(costm %>% group_by(variable22,variable3,variable4,variable5,variable6,cov) %>% 
                                                     dplyr::summarise(mean.sch = mean(value),stdv.sch = sd(value)))
                         
                         #pop.sachet2$variable22 <-factor(pop.sachet2$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
                         pop.sachet2$variable3 <-factor(pop.sachet2$variable3,levels=c("Given","Half","Double"))
                         
                         ## 1 packet of csb used for 6 days
                         pop.sachet2$mean.sch <- ifelse(pop.sachet2$variable22 == "Standard - RUTF/FBF" & pop.sachet2$variable6 == "MAM",pop.sachet2$mean.sch/6,pop.sachet2$mean.sch)
                         pop.sachet2$stdv.sch <- ifelse(pop.sachet2$variable22 == "Standard - RUTF/FBF" & pop.sachet2$variable6 == "MAM",pop.sachet2$stdv.sch/6,pop.sachet2$stdv.sch)
                         
                         ci.sachet.all <- data.frame(costm %>% group_by(variable22,variable3,variable4,variable5,cov) %>% 
                                                           dplyr::summarise(mean.sch = mean(value),stdv.sch = sd(value)))
                         
                         ci.sachet <- data.frame(costm %>% group_by(variable22,variable3,variable4,variable5,variable6,cov) %>% 
                                                       dplyr::summarise(mean.sch = mean(value),stdv.sch = sd(value)))
                         
                         if (input$mamorsam %in% "Just SAM") {
                           ci.sachet.all <- ci.sachet[which(ci.sachet$variable6 %in% "SAM"),]
                         }
                         
                         #ci.sachet.all$variable22 <-factor(ci.sachet.all$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
                         ci.sachet.all$variable3 <-factor(ci.sachet.all$variable3,levels=c("Given","Half","Double"))
                         ci.sachet.all$variable33 <- rep(NA,dim(ci.sachet.all)[1])
                         ci.sachet.all$variable33[which(ci.sachet.all$variable3 %in% "Given")] <- paste(scales::dollar(input$budget.program),Currencyforoutputs)
                         ci.sachet.all$variable33[which(ci.sachet.all$variable3 %in% "Half")] <- paste(scales::dollar(input$budget.program/2),Currencyforoutputs)
                         ci.sachet.all$variable33[which(ci.sachet.all$variable3 %in% "Double")] <- paste(scales::dollar(input$budget.program*2),Currencyforoutputs)
                         #ci.sachet$variable22 <-factor(ci.sachet$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
                         ci.sachet$variable3 <-factor(ci.sachet$variable3,levels=c("Given","Half","Double"))
                         ci.sachet$variable33 <- rep(NA,dim(ci.sachet)[1])
                         ci.sachet$variable33[which(ci.sachet$variable3 %in% "Given")] <- paste(scales::dollar(input$budget.program),Currencyforoutputs)
                         ci.sachet$variable33[which(ci.sachet$variable3 %in% "Half")] <- paste(scales::dollar(input$budget.program/2),Currencyforoutputs)
                         ci.sachet$variable33[which(ci.sachet$variable3 %in% "Double")] <- paste(scales::dollar(input$budget.program*2),Currencyforoutputs)
                         
                         ci.sachet.all_CLEANED2 = subset(ci.sachet.all, select = -c(variable3,variable4,variable5) )
                         ci.sachet.all_CLEANED2 = ci.sachet.all_CLEANED2[which(ci.sachet.all_CLEANED2$cov %in% input$relapse_treat_prop),]
                         ci.sachet.all_CLEANED <- data.frame(Protocol = ci.sachet.all_CLEANED2$variable22,
                                                                 Budget = ci.sachet.all_CLEANED2$variable33,
                                                                 `Average Sachets per Treated Child` = round(ci.sachet.all_CLEANED2$mean.sch,0),
                                                                 `SD for Sachets per Treated Child` = round(ci.sachet.all_CLEANED2$stdv.sch,0))
                         
                         ci.sachet_CLEANED2 = subset(ci.sachet, select = -c(variable3,variable4,variable5) )
                         ci.sachet_CLEANED2 = ci.sachet_CLEANED2[which(ci.sachet_CLEANED2$cov %in% input$relapse_treat_prop),]
                         ci.sachet_CLEANED3 <- data.frame(Protocol = ci.sachet_CLEANED2$variable22,
                                                              Budget = ci.sachet_CLEANED2$variable33,
                                                              `Average Sachets per Treated Child` = round(ci.sachet_CLEANED2$mean.sch,0),
                                                              `SD for Sachets per Treated Child` = round(ci.sachet_CLEANED2$stdv.sch,0),
                                                              `Malnutrition Status` = ci.sachet_CLEANED2$variable6)
                         ci.sachet_CLEANED <- ci.sachet_CLEANED3
                         names(ci.sachet.all_CLEANED) <- c("Protocol","Budget","Average Sachets per Treated Child","SD for Sachets per Treated Child")
                         names(ci.sachet_CLEANED) <- c("Protocol","Budget","Average Sachets per Treated Child","SD for Sachets per Treated Child","Malnutrition Status")
                         
                         ###################################  ###################################  ###################################  ###################################
                         
                         ### number of wasted children can be treated for a given budget
                         cost.sch$optimal.trt.s.csb.givenbudget <- cost.sch$optimal.trt.s.givenbudget
                         cost.sch$optimal.trt.s.csb.halfbudget <- cost.sch$optimal.trt.s.halfbudget
                         cost.sch$optimal.trt.s.csb.doublebudget <- cost.sch$optimal.trt.s.doublebudget
                         costmnumber <- data.table(melt(cost.sch,measure.vars=c("optimal.trt.s.givenbudget" , "optimal.trt.s.csb.givenbudget", "optimal.trt.s1.givenbudget", "optimal.trt.s2.givenbudget","optimal.trt.s3.givenbudget", 
                                                                          "optimal.trt.m.givenbudget", "optimal.trt.m.csb.givenbudget", "optimal.trt.m1.givenbudget", "optimal.trt.m2.givenbudget", "optimal.trt.m3.givenbudget",  
                                                                          
                                                                          "optimal.trt.s.halfbudget", "optimal.trt.s.csb.halfbudget" , "optimal.trt.s1.halfbudget", "optimal.trt.s2.halfbudget","optimal.trt.s3.halfbudget", 
                                                                          "optimal.trt.m.halfbudget", "optimal.trt.m.csb.halfbudget", "optimal.trt.m1.halfbudget", "optimal.trt.m2.halfbudget", "optimal.trt.m3.halfbudget",    
                                                                          
                                                                          "optimal.trt.s.doublebudget", "optimal.trt.s.csb.doublebudget" , "optimal.trt.s1.doublebudget", "optimal.trt.s2.doublebudget","optimal.trt.s3.doublebudget", 
                                                                          "optimal.trt.m.doublebudget", "optimal.trt.m.csb.doublebudget", "optimal.trt.m1.doublebudget", "optimal.trt.m2.doublebudget", "optimal.trt.m3.doublebudget"),value.name = "value"))
                         
                         costmnumber$variable2 <- ifelse(costmnumber$variable %in% c("optimal.trt.s.givenbudget" , "optimal.trt.s.halfbudget" , "optimal.trt.s.doublebudget" , 
                                                                         "optimal.trt.m.givenbudget" , "optimal.trt.m.halfbudget" , "optimal.trt.m.doublebudget" ,
                                                                         "optimal.trt.s.csb.givenbudget", "optimal.trt.s.csb.halfbudget",  "optimal.trt.s.csb.doublebudget",
                                                                         "optimal.trt.m.csb.givenbudget", "optimal.trt.m.csb.halfbudget",  "optimal.trt.m.csb.doublebudget"), "Standard",
                                                   
                                                   ifelse(costmnumber$variable %in% c("optimal.trt.s1.givenbudget" , "optimal.trt.s1.halfbudget" ,"optimal.trt.s1.doublebudget" ,
                                                                                "optimal.trt.m1.givenbudget" ,"optimal.trt.m1.halfbudget" , "optimal.trt.m1.doublebudget"), "OptimA",
                                                          
                                                          ifelse(costmnumber$variable %in% c("optimal.trt.s2.givenbudget" , "optimal.trt.s2.halfbudget" , "optimal.trt.s2.doublebudget" ,
                                                                                       "optimal.trt.m2.givenbudget" , "optimal.trt.m2.halfbudget" ,"optimal.trt.m2.doublebudget"),"ComPAS","MANGO")))
                         
                         
                         costmnumber$variable3 <- ifelse(costmnumber$variable %in% c("optimal.trt.s.givenbudget", "optimal.trt.s.csb.givenbudget" , "optimal.trt.s1.givenbudget", "optimal.trt.s2.givenbudget","optimal.trt.s3.givenbudget", 
                                                                         "optimal.trt.m.givenbudget", "optimal.trt.m.csb.givenbudget", "optimal.trt.m1.givenbudget", "optimal.trt.m2.givenbudget", "optimal.trt.m3.givenbudget"), "Given",
                                                   
                                                   ifelse(costmnumber$variable %in% c("optimal.trt.s.halfbudget", "optimal.trt.s.csb.halfbudget" , "optimal.trt.s1.halfbudget", "optimal.trt.s2.halfbudget","optimal.trt.s3.halfbudget", 
                                                                                "optimal.trt.m.halfbudget", "optimal.trt.m.csb.halfbudget", "optimal.trt.m1.halfbudget", "optimal.trt.m2.halfbudget", "optimal.trt.m3.halfbudget"), "Half",
                                                          
                                                          ifelse(costmnumber$variable %in% c("optimal.trt.s.doublebudget", "optimal.trt.s.csb.doublebudget" , "optimal.trt.s1.doublebudget", "optimal.trt.s2.doublebudget","optimal.trt.s3.doublebudget", 
                                                                                       "optimal.trt.m.doublebudget", "optimal.trt.m.csb.doublebudget", "optimal.trt.m1.doublebudget", "optimal.trt.m2.doublebudget", "optimal.trt.m3.doublebudget"),"Double","")))
                         
                         costmnumber$variable22 <- ifelse(costmnumber$variable %in% c("optimal.trt.s.givenbudget" , "optimal.trt.s.halfbudget" , "optimal.trt.s.doublebudget", 
                                                                          "optimal.trt.m.givenbudget" , "optimal.trt.m.halfbudget" , "optimal.trt.m.doublebudget"), "Standard",
                                                    
                                                    ifelse(costmnumber$variable %in% c("optimal.trt.s.csb.givenbudget", "optimal.trt.s.csb.halfbudget",  "optimal.trt.s.csb.doublebudget",
                                                                                       "optimal.trt.m.csb.givenbudget", "optimal.trt.m.csb.halfbudget",  "optimal.trt.m.csb.doublebudget"),"Standard - CSB",
                                                           
                                                           ifelse(costmnumber$variable %in% c("optimal.trt.s1.givenbudget" , "optimal.trt.s1.halfbudget" ,"optimal.trt.s1.doublebudget", 
                                                                                        "optimal.trt.m1.givenbudget" ,"optimal.trt.m1.halfbudget" , "optimal.trt.m1.doublebudget"), "OptiMA",
                                                                  
                                                                  ifelse(costmnumber$variable %in% c("optimal.trt.s2.givenbudget" , "optimal.trt.s2.halfbudget" , "optimal.trt.s2.doublebudget" , 
                                                                                               "optimal.trt.m2.givenbudget" , "optimal.trt.m2.halfbudget" ,"optimal.trt.m2.doublebudget"),"ComPAS","MANGO"))))
                         
                         
                         substrRight <- function(x, n){
                           substr(x, nchar(x)-n+1, nchar(x))
                         }
                         
                         costmnumber[,variable4 := paste(round(weight.adm.s), "kg")] # weight at admission
                         costmnumber[,variable5 := paste(round(muac.s), "mm")] #muac at admission
                         
                         costmnumber$variable6 <- ifelse(costmnumber$variable %in% c("optimal.trt.s.givenbudget" , "optimal.trt.s1.givenbudget", "optimal.trt.s2.givenbudget","optimal.trt.s3.givenbudget", 
                                                                                     "optimal.trt.s.csb.givenbudget", "optimal.trt.s.csb.halfbudget",  "optimal.trt.s.csb.doublebudget",
                                                                         "optimal.trt.s.halfbudget" , "optimal.trt.s1.halfbudget", "optimal.trt.s2.halfbudget","optimal.trt.s3.halfbudget", 
                                                                         "optimal.trt.s.doublebudget" , "optimal.trt.s1.doublebudget", "optimal.trt.s2.doublebudget","optimal.trt.s3.doublebudget"),"Severe Wasting",
                                                   
                                                   ifelse(costmnumber$variable %in% c("optimal.trt.m.halfbudget",  "optimal.trt.m1.halfbudget", "optimal.trt.m2.halfbudget", "optimal.trt.m3.halfbudget",    
                                                                                "optimal.trt.m.givenbudget","optimal.trt.m1.givenbudget", "optimal.trt.m2.givenbudget", "optimal.trt.m3.givenbudget",  
                                                                                "optimal.trt.m.doublebudget","optimal.trt.m1.doublebudget", "optimal.trt.m2.doublebudget", "optimal.trt.m3.doublebudget"),
                                                          "Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB"))
                         
                         costmnumber$variable6[which(costmnumber$variable6 %in% c("Severe Wasting"))] <- "SAM"
                         costmnumber$variable6[which(costmnumber$variable6 %in% c("Moderate Wasting - RUSF/RUTF","Moderate Wasting - CSB"))] <- "MAM"
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){
                           costmnumber <- costmnumber[-which(costmnumber$variable22 %in% "Standard - CSB"),]
                           costmnumber$variable22[which(costmnumber$variable22 %in% "Standard")] <- "Standard - RUTF/RUSF"
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){
                           costmnumber <- costmnumber[-which(costmnumber$variable22 %in% "Standard"),]
                           costmnumber$variable22[which(costmnumber$variable22 %in% "Standard - CSB")] <- "Standard - RUTF/FBF"
                         }
                         if (input$mamorsam %in% "Just SAM"){
                           costmnumber <- costmnumber[-which(costmnumber$variable22 %in% "Standard - CSB"),]
                         }
                         ### 
                         ### pop 
                         ### 
                         pop.sachet <- data.frame(costmnumber %>% group_by(variable22,variable3,variable4,variable5,variable6,cov) %>% 
                                                    dplyr::summarise(mean.trt = mean(value),stdv.trt = sd(value)))
                      
                         #pop.sachet$variable22 <-factor(pop.sachet$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
                         pop.sachet$variable3 <-factor(pop.sachet$variable3,levels=c("Given","Half","Double"))
                         
                         ci.pop.sachet.all <- data.frame(costmnumber %>% group_by(variable22,variable3,variable4,variable5,cov) %>% 
                                                           dplyr::summarise(mean.trt = mean(value),stdv.trt = sd(value)))
                         
                         ci.pop.sachet <- data.frame(costmnumber %>% group_by(variable22,variable3,variable4,variable5,variable6,cov) %>% 
                                                       dplyr::summarise(mean.trt = mean(value),stdv.trt = sd(value)))
                         
                         if (input$mamorsam %in% "Just SAM") {
                           ci.pop.sachet.all <- ci.pop.sachet[which(ci.pop.sachet$variable6 %in% "SAM"),]
                         }
                         
                         #ci.pop.sachet.all$variable22 <-factor(ci.pop.sachet.all$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
                         ci.pop.sachet.all$variable3 <-factor(ci.pop.sachet.all$variable3,levels=c("Given","Half","Double"))
                         ci.pop.sachet.all$variable33 <- rep(NA,dim(ci.pop.sachet.all)[1])
                         ci.pop.sachet.all$variable33[which(ci.pop.sachet.all$variable3 %in% "Given")] <- paste(scales::dollar(input$budget.program),Currencyforoutputs)
                         ci.pop.sachet.all$variable33[which(ci.pop.sachet.all$variable3 %in% "Half")] <- paste(scales::dollar(input$budget.program/2),Currencyforoutputs)
                         ci.pop.sachet.all$variable33[which(ci.pop.sachet.all$variable3 %in% "Double")] <- paste(scales::dollar(input$budget.program*2),Currencyforoutputs)
                         #ci.pop.sachet$variable22 <-factor(ci.pop.sachet$variable22,levels=c("Standard","Standard - CSB","OptiMA","ComPAS", "MANGO"))
                         ci.pop.sachet$variable3 <-factor(ci.pop.sachet$variable3,levels=c("Given","Half","Double"))
                         ci.pop.sachet$variable33 <- rep(NA,dim(ci.pop.sachet)[1])
                         ci.pop.sachet$variable33[which(ci.pop.sachet$variable3 %in% "Given")] <- paste(scales::dollar(input$budget.program),Currencyforoutputs)
                         ci.pop.sachet$variable33[which(ci.pop.sachet$variable3 %in% "Half")] <- paste(scales::dollar(input$budget.program/2),Currencyforoutputs)
                         ci.pop.sachet$variable33[which(ci.pop.sachet$variable3 %in% "Double")] <- paste(scales::dollar(input$budget.program*2),Currencyforoutputs)
                         
                         ci.pop.sachet.all_CLEANED2 = subset(ci.pop.sachet.all, select = -c(variable3,variable4,variable5) )
                         ci.pop.sachet.all_CLEANED2 = ci.pop.sachet.all_CLEANED2[which(ci.pop.sachet.all_CLEANED2$cov %in% input$relapse_treat_prop),]
                         ci.pop.sachet.all_CLEANED <- data.frame(Protocol = ci.pop.sachet.all_CLEANED2$variable22,
                                                                 Budget = ci.pop.sachet.all_CLEANED2$variable33,
                                                                    `Average Children Treated` = round(ci.pop.sachet.all_CLEANED2$mean.trt,0),
                                                                    `SD for Children Treated` = round(ci.pop.sachet.all_CLEANED2$stdv.trt,0))
                         
                         ci.pop.sachet_CLEANED2 = subset(ci.pop.sachet, select = -c(variable3,variable4,variable5) )
                         ci.pop.sachet_CLEANED2 = ci.pop.sachet_CLEANED2[which(ci.pop.sachet_CLEANED2$cov %in% input$relapse_treat_prop),]
                         ci.pop.sachet_CLEANED3 <- data.frame(Protocol = ci.pop.sachet_CLEANED2$variable22,
                                                              Budget = ci.pop.sachet_CLEANED2$variable33,
                                                                 `Average Children Treated` = round(ci.pop.sachet_CLEANED2$mean.trt,0),
                                                                 `SD for Children Treated` = round(ci.pop.sachet_CLEANED2$stdv.trt,0),
                                                                 `Malnutrition Status` = ci.pop.sachet_CLEANED2$variable6)
                         #ci.total.product.all_CLEANED3 <- ci.total.product.all_CLEANED
                         #ci.total.product.all_CLEANED3$Malnutrition.Status <- rep("All",dim(ci.total.product.all_CLEANED3)[1])
                         ci.pop.sachet_CLEANED <- ci.pop.sachet_CLEANED3#rbind(ci.total.product.all_CLEANED3,ci.total.product_CLEANED3)
                         names(ci.pop.sachet.all_CLEANED) <- c("Protocol","Budget","Average Children Treated","SD for Children Treated")
                         names(ci.pop.sachet_CLEANED) <- c("Protocol","Budget","Average Children Treated","SD for Children Treated","Malnutrition Status")
                         

                         output$Interpretation_Primary <-  renderText({gsub(pattern = "\\n", replacement = "<br/>", 
                                                                         paste("Given a fixed budget of", scales::dollar(input$budget.program), Currencyforoutputs, ", an average ", round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable3 %in% "Given")],0) ,"children could be treated under the ",CurrentApproach," protocol, assuming ",scales::percent(input$cov_s), " treatment coverage for SAM children and ",scales::percent(input$cov_m), "treatment coverage for MAM children. Note: you can go back to the previous tab to change the coverage estimates for your context and rerun the model."
                                                                               
                                                                         ))
                         })
                         output$Relative_to_Current <-  renderText({gsub(pattern = "\\n", replacement = "<br/>", 
                                                                         paste("How the amount of product needed per the current protocol relates to needs for other protocols is depicted here. At the given budget, a protocol with a 75% would treat 25% fewer children than the current protocol, while a protocol with 125% would treat 25% more children than the current protocol."
                                                                         ))
                         })
                         if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "ComPAS" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) >1){
                           colorforCompas = "green"
                           Compasicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "ComPAS" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) <1){
                           colorforCompas = "red"
                           Compasicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "ComPAS" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) == 1){
                           colorforCompas = "blue"
                           Compasicon = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "MANGO" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) >1){
                           colorforMango = "green"
                           Mangoicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "MANGO" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) <1){
                           colorforMango = "red"
                           Mangoicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "MANGO" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) == 1){
                           colorforMango = "blue"
                           Mangoicon = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "OptiMA" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) >1){
                           colorforOptima = "green"
                           Optimaicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "OptiMA" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) <1){
                           colorforOptima = "red"
                           Optimaicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "OptiMA" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) == 1){
                           colorforOptima = "blue"
                           Optimaicon = icon("equals", lib = "font-awesome")}
                         
                         if (input$mamorsam %in% "Just SAM"){
                           if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) >1){
                             colorforStandard = "green"
                             Standardicon = icon("angle-double-up", lib = "font-awesome")}
                           else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) <1){
                             colorforStandard = "red"
                             Standardicon = icon("angle-double-down", lib = "font-awesome")}
                           else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) == 1){
                             colorforStandard = "blue"
                             Standardicon = icon("equals", lib = "font-awesome")}
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){          
                           if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/RUSF" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) >1){
                             colorforStandard = "green"
                             Standardicon = icon("angle-double-up", lib = "font-awesome")}
                           else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/RUSF" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) <1){
                             colorforStandard = "red"
                             Standardicon = icon("angle-double-down", lib = "font-awesome")}
                           else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/RUSF" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) == 1){
                             colorforStandard = "blue"
                             Standardicon = icon("equals", lib = "font-awesome")}
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){          
                           if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/FBF" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) >1){
                             colorforStandard = "green"
                             Standardicon = icon("angle-double-up", lib = "font-awesome")}
                           else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/FBF" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) <1){
                             colorforStandard = "red"
                             Standardicon = icon("angle-double-down", lib = "font-awesome")}
                           else if (round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/FBF" & ci.pop.sachet.all$variable3 %in% "Given")],0)/round(ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")],0) == 1){
                             colorforStandard = "blue"
                             Standardicon = icon("equals", lib = "font-awesome")}
                         }

                         
                         output$ComPASBox <- renderInfoBox({
                           infoBox(
                             "ComPAS", paste0(percent((ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "ComPAS" & ci.pop.sachet.all$variable3 %in% "Given")]/ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")]))), icon = Compasicon,
                             color = colorforCompas, fill =TRUE#, width = 3
                           )
                         })
                         if (input$mamorsam %in% "Just SAM") {
                         output$MANGOBox　<- renderInfoBox({
                           infoBox(
                             "MANGO", paste0(percent((ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "MANGO" & ci.pop.sachet.all$variable3 %in% "Given")]/ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")]))), icon = Mangoicon,
                             color = colorforMango, fill =TRUE#, width = 3
                           )
                         })
                         }
                         output$OptiMABox <-　renderInfoBox({
                           infoBox(
                             "OptiMA", paste0(percent((ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "OptiMA" & ci.pop.sachet.all$variable3 %in% "Given")]/ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")]))), icon = Optimaicon,
                             color = colorforOptima, fill =TRUE#, width = 3
                           )
                         })

                         if (input$mamorsam %in% "Just SAM"){
                           output$StandardBox <- renderInfoBox({
                             infoBox(
                               "Standard", paste0(percent((ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard" & ci.pop.sachet.all$variable3 %in% "Given")]/ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")]))), icon = Standardicon,
                               color = colorforStandard, fill =TRUE#, width = 3
                             )
                           })
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "RUSF"){          
                           output$StandardBox <- renderInfoBox({
                             infoBox(
                               "Standard", paste0(percent((ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/RUSF" & ci.pop.sachet.all$variable3 %in% "Given")]/ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")]))), icon = Standardicon,
                               color = colorforStandard, fill =TRUE#, width = 3
                             )
                           })
                         }
                         if (input$mamorsam %in% "Both SAM and MAM" & input$standard_mam_product %in% "CSB"){          
                           output$StandardBox <- renderInfoBox({
                             infoBox(
                               "Standard", paste0(percent((ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% "Standard - RUTF/FBF" & ci.pop.sachet.all$variable3 %in% "Given")]/ci.pop.sachet.all$mean.trt[which(ci.pop.sachet.all$cov %in% input$relapse_treat_prop & ci.pop.sachet.all$variable22 %in% CurrentApproach & ci.pop.sachet.all$variable3 %in% "Given")]))), icon = Standardicon,
                               color = colorforStandard, fill =TRUE#, width = 3
                             )
                           })
                         }
                         
                         observeEvent(input$mamorsam,       
                                      ignoreNULL = F, { 
                                        if (input$mamorsam %in% "Just SAM") {
                                          #### Primary ####
                                          makePlot_NumberReached_All <- function() {
                                            theme_set(theme_bw(15))
                                            ggplot(ci.pop.sachet.all[ci.pop.sachet.all$cov %in% input$relapse_treat_prop
                                                                     ,] ,aes(x=variable22,y=mean.trt,fill=variable22)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  
                                              geom_errorbar(aes(ymin=mean.trt -stdv.trt , ymax=mean.trt +stdv.trt ), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab(paste("Total number of children treated"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733","#88CCEE")) + facet_wrap(~variable33) + guides(fill="none")
                                              #ggtitle("Number of SAM Children Treated, by Budget and Per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")
                                          }
                                          
                                          output$Primary_Graph <- renderPlot({
                                            makePlot_NumberReached_All()
                                          })# renderplot end
                                          output$downloadPlot_Primary_Graph <- downloadHandler(
                                            filename = function() {
                                              paste('plot-ChildrenTreated-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_NumberReached_All(), width = 9, height = 5, dpi = 300, units = "in")
                                            }
                                          )
                                          
                                          output$Primary_Table_Title <- renderText('Children Treated, by Budget and per Protocol')
                                          output$Note_uncertainty <- renderText(
                                            "Our model is built to randomly add some uncertainty to the cost inputs. We do this for 25 iterations. The Number of Children Treated
              in the table reflect the average results across the 25 iterations, and the standard deviation is also presented to reflect that
              the results should be interpreted as estimates within a range rather than exact values with certainty."
                                          )
                                          
                                          output$Primary_tbl = renderDT(
                                            ci.pop.sachet.all_CLEANED, options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Primary <- downloadHandler(
                                            filename = function() {
                                              paste('data-ChildrenTreated', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(ci.pop.sachet.all_CLEANED, con)
                                            }
                                          )
                                          
                                            #### Secondary ####
                                            output$Secondary_Results_Title_Children_Treated <- renderText("Number of Sachets per SAM Child Treated, by Budget and per Protocol")
                                            makePlot_ProgramCosts_All_Children_Treated <- function() {
                                              theme_set(theme_bw(15))
                                              ggplot(ci.sachet.all[ci.sachet.all$cov %in% input$relapse_treat_prop 
                                                                   ,] ,aes(x=variable22,y=mean.sch,fill=variable22)) +
                                                geom_bar(stat="identity", color="black", position=position_dodge()) +
                                                geom_errorbar(aes(ymin=mean.sch -stdv.sch , ymax=mean.sch +stdv.sch ), width=.2, position=position_dodge(.9))+
                                                xlab("")+ ylab(paste("Number of sachets per treated child"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
                                                scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733","#88CCEE")) + facet_wrap(~variable33) + guides(fill="none") #+
                                              #ggtitle("Number of Sachets per SAM Child Treated, by Budget and per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")
                                            }
                                            output$Secondary_Graph_Children_Treated <- renderPlot({
                                              makePlot_ProgramCosts_All_Children_Treated()
                                            })
                                            
                                            output$downloadPlot_Secondary_Graph_Children_Treated <- downloadHandler(
                                              filename = function() {
                                                paste('plot-SachetsPerChildFixedBudget-', Sys.Date(), '.pdf', sep='')
                                              },
                                              content = function(file) {
                                                
                                                #telemetry tracking
                                                log_to_database("download", list(
                                                  input_id = "downloadPlot",
                                                  download_type = "plot",
                                                  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                                ))
                                                
                                                ggsave(file, makePlot_ProgramCosts_All_Children_Treated(), width = 6, height = 5, dpi = 300, units = "in")
                                              }
                                            )
                                            output$Secondary_Table_Title_Children_Treated <- renderText('Sachets per Child, by Budget and per Protocol')
                                            ci.sachet.all_CLEANED2 <- ci.sachet.all_CLEANED#[which(!(ci.sachet.all_CLEANED$Protocol %in% "Standard - CSB") ),]
                                            output$Secondary_tbl_Children_Treated = renderDT(
                                              ci.sachet.all_CLEANED2, options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                            )
                                            output$downloadDataTable_Secondary_Children_Treated <- downloadHandler(
                                              filename = function() {
                                                paste('data-SachetsPerChildFixedBudget', Sys.Date(), '.csv', sep='')
                                              },
                                              content = function(con) {
                                                
                                                #telemetry tracking
                                                log_to_database("download", list(
                                                  input_id = "downloadData",
                                                  download_type = "data",
                                                  timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                                ))
                                                
                                                write.csv(ci.sachet.all_CLEANED2, con)
                                              }
                                            )
                                          
                                        }
                                        
                                        if (input$mamorsam %in% "Both SAM and MAM") {
                                          
                                          makePlot_NumberReached_All_SAM_MAM <- function() {
                                            theme_set(theme_bw(13))
                                            a1 <- ggplot(ci.pop.sachet[ci.pop.sachet$cov %in% input$relapse_treat_prop & 
                                                                         ci.pop.sachet$variable6 != "SAM" & 
                                                                    ci.pop.sachet$variable22!="MANGO" 
                                                                    ,] ,aes(x=variable22,y=mean.trt ,fill=variable22)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  facet_wrap(~variable33)+ 
                                              geom_errorbar(aes(ymin=mean.trt -stdv.trt , ymax=mean.trt +stdv.trt ), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab("MAM children reached")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + guides(fill="none") #+
                                              #ggtitle("Number of MAM Children Reached, by Budget and per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) 
                                            a2 <-ggplot(ci.pop.sachet[ci.pop.sachet$cov %in% input$relapse_treat_prop & 
                                                                        ci.pop.sachet$variable6 == "SAM" & 
                                                                        ci.pop.sachet$variable22!="MANGO",] ,aes(x=variable22,y=mean.trt ,fill=variable22)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +   facet_wrap(~variable33)+ 
                                              geom_errorbar(aes(ymin=mean.trt -stdv.trt , ymax=mean.trt +stdv.trt), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab("SAM children reached")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + guides(fill="none") #+
                                              #ggtitle("Number of SAM Children Reached, by Budget and per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) 
                                            grid.arrange(a1,a2,ncol=1)
                                            }
                                          
                                          output$Primary_Graph <- renderPlot({
                                            makePlot_NumberReached_All_SAM_MAM()
                                          },height = 450, units="px")

                                          output$downloadPlot_Primary_Graph <- downloadHandler(
                                            filename = function() {
                                              paste('plot-ProductNeeded-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_NumberReached_All_SAM_MAM(), width = 9, height = 12, dpi = 300, units = "in")
                                            }
                                          )
                                          
                                          output$Primary_Table_Title <- renderText('Children Treated, by Malnutrition Status and Budget, per Protocol')
                                          output$Note_uncertainty <- renderText(
                                            "Our model is built to randomly add some uncertainty to the cost inputs. We do this for 25 iterations. The Number of Children Treated
              in the table reflect the average results across the 25 iterations, and the standard deviation is also presented to reflect that
              the results should be interpreted as estimates within a range rather than exact values with certainty."
                                          )
                                          
                                          output$Primary_tbl = renderDT(
                                            ci.pop.sachet_CLEANED[which(ci.pop.sachet_CLEANED[,1] != "MANGO"),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Primary <- downloadHandler(
                                            filename = function() {
                                              paste('data-ChildrenTreated', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(ci.pop.sachet_CLEANED, con)
                                            }
                                          )
                                          output$Secondary_Results_Title_Children_Treated <- renderText("Number of Sachets per Child Treated, by Budget and per Protocol")
                                          makePlot_ProgramCosts_All_SAM_MAM_Children_Treated <- function() {
                                            theme_set(theme_bw(13))
                                            a1 <- ggplot(ci.sachet[ci.sachet$cov %in% input$relapse_treat_prop &
                                                                     ci.sachet$variable6 != "SAM" &
                                                                   ci.pop.sachet$variable22!="MANGO"
                                                                   ,] ,aes(x=variable22,y=mean.sch ,fill=variable22)) +
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  facet_wrap(~variable33)+
                                              geom_errorbar(aes(ymin=mean.sch -stdv.sch , ymax=mean.sch +stdv.sch), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab("Sachets per MAM child")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + guides(fill="none") #+
                                            #ggtitle("Number of MAM Children Reached, by Budget and per Protocol")+ theme(plot.title = element_text(hjust = 0.5))
                                            a2 <-ggplot(ci.sachet[ci.sachet$cov %in% input$relapse_treat_prop &
                                                                        ci.pop.sachet$variable6 == "SAM" &
                                                                    ci.pop.sachet$variable22!="MANGO",] ,aes(x=variable22,y=mean.sch ,fill=variable22)) +
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +   facet_wrap(~variable33)+
                                              geom_errorbar(aes(ymin=mean.sch -stdv.sch , ymax=mean.sch +stdv.sch), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab("Sachets per SAM child")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + guides(fill="none") #+
                                            #ggtitle("Number of SAM Children Reached, by Budget and per Protocol")+ theme(plot.title = element_text(hjust = 0.5))
                                            grid.arrange(a1,a2,ncol=1)
                                          }
                                          
                                          output$Secondary_Graph_Children_Treated <- renderPlot({
                                            makePlot_ProgramCosts_All_SAM_MAM_Children_Treated()
                                          },height = 450, units="px")
                                          
                                          output$downloadPlot_Secondary_Graph_Children_Treated <- downloadHandler(
                                            filename = function() {
                                              paste('plot-SachetsPerChildFixedBudget-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_ProgramCosts_All_SAM_MAM_Children_Treated(), width = 6, height = 12, dpi = 300, units = "in")
                                            }
                                          )
                                          output$Secondary_Table_Title_Children_Treated <- renderText('Sachets per Child, by Budget and per Protocol')
                                          #ci.sachet.all_CLEANED2 <- ci.sachet_CLEANED
                                          output$Secondary_tbl_Children_Treated = renderDT(
                                            ci.sachet_CLEANED[which(ci.sachet_CLEANED[,1] !="MANGO"),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Secondary_Children_Treated <- downloadHandler(
                                            filename = function() {
                                              paste('data-SachetsPerChildFixedBudget', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(ci.sachet_CLEANED[which(ci.sachet_CLEANED[,1] !="MANGO"),], con)
                                            }
                                          )
                                        }
                                      })
                         
                       }
                       else if (input$ph_objective %in% "Understand amount of RUTF needed for programming across protocols") {
                         
                         cov.mSTANDARD = pop.mSTANDARD = pop.mMANGO = cov.mMANGO = 0
                         cov.m = input$mam_cov_obj3[1]
                         pop.m = input$n_pop_m*cov.m
                         costm$number.treated_sc1  <- ifelse(costm$variable2 %in% c("Standard") &
                                                               costm$variable6 == "SAM" , (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m + costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) +
                                                                                                        costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                                        costm$cov* pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.sam  + costm$cov * costm$tau.m * pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m),
                                                             
                                                             ifelse(costm$variable2 %in% c("Standard") &
                                                                      costm$variable6 %in% c("MAM")  ,(pop.mSTANDARD + costm$cov *  (0 + pop.mSTANDARD*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                                                                                           costm$cov *  pop.mSTANDARD * costm$alpha.m3 * costm$sigma.m3.to.mam) ,
                                                                    
                                                                    ifelse(costm$variable2=="OptiMA"&
                                                                             costm$variable6 == "SAM" ,(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                     costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                                     costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                                     costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m),
                                                                           ifelse(costm$variable2=="OptiMA" &
                                                                                    costm$variable6 == "MAM" ,(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                                                          costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ,
                                                                                  
                                                                                  ifelse(costm$variable2=="ComPAS"&
                                                                                           costm$variable6 == "SAM",(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                  costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                                                  costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                                                  costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m),
                                                                                         
                                                                                         ifelse(costm$variable2=="ComPAS" &
                                                                                                  costm$variable6 == "MAM" ,(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                                                                        costm$cov *  pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam  ) ,
                                                                                                
                                                                                                ifelse(costm$variable2=="MANGO"&
                                                                                                         costm$variable6 == "SAM",
                                                                                                       (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m + costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m) +
                                                                                                          costm$cov * costm$tau.m * (pop + pop.mMANGO*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam +
                                                                                                          costm$cov* pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                                          costm$cov * pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m),
                                                                                                       
                                                                                                       0 )))))))
                         
                         costm$number.sach_sc1  <- ifelse(costm$variable22=="Standard - RUTF/RUSF" & costm$variable6 == "SAM" ,(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m)  +
                                                                                                                                 costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) +
                                                                                                                                                               costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                                                 costm$number.sachet.prms * (costm$cov* pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                                               costm$cov * pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,
                                                          ifelse(costm$variable22=="Standard - RUTF/CSB" & costm$variable6 == "SAM" ,(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m)  +
                                                                                                                                              costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) +
                                                                                                                                                                            costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                                                              costm$number.sachet.prms * (costm$cov* pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                                                            costm$cov * pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,
                                                                 
                                                                 ifelse(costm$variable22=="Standard - RUTF/RUSF" & costm$variable6 %in% c("MAM") ,(costm$number.sachet.m * pop.mSTANDARD +
                                                                                                                                                                                           costm$number.sachet.prsm * (costm$cov *  (0 + pop.mSTANDARD*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                                           costm$number.sachet.prmm * (costm$cov *  pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam )) ,
                                                                        
                                                                        ifelse(costm$variable22=="Standard - RUTF/CSB" & costm$variable6 %in% c("MAM") ,(costm$number.csb.m * pop.mSTANDARD +
                                                                                                                                                                                                        costm$number.csb.prsm * (costm$cov *  (0 + pop.mSTANDARD*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                                                        costm$number.csb.prmm * (costm$cov *  pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam )) ,
                                                                               
                                                                               ifelse(costm$variable22=="OptiMA"& costm$variable6 == "SAM" ,(costm$number.sachet.s1 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                          costm$number.sachet.prss1 * (costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                                         costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                                          costm$number.sachet.prms1 * (costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                                                                                                         costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m)),
                                                                                      
                                                                                      ifelse(costm$variable22=="OptiMA" & costm$variable6 == "MAM" ,(costm$number.sachet.m1 * pop.m +
                                                                                                                                                                                costm$number.sachet.prsm1 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                                                                costm$number.sachet.prmm1 * (costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ) ,
                                                                                             
                                                                                             
                                                                                             ifelse(costm$variable22=="ComPAS"& costm$variable6 == "SAM",(costm$number.sachet.s2 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                                                                       costm$number.sachet.prss2 * (costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                                                      costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam) +
                                                                                                                                                                       costm$number.sachet.prms2 * (costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                                                                                                                      costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m)),
                                                                                                    
                                                                                                    ifelse(costm$variable22=="ComPAS" & costm$variable6 == "MAM" ,(costm$number.sachet.m2 * pop.m +
                                                                                                                                                                                              costm$number.sachet.prsm2 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam)   +
                                                                                                                                                                                              costm$number.sachet.prmm2 * (costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam)) ,
                                                                                                           
                                                                                                           ifelse(costm$variable22=="MANGO"& costm$variable6 == "SAM", (costm$number.sachet.s3 * (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m)  +
                                                                                                                                                                                     costm$number.sachet.prss3 * (costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m) +
                                                                                                                                                                                                                    costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam) +
                                                                                                                                                                                     costm$number.sachet.prms3 * (costm$cov* pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                                                                                                                                                    costm$cov * pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m) ),
                                                                                                                  
                                                                                                                  0 )))))))))
                         
                         costm$number.recovered_sc1  <- ifelse(costm$variable2 %in% c("Standard") &
                                                                 costm$variable6 == "SAM" ,(pop*costm$alpha.s + pop*costm$alpha2.s*costm$phi.s +
                                                                                                         costm$cov*costm$alpha.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam +
                                                                                                         costm$cov*costm$phi.s*costm$alpha2.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam#+
                                                                                                       #costm$cov*costm$alpha.s.prms*pop.m*costm$alpha.m*costm$sigma.m.to.sam +
                                                                                                       #costm$cov*costm$phi.s*costm$alpha2.s.prms*pop.m*costm$alpha.m*costm$sigma.m.to.sam
                                                                 ) ,
                                                               
                                                               ifelse(costm$variable2 %in% c("Standard") &
                                                                        costm$variable6 %in% c("MAM")  ,(pop.mSTANDARD*costm$alpha.m + pop.mSTANDARD*costm$alpha2.m*costm$phi.m +
                                                                                                                                                             costm$cov*costm$alpha.m.prmm*pop.mSTANDARD*costm$alpha.m*costm$sigma.m.to.mam +
                                                                                                                                                             costm$cov*costm$phi.m*costm$alpha2.m.prmm*pop.mSTANDARD*costm$alpha.m*costm$sigma.m.to.mam+
                                                                                                                                                             costm$cov*costm$alpha.m.prsm*pop.mSTANDARD*costm$alpha.s*costm$sigma.s.to.mam +
                                                                                                                                                             costm$cov*costm$phi.m*costm$alpha2.m.prsm*cov.mSTANDARD*pop*costm$alpha.s*costm$sigma.s.to.mam
                                                                        ) ,
                                                                      
                                                                      ifelse(costm$variable2=="OptiMA"&
                                                                               costm$variable6 == "SAM" ,(pop*costm$alpha.s1 + pop*costm$alpha2.s1*costm$phi.s1  +
                                                                                                                       costm$cov*costm$alpha.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                                       costm$cov*costm$phi.s1*costm$alpha2.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                                       costm$cov*costm$alpha.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam +
                                                                                                                       costm$cov*costm$phi.s1*costm$alpha2.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam),
                                                                             ifelse(costm$variable2=="OptiMA" &
                                                                                      costm$variable6 == "MAM" ,(pop.m*costm$alpha.m1 + pop.m*costm$alpha2.m1*costm$phi.m1 +
                                                                                                                                            costm$cov*costm$alpha.m1.prmm*pop.m*costm$alpha.m1*costm$sigma.m1.to.mam +
                                                                                                                                            costm$cov*costm$phi.m1*costm$alpha2.m1.prmm*pop.m*costm$alpha.m1*costm$sigma.m1.to.mam+
                                                                                                                                            costm$cov*costm$alpha.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam +
                                                                                                                                            costm$cov*costm$phi.m1*costm$alpha2.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam) ,
                                                                                    
                                                                                    ifelse(costm$variable2=="ComPAS"&
                                                                                             costm$variable6 == "SAM",(pop*costm$alpha.s2 + pop*costm$alpha2.s2*costm$phi.s2  +
                                                                                                                                    costm$cov*costm$alpha.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                                    costm$cov*costm$phi.s2*costm$alpha2.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                                    costm$cov*costm$alpha.s2.prms*pop.m*costm$alpha.m2*costm$sigma.m2.to.sam +
                                                                                                                                    costm$cov*costm$phi.s2*costm$alpha2.s2.prms*pop.m*costm$alpha.m2*costm$sigma.m2.to.sam),
                                                                                           
                                                                                           ifelse(costm$variable2=="ComPAS" &
                                                                                                    costm$variable6 == "MAM" ,(pop.m*costm$alpha.m2 + pop.m*costm$alpha2.m2*costm$phi.m2 +
                                                                                                                                                          costm$cov*costm$alpha.m2.prmm*pop.m*costm$alpha.m2*costm$sigma.m2.to.mam +
                                                                                                                                                          costm$cov*costm$phi.m2*costm$alpha2.m2.prmm*pop.m*costm$alpha.m2*costm$sigma.m2.to.mam+
                                                                                                                                                          costm$cov*costm$alpha.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam +
                                                                                                                                                          costm$cov*costm$phi.m2*costm$alpha2.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam) ,
                                                                                                  
                                                                                                  ifelse(costm$variable2=="MANGO"&
                                                                                                           costm$variable6 == "SAM",
                                                                                                         (pop*costm$alpha.s3 + pop*costm$alpha2.s3*costm$phi.s3  +
                                                                                                            costm$cov*costm$alpha.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                                            costm$cov*costm$phi.s3*costm$alpha2.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam# +
                                                                                                            #costm$cov*costm$alpha.s3.prms*pop.mMANGO*costm$alpha.m3*costm$sigma.m3.to.sam +
                                                                                                            #costm$cov*costm$phi.s3*costm$alpha2.s3.prms*pop.mMANGO*costm$alpha.m3*costm$sigma.m3.to.sam
                                                                                                          ),
                                                                                                         
                                                                                                         0 )))))))
                         
                         cov.mSTANDARD = pop.mSTANDARD = 0
                         cov.m = input$mam_cov_obj3[2]
                         pop.m = input$n_pop_m*cov.m
                         costm$number.treated_sc2  <- ifelse(costm$variable2 %in% c("Standard") &
                                                               costm$variable6 == "SAM" , (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m + costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) +
                                                                                                        costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                                        costm$cov* pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.sam  + costm$cov * costm$tau.m * pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m),
                                                             
                                                             ifelse(costm$variable2 %in% c("Standard")  &
                                                                      costm$variable6 %in% c("MAM")  ,(pop.mSTANDARD + costm$cov *  (0 + pop.mSTANDARD*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam +
                                                                                                                                                           costm$cov *  pop.mSTANDARD * costm$alpha.m3 * costm$sigma.m3.to.mam) ,
                                                                    
                                                                    ifelse(costm$variable2=="OptiMA"&
                                                                             costm$variable6 == "SAM" ,(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                     costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                                     costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                                     costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m),
                                                                           ifelse(costm$variable2=="OptiMA" &
                                                                                    costm$variable6 == "MAM" ,(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam +
                                                                                                                                          costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ,
                                                                                  
                                                                                  ifelse(costm$variable2=="ComPAS"&
                                                                                           costm$variable6 == "SAM",(pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m + costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                  costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                                                  costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                                                  costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m),
                                                                                         
                                                                                         ifelse(costm$variable2=="ComPAS" &
                                                                                                  costm$variable6 == "MAM" ,(pop.m + costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam +
                                                                                                                                                        costm$cov *  pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam  ) ,
                                                                                                
                                                                                                ifelse(costm$variable2=="MANGO"&
                                                                                                         costm$variable6 == "SAM",
                                                                                                       (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m + costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m) +
                                                                                                          costm$cov * costm$tau.m * (pop + pop.mMANGO*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam +
                                                                                                          costm$cov* pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                                          costm$cov * pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m),
                                                                                                       
                                                                                                       0 )))))))
                         costm$number.sach_sc2  <- ifelse(costm$variable22 %in% c("Standard - RUTF/RUSF")  & costm$variable6 == "SAM" ,(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m)  +
                                                                                                                                 costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) +
                                                                                                                                                               costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                                                 costm$number.sachet.prms * (costm$cov* pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                                               costm$cov * pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,
                                                          ifelse(costm$variable22=="Standard - RUTF/CSB" & costm$variable6 == "SAM" ,(costm$number.sachet.s * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m)  +
                                                                                                                                              costm$number.sachet.prss * (costm$cov*costm$alpha.s * costm$sigma.s.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) +
                                                                                                                                                                            costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mSTANDARD))*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam) +
                                                                                                                                              costm$number.sachet.prms * (costm$cov* pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.sam  +
                                                                                                                                                                            costm$cov * pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam * costm$tau.m) ) ,
                                                                 
                                                                 ifelse(costm$variable22 %in% c("Standard - RUTF/RUSF") & costm$variable6 %in% c("MAM") ,(costm$number.sachet.m * pop.mSTANDARD +
                                                                                                                                                                                           costm$number.sachet.prsm * (costm$cov *  (0 + pop.mSTANDARD*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                                           costm$number.sachet.prmm * (costm$cov *  pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam )) ,
                                                                        
                                                                        ifelse(costm$variable22=="Standard - RUTF/CSB" & costm$variable6 %in% c("MAM") ,(costm$number.csb.m * pop.mSTANDARD +
                                                                                                                                                                                                        costm$number.csb.prsm * (costm$cov *  (0 + pop.mSTANDARD*costm$tau.m) * costm$alpha.s * costm$sigma.s.to.mam ) +
                                                                                                                                                                                                        costm$number.csb.prmm * (costm$cov *  pop.mSTANDARD * costm$alpha.m * costm$sigma.m.to.mam )) ,
                                                                               
                                                                               ifelse(costm$variable22=="OptiMA"& costm$variable6 == "SAM" ,(costm$number.sachet.s1 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                          costm$number.sachet.prss1 * (costm$cov*costm$alpha.s1 * costm$sigma.s1.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                                         costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                                          costm$number.sachet.prms1 * (costm$cov* pop.m * costm$alpha.m1 * costm$sigma.m1.to.sam  +
                                                                                                                                                                                         costm$cov * pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam * costm$tau.m)),
                                                                                      
                                                                                      ifelse(costm$variable22=="OptiMA" & costm$variable6 == "MAM" ,(costm$number.sachet.m1 * pop.m +
                                                                                                                                                                                costm$number.sachet.prsm1 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s1 * costm$sigma.s1.to.mam) +
                                                                                                                                                                                costm$number.sachet.prmm1 * (costm$cov *  pop.m * costm$alpha.m1 * costm$sigma.m1.to.mam) ) ,
                                                                                             
                                                                                             
                                                                                             ifelse(costm$variable22=="ComPAS"& costm$variable6 == "SAM",(costm$number.sachet.s2 * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m)  +
                                                                                                                                                                       costm$number.sachet.prss2 * (costm$cov*costm$alpha.s2 * costm$sigma.s2.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) +
                                                                                                                                                                                                      costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.m))*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam) +
                                                                                                                                                                       costm$number.sachet.prms2 * (costm$cov* pop.m * costm$alpha.m2 * costm$sigma.m2.to.sam  +
                                                                                                                                                                                                      costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam * costm$tau.m)),
                                                                                                    
                                                                                                    ifelse(costm$variable22=="ComPAS" & costm$variable6 == "MAM" ,(costm$number.sachet.m2 * pop.m +
                                                                                                                                                                                              costm$number.sachet.prsm2 * (costm$cov *  (pop + pop.m*costm$tau.m) * costm$alpha.s2 * costm$sigma.s2.to.mam)   +
                                                                                                                                                                                              costm$number.sachet.prmm2 * (costm$cov * pop.m * costm$alpha.m2 * costm$sigma.m2.to.mam)) ,
                                                                                                           
                                                                                                           ifelse(costm$variable22=="MANGO"& costm$variable6 == "SAM", (costm$number.sachet.s3 * (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m)  +
                                                                                                                                                                                     costm$number.sachet.prss3 * (costm$cov*costm$alpha.s3 * costm$sigma.s3.to.sam  *  (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m) +
                                                                                                                                                                                                                    costm$cov * costm$tau.m * (pop + (cov.s*mod.wasted * (1-cov.mMANGO))*costm$tau.m) * costm$alpha.s3 * costm$sigma.s3.to.mam) +
                                                                                                                                                                                     costm$number.sachet.prms3 * (costm$cov* pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.sam  +
                                                                                                                                                                                                                    costm$cov * pop.mMANGO * costm$alpha.m3 * costm$sigma.m3.to.mam * costm$tau.m) ),
                                                                                                                  
                                                                                                                  0 )))))))))
                         
                         costm$number.recovered_sc2  <- ifelse(costm$variable2 %in% c("Standard") &
                                                                 costm$variable6 == "SAM" ,(pop*costm$alpha.s + pop*costm$alpha2.s*costm$phi.s +
                                                                                                         costm$cov*costm$alpha.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam +
                                                                                                         costm$cov*costm$phi.s*costm$alpha2.s.prss*pop*costm$alpha.s*costm$sigma.s.to.sam#+
                                                                                                       #costm$cov*costm$alpha.s.prms*pop.m*costm$alpha.m*costm$sigma.m.to.sam +
                                                                                                       #costm$cov*costm$phi.s*costm$alpha2.s.prms*pop.m*costm$alpha.m*costm$sigma.m.to.sam
                                                                 ) ,
                                                               
                                                               ifelse(costm$variable2=="Standard" &
                                                                        costm$variable6 %in% c("MAM")  ,(pop.mSTANDARD*costm$alpha.m + pop.mSTANDARD*costm$alpha2.m*costm$phi.m +
                                                                                                                                                             costm$cov*costm$alpha.m.prmm*pop.mSTANDARD*costm$alpha.m*costm$sigma.m.to.mam +
                                                                                                                                                             costm$cov*costm$phi.m*costm$alpha2.m.prmm*pop.mSTANDARD*costm$alpha.m*costm$sigma.m.to.mam+
                                                                                                                                                             costm$cov*costm$alpha.m.prsm*pop.mSTANDARD*costm$alpha.s*costm$sigma.s.to.mam +
                                                                                                                                                             costm$cov*costm$phi.m*costm$alpha2.m.prsm*cov.mSTANDARD*pop*costm$alpha.s*costm$sigma.s.to.mam
                                                                        ) ,
                                                                      
                                                                      ifelse(costm$variable2=="OptiMA"&
                                                                               costm$variable6 == "SAM" ,(pop*costm$alpha.s1 + pop*costm$alpha2.s1*costm$phi.s1  +
                                                                                                                       costm$cov*costm$alpha.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                                       costm$cov*costm$phi.s1*costm$alpha2.s1.prss*pop*costm$alpha.s1*costm$sigma.s1.to.sam +
                                                                                                                       costm$cov*costm$alpha.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam +
                                                                                                                       costm$cov*costm$phi.s1*costm$alpha2.s1.prms*pop*costm$alpha.m1*costm$sigma.m1.to.sam),
                                                                             ifelse(costm$variable2=="OptiMA" &
                                                                                      costm$variable6 == "MAM" ,(pop.m*costm$alpha.m1 + pop.m*costm$alpha2.m1*costm$phi.m1 +
                                                                                                                                            costm$cov*costm$alpha.m1.prmm*pop.m*costm$alpha.m1*costm$sigma.m1.to.mam +
                                                                                                                                            costm$cov*costm$phi.m1*costm$alpha2.m1.prmm*pop.m*costm$alpha.m1*costm$sigma.m1.to.mam+
                                                                                                                                            costm$cov*costm$alpha.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam +
                                                                                                                                            costm$cov*costm$phi.m1*costm$alpha2.m1.prsm*pop*costm$alpha.s1*costm$sigma.s1.to.mam) ,
                                                                                    
                                                                                    ifelse(costm$variable2=="ComPAS"&
                                                                                             costm$variable6 == "SAM",(pop*costm$alpha.s2 + pop*costm$alpha2.s2*costm$phi.s2  +
                                                                                                                                    costm$cov*costm$alpha.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                                    costm$cov*costm$phi.s2*costm$alpha2.s2.prss*pop*costm$alpha.s2*costm$sigma.s2.to.sam +
                                                                                                                                    costm$cov*costm$alpha.s2.prms*pop.m*costm$alpha.m2*costm$sigma.m2.to.sam +
                                                                                                                                    costm$cov*costm$phi.s2*costm$alpha2.s2.prms*pop.m*costm$alpha.m2*costm$sigma.m2.to.sam),
                                                                                           
                                                                                           ifelse(costm$variable2=="ComPAS" &
                                                                                                    costm$variable6 == "MAM" ,(pop.m*costm$alpha.m2 + pop.m*costm$alpha2.m2*costm$phi.m2 +
                                                                                                                                                          costm$cov*costm$alpha.m2.prmm*pop.m*costm$alpha.m2*costm$sigma.m2.to.mam +
                                                                                                                                                          costm$cov*costm$phi.m2*costm$alpha2.m2.prmm*pop.m*costm$alpha.m2*costm$sigma.m2.to.mam+
                                                                                                                                                          costm$cov*costm$alpha.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam +
                                                                                                                                                          costm$cov*costm$phi.m2*costm$alpha2.m2.prsm*pop*costm$alpha.s2*costm$sigma.s2.to.mam) ,
                                                                                                  
                                                                                                  ifelse(costm$variable2=="MANGO"&
                                                                                                           costm$variable6 == "SAM",
                                                                                                         (pop*costm$alpha.s3 + pop*costm$alpha2.s3*costm$phi.s3  +
                                                                                                            costm$cov*costm$alpha.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam +
                                                                                                            costm$cov*costm$phi.s3*costm$alpha2.s3.prss*pop*costm$alpha.s3*costm$sigma.s3.to.sam #+
                                                                                                            #costm$cov*costm$alpha.s3.prms*pop.mMANGO*costm$alpha.m3*costm$sigma.m3.to.sam +
                                                                                                            #costm$cov*costm$phi.s3*costm$alpha2.s3.prms*pop.mMANGO*costm$alpha.m3*costm$sigma.m3.to.sam
                                                                                                          ),
                                                                                                         
                                                                                                         0 )))))))
                         
                         ###   
                         ### total amount of product needed across scenarios
                         ###
                         costm_prod_REV$number.sach_sc1 = rep(NA,dim(costm)[1])
                         costm_prod_REV$number.sach_sc2 = rep(NA,dim(costm)[1])
                         costm_prod_REV$number.recovered_sc1 = rep(NA,dim(costm)[1])
                         costm_prod_REV$number.recovered_sc2 = rep(NA,dim(costm)[1])
                         costm_prod_REV$number.treated_sc1 = rep(NA,dim(costm)[1])
                         costm_prod_REV$number.treated_sc2 = rep(NA,dim(costm)[1])
                         prot = names(table(costm$variable22))
                         hh = length(as.numeric(names(table(costm$alpha.s))))
                         for (i in 1:dim(costm_prod_REV)[1]){
                           for (ii in 1:length(prot)){
                             for (iii in 1:hh){
                               costm_prod_REV$number.sach_sc1[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$number.sach_sc1[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
                               costm_prod_REV$number.sach_sc2[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$number.sach_sc2[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
                               costm_prod_REV$number.recovered_sc1[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$number.recovered_sc1[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
                               costm_prod_REV$number.recovered_sc2[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$number.recovered_sc2[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
                               costm_prod_REV$number.treated_sc1[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$number.treated_sc1[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
                               costm_prod_REV$number.treated_sc2[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])] = sum(costm$number.treated_sc2[which(costm$alpha.s == costm$alpha.s[iii] & costm$variable22 %in% prot[ii])])
                             }
                           }
                         }
                         
                         ci.total.product.all_sc1 <- data.frame(costm_prod_REV %>% group_by(variable22,cov) %>% 
                                                                  dplyr::summarise(mean.prod= mean(number.sach_sc1),stdv.prod= sd(number.sach_sc1),
                                                                                   mean.recovered = mean(number.recovered_sc1),
                                                                                   mean.treated = mean(number.treated_sc1)))
                         
                         ci.total.product_sc1 <- data.frame(costm %>% group_by(variable22,variable4,variable5,variable6,cov) %>% 
                                                              dplyr::summarise(mean.prod= mean(number.sach_sc1),stdv.prod= sd(number.sach_sc1),
                                                                               mean.recovered = mean(number.recovered_sc1),
                                                                               mean.treated = mean(number.treated_sc1)))
                         
                         ci.total.product.all_sc2 <- data.frame(costm_prod_REV %>% group_by(variable22,cov) %>% 
                                                                  dplyr::summarise(mean.prod= mean(number.sach_sc2),stdv.prod= sd(number.sach_sc2),
                                                                                   mean.recovered = mean(number.recovered_sc2),
                                                                                   mean.treated = mean(number.treated_sc2)))
                         
                         ci.total.product_sc2 <- data.frame(costm %>% group_by(variable22,variable6,cov) %>% 
                                                              dplyr::summarise(mean.prod= mean(number.sach_sc2),stdv.prod= sd(number.sach_sc2),
                                                                               mean.recovered = mean(number.recovered_sc2),
                                                                               mean.treated = mean(number.treated_sc2)))
                         
                         ci.total.product.all_sc1_CLEANED2 = ci.total.product.all_sc1[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop),]
                         ci.total.product.all_sc1_CLEANED <- data.frame(Protocol = ci.total.product.all_sc1_CLEANED2$variable22,
                                                                        `Units of RUTF` = (ci.total.product.all_sc1_CLEANED2$mean.prod),
                                                                        `SD for Units of RUTF` = (ci.total.product.all_sc1_CLEANED2$stdv.prod),
                                                                        `Total Treated` = (ci.total.product.all_sc1_CLEANED2$mean.treated),
                                                                        `Total Recoveries` = (ci.total.product.all_sc1_CLEANED2$mean.recovered),
                                                                        Scenario = ifelse(ci.total.product.all_sc1_CLEANED2$variable22 %in% c("MANGO","Standard - RUTF/RUSF"),
                                                                                          paste(input$cov_s,"for SAM, 0 for MAM"), 
                                                                                          paste(input$cov_s,"for SAM,",input$mam_cov_obj3[1],"for MAM")),
                                                                        Scenario2 = rep(input$mam_cov_obj3[1], length(ci.total.product.all_sc1_CLEANED2$variable22)))
                         
                         ci.total.product.all_sc2_CLEANED2 = ci.total.product.all_sc2[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop),]
                         ci.total.product.all_sc2_CLEANED <- data.frame(Protocol = ci.total.product.all_sc2_CLEANED2$variable22,
                                                                        `Units of RUTF` = (ci.total.product.all_sc2_CLEANED2$mean.prod),
                                                                        `SD for Units of RUTF` = (ci.total.product.all_sc2_CLEANED2$stdv.prod),
                                                                        `Total Treated` = (ci.total.product.all_sc2_CLEANED2$mean.treated),
                                                                        `Total Recoveries` = (ci.total.product.all_sc2_CLEANED2$mean.recovered),
                                                                        Scenario = ifelse(ci.total.product.all_sc2_CLEANED2$variable22 %in% c("MANGO","Standard - RUTF/RUSF"),
                                                                                          paste(input$cov_s,"for SAM, 0 for MAM"), 
                                                                                          paste(input$cov_s,"for SAM,",input$mam_cov_obj3[2],"for MAM")),
                                                                        Scenario2 = rep(input$mam_cov_obj3[2], length(ci.total.product.all_sc1_CLEANED2$variable22)))
                         
                         ci.total.product_sc1_CLEANED2 = ci.total.product_sc1[which(ci.total.product_sc1$cov %in% input$relapse_treat_prop),]
                         ci.total.product_sc1_CLEANED3 <- data.frame(Protocol = ci.total.product_sc1_CLEANED2$variable22,
                                                                     `Units of RUTF` = (ci.total.product_sc1_CLEANED2$mean.prod),
                                                                     `SD for Units of RUTF` = (ci.total.product_sc1_CLEANED2$stdv.prod),
                                                                     `Total Treated` = (ci.total.product_sc1_CLEANED2$mean.treated),
                                                                     `Total Recoveries` = (ci.total.product_sc1_CLEANED2$mean.recovered),
                                                                     Scenario = ifelse(ci.total.product_sc1_CLEANED2$variable22 %in% c("MANGO","Standard - RUTF/RUSF"),
                                                                                       paste(input$cov_s,"for SAM, 0 for MAM"), 
                                                                                       paste(input$cov_s,"for SAM,",input$mam_cov_obj3[1],"for MAM")),
                                                                     Scenario2 = rep(input$mam_cov_obj3[1], length(ci.total.product.all_sc1_CLEANED2$variable22)),
                                                                     `Malnutrition Status` = ci.total.product_sc1_CLEANED2$variable6)
                         
                         ci.total.product_sc2_CLEANED2 = ci.total.product_sc2[which(ci.total.product_sc2$cov %in% input$relapse_treat_prop),]
                         ci.total.product_sc2_CLEANED3 <- data.frame(Protocol = ci.total.product_sc2_CLEANED2$variable22,
                                                                     `Units of RUTF` = (ci.total.product_sc2_CLEANED2$mean.prod),
                                                                     `SD for Units of RUTF` = (ci.total.product_sc2_CLEANED2$stdv.prod),
                                                                     `Total Treated` = (ci.total.product_sc2_CLEANED2$mean.treated),
                                                                     `Total Recoveries` = (ci.total.product_sc2_CLEANED2$mean.recovered),
                                                                     Scenario = ifelse(ci.total.product_sc2_CLEANED2$variable22 %in% c("MANGO","Standard - RUTF/RUSF"),
                                                                                       paste(input$cov_s,"for SAM, 0 for MAM"), 
                                                                                       paste(input$cov_s,"for SAM,",input$mam_cov_obj3[2],"for MAM")),
                                                                     Scenario2 = rep(input$mam_cov_obj3[2], length(ci.total.product.all_sc1_CLEANED2$variable22)),
                                                                     `Malnutrition Status` = ci.total.product_sc2_CLEANED2$variable6)
                         
                         ci.total.product.all_sc1_CLEANED3 <- ci.total.product.all_sc1_CLEANED
                         ci.total.product.all_sc2_CLEANED3 <- ci.total.product.all_sc2_CLEANED
                         total.all_sc1_sc2 <- rbind(ci.total.product.all_sc1_CLEANED,ci.total.product.all_sc2_CLEANED)
                         total_sc1_sc2 <- rbind(ci.total.product_sc1_CLEANED3,ci.total.product_sc2_CLEANED3)
                         total.all_sc1_sc2$Malnutrition.Status <- rep("Overall",dim(total.all_sc1_sc2)[1])
                         #total.all_sc1_sc2$Malnutrition.Status <- ifelse(total.all_sc1_sc2$Protocol %in% c("OptiMA","ComPAS"),"SAM and MAM","SAM only")
                         total.sc1_sc2_CLEANED <- rbind(total.all_sc1_sc2,total_sc1_sc2)
                         names(total.sc1_sc2_CLEANED) <- c("Protocol","Units of RUTF","SD for Units of RUTF","Total Treated","Total Recovered","Coverage Scenario","Scenario2","Malnutrition Status")
                         

                         output$Interpretation_Primary <- renderText({
                           paste("Per the standard protocol and SAM-only treatment approach,", format(round(ci.total.product.all_sc1_CLEANED2$mean.prod[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")],0), big.mark=","),"sachets of RUTF would be needed for a program with coverage of", scales::percent(input$cov_s), "and in a context with an estimated", format(input$n_pop, big.mark=","), "SAM children. 
                                 This equates to", round(ci.total.product.all_sc1_CLEANED2$mean.prod[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")]/
                                                           ci.total.product.all_sc1_CLEANED2$mean.treated[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")],0), "sachets per treated child and", round(ci.total.product.all_sc1_CLEANED2$mean.prod[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")]/
                                                           ci.total.product.all_sc1_CLEANED2$mean.recovered[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")],0), "sachets per recovery.")
                         })
                         
                         output$Relative_to_Current <-  renderText({
                           paste("Scenario 1: Compared to RUTF needs for the Standard protocol with SAM treatment only, the RUTF needs for simplified protocols treating MAM and SAM are provided for treatment coverage of", scales::percent(input$cov_s), 
                           "and a MAM treatment coverage of", scales::percent(input$mam_cov_obj3[1]), "." )
                         })
                         if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "ComPAS")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforCompas = "green"
                           Compasicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "ComPAS")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforCompas = "red"
                           Compasicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "ComPAS")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforCompas = "blue"
                           Compasicon = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "MANGO")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforMango = "green"
                           Mangoicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "MANGO")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforMango = "red"
                           Mangoicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "MANGO")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforMango = "blue"
                           Mangoicon = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "OptiMA")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforOptima = "green"
                           Optimaicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "OptiMA")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforOptima = "red"
                           Optimaicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "OptiMA")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforOptima = "blue"
                           Optimaicon = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforStandard = "green"
                           Standardicon = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforStandard = "red"
                           Standardicon = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforStandard = "blue"
                           Standardicon = icon("equals", lib = "font-awesome")}
                         
                         output$ComPASBox <- renderInfoBox({
                           infoBox(
                             "ComPAS", paste0(percent((ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "ComPAS")]/ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Compasicon,
                             color = colorforCompas, fill =TRUE#, width = 3
                           )
                         })
                           output$MANGOBox　<- renderInfoBox({
                             infoBox(
                               "MANGO", paste0(percent((ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "MANGO")]/ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Mangoicon,
                               color = colorforMango, fill =TRUE#, width = 3
                             )
                           })
                         
                         output$OptiMABox <-　renderInfoBox({
                           infoBox(
                             "OptiMA", paste0(percent((ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "OptiMA")]/ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Optimaicon,
                             color = colorforOptima, fill =TRUE#, width = 3
                           )
                         })
                         output$StandardBox <-　renderInfoBox({
                           infoBox(
                             "Standard", paste0(percent((ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")]/ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Standardicon,
                             color = colorforStandard, fill =TRUE#, width = 3
                           )
                         })
                         output$Interpretation_Primary <- renderText({
                           paste("Per the standard protocol and SAM-only treatment approach,", format(round(ci.total.product.all_sc1_CLEANED2$mean.prod[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")],0), big.mark=","),"sachets of RUTF would be needed for a program with coverage of", scales::percent(input$cov_s), "and in a context with an estimated", format(input$n_pop, big.mark=","), "SAM children. 
                                 This equates to", round(ci.total.product.all_sc1_CLEANED2$mean.prod[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")]/
                                                           ci.total.product.all_sc1_CLEANED2$mean.treated[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")],0), "sachets per treated child and", round(ci.total.product.all_sc1_CLEANED2$mean.prod[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")]/
                                                                                                                                                                                                                                        ci.total.product.all_sc1_CLEANED2$mean.recovered[which(ci.total.product.all_sc1_CLEANED2$variable22 %in% "Standard - RUTF/RUSF")],0), "sachets per recovery.")
                         })
                         output$Relative_to_Current2 <-  renderText({
                           paste("Scenario 2: Compared to RUTF needs for the Standard protocol with SAM treatment only, the RUTF needs for simplified protocols treating MAM and SAM are provided for treatment coverage of", scales::percent(input$cov_s), 
                                 "and a MAM treatment coverage of", scales::percent(input$mam_cov_obj3[2]), ".")
                         })
                         if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "ComPAS")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforCompas2 = "green"
                           Compasicon2 = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "ComPAS")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforCompas2 = "red"
                           Compasicon2 = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "ComPAS")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforCompas2 = "blue"
                           Compasicon2 = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "MANGO")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforMango2 = "green"
                           Mangoicon2 = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "MANGO")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforMango2 = "red"
                           Mangoicon2 = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "MANGO")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforMango2 = "blue"
                           Mangoicon2 = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "OptiMA")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforOptima2 = "green"
                           Optimaicon2 = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "OptiMA")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforOptima2 = "red"
                           Optimaicon2 = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "OptiMA")],0)/round(ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforOptima2 = "blue"
                           Optimaicon2 = icon("equals", lib = "font-awesome")}
                         
                         if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) <1){
                           colorforStandard2 = "green"
                           Standardicon2 = icon("angle-double-down", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) >1){
                           colorforStandard2 = "red"
                           Standardicon2 = icon("angle-double-up", lib = "font-awesome")}
                         else if (round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0)/round(ci.total.product.all_sc1$mean.prod[which(ci.total.product.all_sc1$cov %in% input$relapse_treat_prop & ci.total.product.all_sc1$variable22 %in% "Standard - RUTF/RUSF")],0) == 1){
                           colorforStandard2 = "blue"
                           Standardicon2 = icon("equals", lib = "font-awesome")}
                         
                         output$ComPASBox2 <- renderInfoBox({
                           infoBox(
                             "ComPAS", paste0(percent((ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "ComPAS")]/ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Compasicon2,
                             color = colorforCompas2, fill =TRUE#, width = 3
                           )
                         })
                           output$MANGOBox2　<- renderInfoBox({
                             infoBox(
                               "MANGO", paste0(percent((ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "MANGO")]/ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Mangoicon2,
                               color = colorforMango2, fill =TRUE#, width = 3
                             )
                           })
                         output$OptiMABox2 <-　renderInfoBox({
                           infoBox(
                             "OptiMA", paste0(percent((ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "OptiMA")]/ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Optimaicon2,
                             color = colorforOptima2, fill =TRUE#, width = 3
                           )
                         })
                         output$StandardBox2 <-　renderInfoBox({
                           infoBox(
                             "Standard", paste0(percent((ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")]/ci.total.product.all_sc2$mean.prod[which(ci.total.product.all_sc2$cov %in% input$relapse_treat_prop & ci.total.product.all_sc2$variable22 %in% "Standard - RUTF/RUSF")]))), icon = Standardicon2,
                             color = colorforStandard2, fill =TRUE#, width = 3
                           )
                         })
                         total.sc1_sc2_CLEANED$Protocol[which(total.sc1_sc2_CLEANED$Protocol %in% "Standard - RUTF/RUSF")] <- "Standard"
                            makePlot_RUTF_SAM_MAM <- function() {
                                            theme_set(theme_bw(15))
                                            #grid.arrange(a00,a11,a22,ncol=3)
                                            ggplot(total.sc1_sc2_CLEANED[which(total.sc1_sc2_CLEANED$`Malnutrition Status` %in% "Overall"),],aes(x=Protocol,y=`Units of RUTF`,fill=Protocol)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  
                                              geom_errorbar(aes(ymin=`Units of RUTF` - `SD for Units of RUTF`, ymax=`Units of RUTF` + `SD for Units of RUTF`), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab("Programmatic RUTF Needs")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~Scenario2,nrow=1) +
                                              ggtitle("RUTF Required for Program")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")+
                                              scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
                                          }
                                          
                                          output$Primary_Graph <- renderPlot({
                                            makePlot_RUTF_SAM_MAM()
                                          })
                                          
                                          output$downloadPlot_Primary_Graph <- downloadHandler(
                                            filename = function() {
                                              paste('plot-CostEfficiency-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_RUTF_SAM_MAM(), width = 9, height = 5, dpi = 300, units = "in")
                                            }
                                          )
                                          
                                          output$Primary_Table_Title <- renderText('RUTF Needs, by Protocol, Scenario, and Malnutrition Status')
                                          output$Note_uncertainty <- renderText(
                                            "Our model is built to randomly add some uncertainty to the cost inputs. We do this for 25 iterations. The RUTF needs
              in the table reflect the average results across the 25 iterations, and the standard deviation is also presented to reflect that
              the results should be interpreted as estimates within a range rather than exact values with certainty."
                                          )
                                          
                                          total.sc1_sc2_CLEANED_rev <- total.sc1_sc2_CLEANED[which(total.sc1_sc2_CLEANED$`Malnutrition Status` %in% "Overall"),c(1:6,8)]
                                          total.sc1_sc2_CLEANED_rev[,2:5] <- format(round(total.sc1_sc2_CLEANED_rev[,2:5],0), big.mark=",")
                                          total.sc1_sc2_CLEANED_rev <- total.sc1_sc2_CLEANED_rev[!duplicated(total.sc1_sc2_CLEANED_rev), ]
                                          
                                          output$Primary_tbl = renderDT(
                                            total.sc1_sc2_CLEANED_rev[order(total.sc1_sc2_CLEANED_rev$Protocol,total.sc1_sc2_CLEANED_rev$`Coverage Scenario`),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Primary <- downloadHandler(
                                            filename = function() {
                                              paste('data-CostEfficiency', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(total.sc1_sc2_CLEANED_rev[order(total.sc1_sc2_CLEANED_rev$Protocol,total.sc1_sc2_CLEANED_rev$`Coverage Scenario`),], con)
                                            }
                                          )
                                          
                                          ### Secondary
                                          output$Secondary_Results_Title_Children_Treated_RUTF <- renderText("Amount of RUTF for MAM versus SAM Treatment, per Protocol and MAM Coverage Scenario")
                                          makePlot_ProgramCosts_All_SAM_MAM_Children_Treated_RUTF <- function() {
                                            theme_set(theme_bw(13))
                                            
                                            a1 <- ggplot(total.sc1_sc2_CLEANED[which(total.sc1_sc2_CLEANED$`Malnutrition Status` %in% "SAM"),],aes(x=Protocol,y=`Units of RUTF`,fill=Protocol)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  
                                              geom_errorbar(aes(ymin=`Units of RUTF` - `SD for Units of RUTF`, ymax=`Units of RUTF` + `SD for Units of RUTF`), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab("RUTF Needs")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~Scenario2,nrow=1) +
                                              ggtitle("RUTF Required or SAM Treatment")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")+
                                              scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
                                            
                                            a2 <- ggplot(total.sc1_sc2_CLEANED[which(total.sc1_sc2_CLEANED$`Malnutrition Status` %in% "MAM"),],aes(x=Protocol,y=`Units of RUTF`,fill=Protocol)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  
                                              geom_errorbar(aes(ymin=`Units of RUTF` - `SD for Units of RUTF`, ymax=`Units of RUTF` + `SD for Units of RUTF`), width=.2, position=position_dodge(.9))+
                                              xlab("")+ ylab("RUTF Needs")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~Scenario2,nrow=1) +
                                              ggtitle("RUTF Required or MAM Treatment")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")+
                                              scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
                                            
                                            grid.arrange(a1,a2,ncol=1)
                                          }
                                          
                                          output$Secondary_Graph_Children_Treated_RUTF <- renderPlot({
                                            makePlot_ProgramCosts_All_SAM_MAM_Children_Treated_RUTF()
                                          },height = 450, units="px")
                                          
                                          output$downloadPlot_Secondary_Graph_Children_Treated_RUTF <- downloadHandler(
                                            filename = function() {
                                              paste('plot-RUTFbySAMvsMAM-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_ProgramCosts_All_SAM_MAM_Children_Treated_RUTF(), width = 6, height = 12, dpi = 300, units = "in")
                                            }
                                          )
                                          output$Secondary_Table_Title_Children_Treated_RUTF <- renderText('RUTF Needs per MAM Treatment Coverage Scenario,\nby Malnutrition Status and per Protocol')
                                          
                                          total.sc1_sc2_CLEANED_rev2 <- total.sc1_sc2_CLEANED[which(!(total.sc1_sc2_CLEANED$`Malnutrition Status` %in% "Overall")),c(1:6,8)]
                                          total.sc1_sc2_CLEANED_rev2[,2:5] <- format(round(total.sc1_sc2_CLEANED_rev2[,2:5],0), big.mark=",")
                                          total.sc1_sc2_CLEANED_rev2 <- total.sc1_sc2_CLEANED_rev2[!duplicated(total.sc1_sc2_CLEANED_rev2), ]
                                          
                                          output$Secondary_tbl_Children_Treated_RUTF = renderDT(
                                            total.sc1_sc2_CLEANED_rev2[order(total.sc1_sc2_CLEANED_rev2$Protocol,total.sc1_sc2_CLEANED_rev2$`Coverage Scenario`),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Secondary_Children_Treated_RUTF <- downloadHandler(
                                            filename = function() {
                                              paste('data-RUTFbySAMvsMAM', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(total.sc1_sc2_CLEANED_rev2[order(total.sc1_sc2_CLEANED_rev2$Protocol,total.sc1_sc2_CLEANED_rev2$`Coverage Scenario`),], con)
                                            }
                                          )
                                          
                                          output$Secondary_Results_Title_Children_Recovered_RUTF <- renderText("RUTF per Recovery (accounting for overall program outcomes),\nper Protocol and MAM Coverage Scenario")
                                          total.sc1_sc2_CLEANED3 <- total.sc1_sc2_CLEANED
                                          total.sc1_sc2_CLEANED3$`Units of RUTF per Recovery` <- total.sc1_sc2_CLEANED3$`Units of RUTF`/total.sc1_sc2_CLEANED3$`Total Recovered`
                                          makePlot_ProgramCosts_All_SAM_MAM_Children_Recovered_RUTF <- function() {
                                            theme_set(theme_bw(13))
                                            a1 <- ggplot(total.sc1_sc2_CLEANED3[which(total.sc1_sc2_CLEANED3$`Malnutrition Status` %in% "Overall"),],aes(x=Protocol,y=`Units of RUTF per Recovery`,fill=Protocol)) +  
                                              geom_bar(stat="identity", color="black", position=position_dodge()) +  
                                              xlab("")+ ylab("RUTF Needs")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
                                              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~Scenario2,nrow=2) +
                                              ggtitle("RUTF Required for each Recovery")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")+
                                              scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
                                            
                                            a2 <- ggplot(total.sc1_sc2_CLEANED3[which(total.sc1_sc2_CLEANED3$`Malnutrition Status` %in% c("MAM","SAM")),],aes(x=Protocol,y=`Total Recovered`,fill=`Malnutrition Status`)) +  
                                              geom_bar(stat="identity", color="black", position=position_stack(), alpha=0.5) +  
                                              xlab("")+ ylab("Number Recovered")  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1),legend.position = "top") + 
                                              scale_fill_manual(values=c("yellow","red")) + facet_wrap(~Scenario2,nrow=2) +
                                              ggtitle("Recoveries by Malnutrition Status")+ theme(plot.title = element_text(hjust = 0.5)) + #guides(position="bottom")+
                                              scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
                                            
                                            grid.arrange(a1,a2,ncol=2)
                                          }
                                          
                                          output$Secondary_Graph_Children_Recovered_RUTF <- renderPlot({
                                            makePlot_ProgramCosts_All_SAM_MAM_Children_Recovered_RUTF()
                                          },height = 450, units="px")
                                          
                                          output$downloadPlot_Secondary_Graph_Children_Recovered_RUTF <- downloadHandler(
                                            filename = function() {
                                              paste('plot-RUTFbySAMvsMAM-', Sys.Date(), '.pdf', sep='')
                                            },
                                            content = function(file) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadPlot",
                                                download_type = "plot",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              ggsave(file, makePlot_ProgramCosts_All_SAM_MAM_Children_Recovered_RUTF(), width = 6, height = 12, dpi = 300, units = "in")
                                            }
                                          )
                                          output$Secondary_Table_Title_Children_Recovered_RUTF <- renderText('Number of Recoveries and RUTF Needs per Recovered Child,\nby MAM Treatment Coverage Scenario and per Protocol')
                                          
                                          total.sc1_sc2_CLEANED_rev3 <- total.sc1_sc2_CLEANED3[which((total.sc1_sc2_CLEANED3$`Malnutrition Status` %in% "Overall")),c(1,4:6,9)]
                                          total.sc1_sc2_CLEANED_rev3[,c(2:3,5)] <- format(round(total.sc1_sc2_CLEANED_rev3[,c(2:3,5)],0), big.mark=",")
                                          total.sc1_sc2_CLEANED_rev3 <- total.sc1_sc2_CLEANED_rev3[,c(1,5,2,3,4)]
                                          total.sc1_sc2_CLEANED_rev3 <- total.sc1_sc2_CLEANED_rev3[!duplicated(total.sc1_sc2_CLEANED_rev3), ]
                                          
                                          output$Secondary_tbl_Children_Recovered_RUTF = renderDT(
                                            total.sc1_sc2_CLEANED_rev3[order(total.sc1_sc2_CLEANED_rev3$Protocol,total.sc1_sc2_CLEANED_rev3$`Coverage Scenario`),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
                                          )
                                          output$downloadDataTable_Secondary_Children_Recovered_RUTF <- downloadHandler(
                                            filename = function() {
                                              paste('data-RUTFbySAMvsMAM', Sys.Date(), '.csv', sep='')
                                            },
                                            content = function(con) {
                                              
                                              #telemetry tracking
                                              log_to_database("download", list(
                                                input_id = "downloadData",
                                                download_type = "data",
                                                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
                                              ))
                                              
                                              write.csv(total.sc1_sc2_CLEANED_rev3[order(total.sc1_sc2_CLEANED_rev3$Protocol,total.sc1_sc2_CLEANED_rev3$`Coverage Scenario`),], con)
                                            }
                                          )
                                        
                         
                    }
        })
        
        ### For secondary plots
        observeEvent(input$mamorsam,       
                     ignoreNULL = F, { 
        if (input$mamorsam %in% "Just SAM" & !(input$ph_objective %in% "Reach more children with a fixed budget")) {
          output$Secondary_Results_Title_Total_Program_Costs <- renderText('Total Costs of Program')
          #### Secondary ####
          makePlot_ProgramCosts_All_Total_Program_Costs <- function() {
            theme_set(theme_bw(15))
            ggplot(ci.total.cost.all[ci.total.cost.all$cov %in% input$relapse_treat_prop,] ,aes(x=variable22,y=10^3*mean.tc ,fill=variable22)) +  
              geom_bar(stat="identity", color="black", position=position_dodge()) +  
              geom_errorbar(aes(ymin=10^3*mean.tc -10^3*stdv.tc , ymax=10^3*mean.tc +10^3*stdv.tc ), width=.2, position=position_dodge(.9))+
              xlab("")+ ylab(paste("Total Cost (",input$Currency,")"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733","#88CCEE")) + guides(fill="none")+
              scale_y_continuous(labels = dollar)
              #ggtitle("Total Program Costs")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")
          }
          output$Secondary_Graph_Total_Program_Costs <- renderPlot({
            makePlot_ProgramCosts_All_Total_Program_Costs()
          })

          output$downloadPlot_Secondary_Graph_Total_Program_Costs <- downloadHandler(
            filename = function() {
              paste('plot-ProgramCosts-', Sys.Date(), '.pdf', sep='')
            },
            content = function(file) {
              
              #telemetry tracking
              log_to_database("download", list(
                input_id = "downloadPlot",
                download_type = "plot",
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              
              ggsave(file, makePlot_ProgramCosts_All_Total_Program_Costs(), width = 6, height = 5, dpi = 300, units = "in")
            }
          )
          output$Secondary_Table_Title_Total_Program_Costs <- renderText('Total Program Costs, by Protocol')
          ci.total.cost.all_CLEANED2 <- ci.total.cost.all_CLEANED#[which(!(ci.total.cost.rec.all_CLEANED$Protocol %in% "Standard - CSB") ),]
          output$Secondary_tbl = renderDT(
            ci.total.cost.all_CLEANED2, options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
          )
          output$downloadDataTable_Secondary_Total_Program_Costs <- downloadHandler(
            filename = function() {
              paste('data-ProgramCosts', Sys.Date(), '.csv', sep='')
            },
            content = function(con) {
              
              #telemetry tracking
              log_to_database("download", list(
                input_id = "downloadData",
                download_type = "data",
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              
              write.csv(ci.total.cost.all_CLEANED2, con)
            }
          )
          output$Secondary_Results_Title_Total_Product_Needs <- renderText('Total Cartons of Product Needed')          
          makePlot_Secondary_Product_All <- function() {
            theme_set(theme_bw(15))
            ggplot(ci.total.product.all[ci.total.product.all$cov %in% input$relapse_treat_prop,] ,aes(x=variable22,y=mean.prod/150,fill=variable22)) +  
              geom_bar(stat="identity", color="black", position=position_dodge()) +  
              geom_errorbar(aes(ymin=(mean.prod -stdv.prod)/150 , ymax=(mean.prod +stdv.prod)/150 ), width=.2, position=position_dodge(.9))+
              xlab("")+ ylab(paste("Total number of cartons"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) +
              scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733","#88CCEE")) +
              ggtitle("Cartons of Product to Treat SAM Children Per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")
          }
          
          output$Secondary_Graph_Product_All <- renderPlot({
            makePlot_Secondary_Product_All()
          })# renderplot end
          output$downloadPlot_Secondary_Product_All <- downloadHandler(
            filename = function() {
              paste('plot-ProductNeeded-', Sys.Date(), '.pdf', sep='')
            },
            content = function(file) {
              
              #telemetry tracking
              log_to_database("download", list(
                input_id = "downloadPlot",
                download_type = "plot",
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              
              ggsave(file, makePlot_Secondary_Product_All(), width = 6, height = 5, dpi = 300, units = "in")
            }
          )
          
          output$Secondary_Table_Title_Total_Product <- renderText('Cartons of Product Needed, by Protocol')
          
          ci.total.product.all_CLEANED22 <- ci.total.product.all_CLEANED#[which(!(ci.total.product.all_CLEANED$Protocol %in% "Standard - CSB") ),]
          ci.total.product.all_CLEANED22[,2] <- format(round(ci.total.product.all_CLEANED22[,2]/150,0), big.mark=",")
          ci.total.product.all_CLEANED22[,3] <- format(round(ci.total.product.all_CLEANED22[,3]/150,0), big.mark=",")
          output$Secondary_tbl_Total_Product = renderDT(
            ci.total.product.all_CLEANED22, options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
          )
          output$downloadDataTable_Secondary_Total_Product <- downloadHandler(
            filename = function() {
              paste('data-ProductNeeded', Sys.Date(), '.csv', sep='')
            },
            content = function(con) {
              
              #telemetry tracking
              log_to_database("download", list(
                input_id = "downloadData",
                download_type = "data",
                timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
              ))
              
              write.csv(ci.total.product.all_CLEANED22, con)
            }
          )
        }
        
        if (input$mamorsam %in% "Both SAM and MAM" & !(input$ph_objective %in% "Reach more children with a fixed budget")) {
          output$Secondary_Results_Title_Total_Program_Costs <- renderText('Total Costs of Program')
          makePlot_ProgramCosts_All_SAM_MAM_Total_Program_Costs <- function() {
          theme_set(theme_bw(15))
          ggplot(tc.df[which(tc.df$variable22 != "MANGO"),],aes(x=variable22,y=10^3*mean.tc,fill=variable22)) +  
            geom_bar(stat="identity", color="black", position=position_dodge()) +  
            geom_errorbar(aes(ymin=10^3*mean.tc - 10^3*stdv.tc, ymax=10^3*mean.tc + 10^3*stdv.tc), width=.2, position=position_dodge(.9))+
            xlab("")+ ylab(paste("Total Cost (",Currencyforoutputs,")"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
            scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~variable6,nrow=1) + guides(fill="none") +
            scale_y_continuous(labels = dollar)
            #ggtitle("Cost per Recovered Child")+ theme(plot.title = element_text(hjust = 0.5)) 
        }

        output$Secondary_Graph_Total_Program_Costs <- renderPlot({
            makePlot_ProgramCosts_All_SAM_MAM_Total_Program_Costs()
        })
        
        output$downloadPlot_Secondary_Graph_Total_Program_Costs <- downloadHandler(
          filename = function() {
            paste('plot-ProgramCosts-', Sys.Date(), '.pdf', sep='')
          },
          content = function(file) {
            
            #telemetry tracking
            log_to_database("download", list(
              input_id = "downloadPlot",
              download_type = "plot",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            ))
            
            ggsave(file, makePlot_ProgramCosts_All_SAM_MAM_Total_Program_Costs(), width = 9, height = 5, dpi = 300, units = "in")
          }
        )
        
        output$Secondary_Table_Title_Total_Program_Costs <- renderText('Total Program Costs, by Protocol')
        #ci.total.cost_CLEANED2 <- ci.total.cost_CLEANED
        output$Secondary_tbl_Total_Program_Costs = renderDT(
          ci.total.cost_CLEANED[which(ci.total.cost_CLEANED[,1] != "MANGO"),][order(ci.total.cost_CLEANED$Protocol[which(ci.total.cost_CLEANED[,1] != "MANGO")]),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
        )
        output$downloadDataTable_Secondary_Total_Program_Costs <- downloadHandler(
          filename = function() {
            paste('data-ProgramCosts', Sys.Date(), '.csv', sep='')
          },
          content = function(con) {
            
            #telemetry tracking
            log_to_database("download", list(
              input_id = "downloadData",
              download_type = "data",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            ))
            
            write.csv(ci.total.cost_CLEANED[which(ci.total.cost_CLEANED[,1] != "MANGO"),][order(ci.total.cost_CLEANED$Protocol[which(ci.total.cost_CLEANED[,1] != "MANGO")]),], con)
          }
        )
        
        output$Secondary_Results_Title_Total_Product_Needs <- renderText('Total Cartons of Product Needed')          
        makePlot_Secondary_Product_All_SAM_MAM <- function() {
          theme_set(theme_bw(15))
          ggplot(prod.df[which(!(prod.df$variable22 %in% "MANGO")),],aes(x=variable22,y=mean.prod/150,fill=variable22)) +  
            geom_bar(stat="identity", color="black", position=position_dodge()) +  
            geom_errorbar(aes(ymin=(mean.prod - stdv.prod)/150, ymax=(mean.prod + stdv.prod)/150), width=.2, position=position_dodge(.9))+
            xlab("")+ ylab(paste("Total number of cartons"))  + theme(axis.text.x=element_text(angle = 45, hjust = 1, vjust = 1)) + 
            scale_fill_manual(values=c("#4477AA","#DDCC77","#CC6677","#117733")) + facet_wrap(~variable6,nrow=1) +
            ggtitle("Cartons of Product to Treat Children, by Malnutrition Status and per Protocol")+ theme(plot.title = element_text(hjust = 0.5)) + guides(fill="none")
        }
        
        output$Secondary_Graph_Product_All <- renderPlot({
          makePlot_Secondary_Product_All_SAM_MAM()
        })
        
        output$downloadPlot_Secondary_Product_All <- downloadHandler(
          filename = function() {
            paste('plot-ProductNeeded-', Sys.Date(), '.pdf', sep='')
          },
          content = function(file) {
            
            #telemetry tracking
            log_to_database("download", list(
              input_id = "downloadPlot",
              download_type = "plot",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            ))
            
            ggsave(file, makePlot_Secondary_Product_All_SAM_MAM(), width = 9, height = 5, dpi = 300, units = "in")
          }
        )
        
        output$Secondary_Table_Title_Total_Product <- renderText('Cartons of Product Needed, by Malnutrition Status and Protocol')
        
        ci.total.product_CLEANED[,2] <- format(round(ci.total.product_CLEANED[,2]/150,0), big.mark=",")
        ci.total.product_CLEANED[,3] <- format(round(ci.total.product_CLEANED[,3]/150,0), big.mark=",")
        output$Secondary_tbl_Total_Product = renderDT(
          ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][order(ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][["Protocol"]]),], options = list(lengthChange = FALSE,scrollX = TRUE,dom = 'tp')
        )
        output$downloadDataTable_Secondary_Total_Product <- downloadHandler(
          filename = function() {
            paste('data-ProductNeeded', Sys.Date(), '.csv', sep='')
          },
          content = function(con) {
            
            #telemetry tracking
            log_to_database("download", list(
              input_id = "downloadData",
              download_type = "data",
              timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
            ))
            
            write.csv(ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][order(ci.total.product_CLEANED[which(!(ci.total.product_CLEANED[,1] %in% "MANGO")),][["Protocol"]]),], con)
          }
        )
        
        }
                       removeModal()      
                       })
        
        }) # Observe event end
              

#reactive cost values - inpatient
observe({
  # Get the selected country
  country <- input$countryChoice
  
  # Define different cost ranges for each country
  cost_ranges <- list(
    "Bangladesh" = c(round(5.43*1.47*input$conv_rate,2), round(30.42*1.47*input$conv_rate,2)),
    "Burkina Faso" = c(round(4.08*1.47*input$conv_rate, 2), round(23.54*1.47*input$conv_rate, 2)),
    "DRC" = c(round(0.9*1.47*input$conv_rate, 2), round(5.73*1.47*input$conv_rate, 2)),
    "Ethiopia" = c(round(2.91*1.47*input$conv_rate, 2), round(17.06*1.47*input$conv_rate, 2)),
    "Ghana" = c(round(5.94*1.47*input$conv_rate,2), round(32.5*1.47*input$conv_rate,2)),
    "Malawi" = c(round(2.58*1.47*input$conv_rate, 2), round(14.13*1.47*input$conv_rate, 2)),
    "Mali" = c(round(4.05*1.47*input$conv_rate, 2), round(21.49*1.47*input$conv_rate, 2)),
    "Mozambique" = c(round(2.74*1.47*input$conv_rate,2), round(15.29*1.47*input$conv_rate,2)),
    "Nepal" = c(round(4.64*1.47*input$conv_rate,2), round(25.13*1.47*input$conv_rate,2)),
    "Niger" = c(round(1.73*1.47*input$conv_rate,2), round(10.04*1.47*input$conv_rate,2)),
    "Pakistan" = c(round(10.26*1.47*input$conv_rate,2), round(56.7*1.47*input$conv_rate,2)),
    "Sierra Leone" = c(round(3.53*1.47*input$conv_rate,2), round(19.47*1.47*input$conv_rate,2)),
    "Somalia" = c(round(2.91*1.47*input$conv_rate, 2), round(17.06*1.47*input$conv_rate, 2)),
    "South Sudan" = c(round(2.91*1.47*input$conv_rate, 2), round(17.06*1.47*input$conv_rate, 2)),
    "Uganda" = c(round(4.08*1.47*input$conv_rate,2), round(23.83*1.47*input$conv_rate,2))
  )
  
  # Default value if no country is selected or "Select from list"
  default_cost <- c(round(2.91*1.47*input$conv_rate, 2), round(17.06*1.47*input$conv_rate, 2)) #used Ethiopia
  
  # Update the slider based on selection
  if(country %in% names(cost_ranges)) {
    updateNumericRangeInput(
      session,
      "inp.hosp.cost",
      value = cost_ranges[[country]]
    )
  } else {
    # Use default if no country is selected
    updateNumericRangeInput(
      session,
      "inp.hosp.cost",
      value = default_cost
    )
  }
})

#reactive cost values - outpatient
observe({
  # Get the selected country
  country <- input$countryChoice
  
  # Define different cost ranges for each country
  cost_ranges_op <- list(
    "Bangladesh" = c(round(0.80*1.47*input$conv_rate,2), round(13.13*1.47*input$conv_rate,2)),
    "Burkina Faso" = c(round(0.69*1.47*input$conv_rate, 2), round(10.72*1.47*input$conv_rate, 2)),
    "DRC" = c(round(0.22*1.47*input$conv_rate, 2), round(3.78*1.47*input$conv_rate, 2)),
    "Ethiopia" = c(round(0.53*1.47*input$conv_rate, 2), round(8.54*1.47*input$conv_rate, 2)),
    "Ghana" = c(round(0.83*1.47*input$conv_rate,2), round(11.63*1.47*input$conv_rate,2)),
    "Malawi" = c(round(0.5*1.47*input$conv_rate, 2), round(6.98*1.47*input$conv_rate, 2)),
    "Mali" = c(round(0.67*1.47*input$conv_rate, 2), round(9.32*1.47*input$conv_rate, 2)),
    "Mozambique" = c(round(0.5*1.47*input$conv_rate,2), round(6.53*1.47*input$conv_rate,2)),
    "Nepal" = c(round(0.7*1.47*input$conv_rate,2), round(9.81*1.47*input$conv_rate,2)),
    "Niger" = c(round(0.36*1.47*input$conv_rate,2), round(5.84*1.47*input$conv_rate,2)),
    "Pakistan" = c(round(1.36*1.47*input$conv_rate,2), round(18.56*1.47*input$conv_rate,2)),
    "Sierra Leone" = c(round(0.53*1.47*input$conv_rate,2), round(9.02*1.47*input$conv_rate,2)),
    "Somalia" = c(round(0.53*1.47*input$conv_rate, 2), round(8.54*1.47*input$conv_rate, 2)),
    "South Sudan" = c(round(0.53*1.47*input$conv_rate, 2), round(8.54*1.47*input$conv_rate, 2)),
    "Uganda" = c(round(0.64*1.47*input$conv_rate,2), round(10.16*1.47*input$conv_rate,2))
  )
  
  # Default value if no country is selected or "Select from list"
  default_cost <- c(round(0.53*1.47*input$conv_rate, 2), round(8.54*1.47*input$conv_rate, 2)) # used Ethiopia
  
  # Update the slider based on selection
  if(country %in% names(cost_ranges_op)) {
    updateNumericRangeInput(
      session,
      "labor.cost",
      value = cost_ranges_op[[country]]
    )
  } else {
    # Use default if no country is selected
    updateNumericRangeInput(
      session,
      "labor.cost",
      value = default_cost
    )
  }
})

#reactive cost values - outpatient
observe({
  # Update MAM based on selection
  if(input$mamorsam %in% "Just SAM") {
    updateSliderInput(
      session,
      "cov_m",
      value = 0
    )
  } 
})

# Generate code on feedback survey page
output$code_gen <- renderPrint({ 
  if (input$generateBt > 0 ) 
    isolate(
      floor(runif(1,min=1000,max=9999))
    )
})

observe ({

  updateNumericRangeInput(session, "rutf.cost",
                    value = c(round(0.31*0.9*input$conv_rate,2), round(0.369*1.15*input$conv_rate,2)))

  updateNumericRangeInput(session, "rusf.cost",
                    value = c(round(min(c(0.31,0.32,0.39,0.34,0.354,0.369)*1.15)*0.9*input$conv_rate,2), round(max(c(0.31,0.32,0.39,0.34,0.354,0.369)*1.15)*0.9*input$conv_rate,2)))

  updateNumericRangeInput(session, "csb.cost",
                          value = c(round(min(c(0.31,0.32,0.39,0.34,0.354,0.369)*1.15)*0.9*input$conv_rate,2), round(max(c(0.31,0.32,0.39,0.34,0.354,0.369)*1.15)*0.9*input$conv_rate,2)))
  
  updateNumericRangeInput(session, "drug.cost",
                    value = c(round(0.14*input$conv_rate,2), round(0.24*input$conv_rate,2)))

  updateNumericRangeInput(session, "labor.cost",
                    value = c(round(0.3*input$conv_rate,2), round(3.5*input$conv_rate,2)))

  updateNumericRangeInput(session, "nonlabor.cost",
                    value = c(round(0.9*(1.2)*input$conv_rate,2), round(1.1*(1.2)*input$conv_rate,2)))

  updateNumericRangeInput(session, "transport.cost",
                    value = c(round(0.67*input$conv_rate,2), round(1.41*input$conv_rate,2)))

  updateNumericRangeInput(session, "opportunity.cost",
                    value = c(round(0.06*input$conv_rate,2), round(2.01*input$conv_rate,2)))

  updateNumericRangeInput(session, "inp.hosp.cost",
                    value = c(round(0.9*34.72*input$conv_rate,2), round(1.1*34.72*input$conv_rate,2)))
  
  updateNumericRangeInput(session, "inp.meds.supplies",
                          value = c(round(0.9*(9.51+1.53)*input$conv_rate,2), round(1.1*(9.51+1.53)*input$conv_rate,2)))
  
  updateNumericRangeInput(session, "inp.csb.cost",
                          value = c(round(min(c(2.17,4.23,1.36)*1.15)*input$conv_rate,2), round(max(c(2.17,4.23,1.36)*1.15)*input$conv_rate,2)))
})


observeEvent (input$approach_based_on,{
  if (input$approach_based_on %in% "Standard"){
    updateSliderInput(session,"Recov_prop",value=0.783)
    updateSliderInput(session,"Def_prop",value=0.029)
    updateSliderInput(session,"Dea_prop",value=0.031)
    updateSliderInput(session,"Hosp_prop",value=0)
    updateSliderInput(session,"NevRecov_prop",value=0.118)
    updateSliderInput(session,"relapse_prop",value=c(0.015,0.136))
    updateSliderInput(session,"relapse_prop_sam_to_mam",value=c(0.0725,0.226))
    
    updateSliderInput(session,"Recov_prop_m",value=0.529)
    updateSliderInput(session,"Def_prop_m",value=0.153)
    updateSliderInput(session,"Dea_prop_m",value=0.056)
    updateSliderInput(session,"Hosp_prop_m",value=0)
    updateSliderInput(session,"NevRecov_prop_m",value=0.067)
    updateSliderInput(session,"relapse_prop_mam_to_mam",value=c(0.1,0.3))
    updateSliderInput(session,"relapse_prop_mam_to_sam",value=c(0,0.1))
   }
  else if (input$approach_based_on %in% "OptiMA"){
    updateSliderInput(session,"Recov_prop",value=0.681)
    updateSliderInput(session,"Def_prop",value=0.058)
    updateSliderInput(session,"Dea_prop",value=0.021)
    updateSliderInput(session,"Hosp_prop",value=0)
    updateSliderInput(session,"NevRecov_prop",value=0.132)
    updateSliderInput(session,"relapse_prop",value=c(0.111,0.111))
    updateSliderInput(session,"relapse_prop_sam_to_mam",value=c(0.01,0.01))
    
    updateSliderInput(session,"Recov_prop_m",value=0.497)
    updateSliderInput(session,"Def_prop_m",value=0.113)
    updateSliderInput(session,"Dea_prop_m",value=0.056)
    updateSliderInput(session,"Hosp_prop_m",value=0)
    updateSliderInput(session,"NevRecov_prop_m",value=0.080)
    updateSliderInput(session,"relapse_prop_mam_to_mam",value=c(0.24,0.24))
    updateSliderInput(session,"relapse_prop_mam_to_sam",value=c(0,0.1))
   }
  else if (input$approach_based_on %in% "MANGO"){
    updateSliderInput(session,"Recov_prop",value=0.500)
    updateSliderInput(session,"Def_prop",value=0.135)
    updateSliderInput(session,"Dea_prop",value=0)
    updateSliderInput(session,"Hosp_prop",value=0.201)
    updateSliderInput(session,"NevRecov_prop",value=0.120)
    updateSliderInput(session,"relapse_prop",value=c(0.024,0.024))
    updateSliderInput(session,"relapse_prop_sam_to_mam",value=c(0.0725,0.226))
    
    updateSliderInput(session,"Recov_prop_m",value=0.586)
    updateSliderInput(session,"Def_prop_m",value=0.094)
    updateSliderInput(session,"Dea_prop_m",value=0.008)
    updateSliderInput(session,"Hosp_prop_m",value=0.172)
    updateSliderInput(session,"NevRecov_prop_m",value=0.141)
    updateSliderInput(session,"relapse_prop_mam_to_mam",value=c(0.1,0.3))
    updateSliderInput(session,"relapse_prop_mam_to_sam",value=c(0,0.1))
   }
  else if (input$approach_based_on %in% "ComPAS"){
    updateSliderInput(session,"Recov_prop",value=0.694)
    updateSliderInput(session,"Def_prop",value=0.061)
    updateSliderInput(session,"Dea_prop",value=0.013)
    updateSliderInput(session,"Hosp_prop",value=0)
    updateSliderInput(session,"NevRecov_prop",value=0.121)
    updateSliderInput(session,"relapse_prop",value=c(0.03,0.124))
    updateSliderInput(session,"relapse_prop_sam_to_mam",value=c(0.174,0.351))
    
    updateSliderInput(session,"Recov_prop_m",value=0.519)
    updateSliderInput(session,"Def_prop_m",value=0.095)
    updateSliderInput(session,"Dea_prop_m",value=0.030)
    updateSliderInput(session,"Hosp_prop_m",value=0)
    updateSliderInput(session,"NevRecov_prop_m",value=0.074)
    updateSliderInput(session,"relapse_prop_mam_to_mam",value=c(0.212,0.318))
    updateSliderInput(session,"relapse_prop_mam_to_sam",value=c(0.001,0.027))
   }
})



# #telemetry----
# tryCatch({
#   # Log initialization
#   log_to_database("session_start", list(
#     app_version = "1.0",
#     timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#   ))
# 
#   # Track submitInfo2 button (Run Model)
#   observeEvent(input$submitInfo2, {
#     print("submitInfo2 button clicked")
#     log_to_database("button_click", list(
#       button_id = "runModel",
#       timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#     ))
#   }, ignoreInit = TRUE)
# 
#   observeEvent(input$toggle_results1, {
#     log_to_database("button_click", list(
#       button_id = "additionalInfo",
#       timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#     ))
#   }, ignoreInit = TRUE)
# 
#   # Track tabs changes
#   observeEvent(input$inTabset, {
#     print(paste("Tab changed to:", input$inTabset))
#     log_to_database("navigation", list(
#       tab = input$inTabset,
#       timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#     ))
#   }, ignoreInit = TRUE)
# 
# 
#   # Vector of inputs to track
#   inputs_to_track <- c("ph_objective", "approach_based_on", "standard_mam_product", "countryChoice", "mamorsam",
#                        "n_pop", "cov_s",
#                        "weight.adm.s", "muac.adm.s", "Recov_prop", "Def_prop", "Dea_prop",
#                        "Hosp_prop", "NevRecov_prop", "Transf_prop", "relapse_prop",
#                        "relapse_treat_prop", "recov_change", "n_pop_m", "cov_m", "Recov_prop_m", "Def_prop_m",
#                        "Dea_prop_m", "Hosp_prop_m", "NevRecov_prop_m", "deteriorate_prop_m",
#                        "Currency", "OtherCurrency", "conv_rate", "budget.program", "rutf.cost", "rusf.cost", "drug.cost",
#                        "labor.cost", "nonlabor.cost", "transport.cost", "opportunity.cost",
#                        "inp.hosp.cost")
# 
#   # Dynamically create an observer for each
#   for (input_id in inputs_to_track) {
#     local({
#       id <- input_id  # lock current input name
#       observeEvent(input[[id]], {
#         print(paste(id, "changed to:", input[[id]]))
#         log_to_database("input_change", list(
#           input_id = id,
#           value = as.character(input[[id]]),
#           timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#         ))
#       }, ignoreInit = TRUE)
#     })
#   }
# 
# 
#   # Session end cleanup
#   onSessionEnded(function() {
#     print("Session ending, cleaning up...")
#     if (!is.null(conn) && dbIsValid(conn)) {
#       tryCatch({
#         log_to_database("session_end", list(
#           timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
#         ))
#         dbDisconnect(conn)
#         print("Database connection closed")
#       }, error = function(e) {
#         print(paste("Error closing connection:", e$message))
#       })
#     }
#   })
# 
# }, error = function(e) {
#   print(paste("Telemetry setup error:", e$message))
# })

})
  
  
  
    

  