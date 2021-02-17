library(shiny)
library(tidyverse)
library(readxl)
library(xlsx)

# Load the data cube
# ----------------------------------
question_wide <- read_csv("../normalized_versions/DataCube/questionnaire.csv") 

question_long <- question_wide %>%
  pivot_longer(!familly_ID, names_to="questionnaire", values_to="nb_responses") %>%
  mutate(questionnaire=ifelse(questionnaire=="F1_NutritionMotorDev_year1_Version2", "F1_NutritionMotorDev_year1", 
                              ifelse(questionnaire=="F1_NutritionMotorDev_year1_Version1", "F1_NutritionMotorDev_year1", 
                                     ifelse(questionnaire=="F2_NutritionMotorDev_year2_Version2", "F2_NutritionMotorDev_year2",
                                            ifelse(questionnaire=="F2_NutritionMotorDev_year2_Version1", "F2_NutritionMotorDev_year2", questionnaire))))) %>%
  filter(nb_responses!=0)


baby <- read_csv("../normalized_versions/DataCube/baby.csv") 

mother <- read_csv("../normalized_versions/DataCube/mother.csv") 

# Load the original spreadsheets
# ----------------------------------

Q1 <- read_csv("../normalized_versions/corrected_translated/Q1_norm_background_04.10.18.csv") 

# ----------------------------------



# UI
ui <- fluidPage( 
  #header section
  # ----------------------------------
  h1("HELMi contextual dataset", align = "center"),
  HTML('<br/>'),
  h3("Explore and download the questionnaire datasets from the HELMi cohort", align = "center"),
  HTML('<center><img src="microbes.jpg" width="100%"></center>'),
  HTML('<br/>'),
  # ----------------------------------
  # questionnaire inputs
  # ----------------------------------
  wellPanel(fluidRow(
    h2("Questionnaires answered", align = "center"),
    HTML('<br/>'),
    column(2,
           checkboxGroupInput("quest1", "Unique questionnaires:",
                               c("Background"="A_background",
                                 "Mother Depression"= "G_MotherDepression", 
                                 "Mother stress Q4" = "G0_MotherStress",
                                 "Mother stress year 2"= "G2_MotherStress"),
                               selected="A_background")),
    column(2,
           checkboxGroupInput("quest2", "Food questionnaires:",
                              c("MotorNutrition year 1"="F1_NutritionMotorDev_year1",
                                "MotorNutrition year 2"="F2_NutritionMotorDev_year2")),
           HTML('<br/>'),
           sliderInput("G_ParentM_FoodFrequency", "Mother Nutrition",
                      min = 0, max = 2,
                      value = c(0,2)),
           sliderInput("G_ParentF_FoodFrequency", "Partner Nutrition",
                        min = 0, max = 2,
                        value = c(0,2))),
    column(3,
           sliderInput("B_0to3months", "0 to 3months:",
                              min = 0, max = 17,
                              value = c(15,17)),
           sliderInput("C_4to6months", "4 to 6 months:",
                              min = 0, max = 7,
                              value = c(5,7))),
    column(3,
           sliderInput("D_7to12months", "7 to 12 months:",
                       min = 0, max = 6,
                       value = c(4,6)),
           sliderInput("E_Previous3Months", "Previous 3months:",
                       min = 0, max = 8,
                       value = c(0,8))),
    column(2,
           downloadButton("downloadData", "familyIDs"),
           h3(""),
           downloadButton("downloadQuest", "Background"),
           h3(""),
           downloadButton("downloadSample", "Samples"),
           ))),
    # ----------------------------------
    # background searches
    # ----------------------------------
    wellPanel(fluidRow(
      h2("Main informations", align = "center"),
      HTML('<br/>'),
      # questionnaire inputs
    # ----------------------------------
    column(1),
    column(2,
           h4("Baby", align = "center"),
           checkboxGroupInput("BirthMode", "Birth Mode:",
                              c("Vaginal"="Vaginal",
                                "C-Section"="C-section"),
                              selected="Vaginal"),
           ),
    column(2,
           h4("Mother", align = "center"),
           checkboxGroupInput("m_HasHeritableDisease", "Has heritable disease:",
                              c("Yes"="Yes",
                                "No"="No",
                                "Unknown"="Unknown"),
                              selected=c("No", "Unknown")),
           checkboxGroupInput("m_ProbioticsUse", "Probiotics during pregnancy:",
                              c("Daily"="Daily",
                                "Sometimes"="Sometimes",
                                "Weekly"="Weekly",
                                "Never"="Never"),
                              selected="Daily"),
    ),
    column(2,
           h4("Father", align = "center"),
    ),
    column(2,
           h4("Partner", align = "center"),
    ),
    column(2,
           h4("Family", align = "center"),
    ),
      
      
    )),
    # ----------------------------------
    # Display outputs
    # ----------------------------------
    fluidRow(
             column(5,offset = 1, h3(textOutput("text")),
                    HTML('<br/>'),
                    tableOutput("table1")),
             ), # end main panel
    # ----------------------------------
    includeHTML("footer.html"),
) # end NavPage panel


# ----------------------------------
# ----------------------------------
# ----------------------------------
# SERVER SIDE
# ----------------------------------
# ----------------------------------

server <- function(input, output) {
  
  # Reactive elements Questionnaire search
  # ----------------------------------
  
  # getting familly ID from Background questionnaire selection
  ques1_dataset <- reactive({
    question_long %>% filter(questionnaire %in% c(input$quest1, input$quest2)) %>% 
      group_by(familly_ID) %>% tally() %>% ungroup() %>% rename("nb_reg"="n") %>%
      filter(nb_reg==length(c(input$quest1, input$quest2))) %>% select(familly_ID) %>% unique()
  })
  
  # getting familly ID from Regular questionnaire selection
  Reg_dataset <- reactive({
    question_wide %>% filter(B_0to3months<=input$B_0to3months[2] & B_0to3months>=input$B_0to3months[1]) %>%
                      filter(C_4to6months<=input$C_4to6months[2] & C_4to6months>=input$C_4to6months[1]) %>%
                      filter(D_7to12months<=input$D_7to12months[2] & D_7to12months>=input$D_7to12months[1]) %>%
                      filter(E_Previous3Months<=input$E_Previous3Months[2] & E_Previous3Months>=input$E_Previous3Months[1]) %>%
                      filter(G_ParentM_FoodFrequency<=input$G_ParentM_FoodFrequency[2] & G_ParentM_FoodFrequency>=input$G_ParentM_FoodFrequency[1]) %>%
                      filter(G_ParentF_FoodFrequency<=input$G_ParentF_FoodFrequency[2] & G_ParentF_FoodFrequency>=input$G_ParentF_FoodFrequency[1]) %>%
                      select(familly_ID)
  })
  
  # get all concerned famillies
  Questfamilly_list <- reactive({
    if(nrow(ques1_dataset())==0){
      Reg_dataset()
    }else if(nrow(Reg_dataset())==0){
      ques1_dataset()
    }else{
      inner_join(Reg_dataset(), ques1_dataset())
    }
  })
  
  # ----------------------------------
  # Reactive elements background search
  baby_dataset <- reactive({
    baby %>% filter(inf_DeliveryMode %in% c(input$BirthMode)) %>%
      select(familly_ID) %>% unique()
  })
  
  Babyfamilly_list <- reactive({
    baby_dataset()
  })
  
  # ----------------------------------
  # Reactive elements Mother search
  mother_dataset <- reactive({
    mother %>% filter(m_HasHeritableDisease %in% c(input$m_HasHeritableDisease)) %>%
      filter(m_ProbioticsUse %in% c(input$m_ProbioticsUse)) %>%
      select(familly_ID) %>% unique()
  })
  
  motherfamilly_list <- reactive({
    mother_dataset()
  })
  
  # ----------------------------------
  # Output displays
  # ----------------------------------
  
  familly_list <- reactive({
      temp1 <- inner_join(Questfamilly_list(), Babyfamilly_list())
      inner_join(temp1, motherfamilly_list())
  })
  
  # Table output
  output$table1 <- renderTable({
    inner_join(familly_list(), question_long) %>% group_by(questionnaire) %>% tally() %>%
      filter(!questionnaire %in% c("all_inf", "all_parents", "all_main", "all_reg"))
  })
  
  # Text output
  output$text <- renderText({ 
    paste ('nb of famillies selected:', as.character(nrow(familly_list())))})
  # ----------------------------------

  
  # ----------------------------------
  # Download button
  perhe_list <- reactive({
    familly_list() %>% mutate(familly_ID=paste("Perhe", familly_ID, sep=""))
  })
  
  Q1_sel <- reactive({
    inner_join(perhe_list(), Q1)
  })
  
  
  output$downloadData <- downloadHandler(
      filename = function() {
        paste("famillyList-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(familly_list(), file, row.names = FALSE)
      }
    )
  
  output$downloadQuest <- downloadHandler(
    filename = function() {
      paste("background-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Q1_sel(), file, row.names = FALSE)
    }
  )

  
}

shinyApp(server = server, ui = ui)