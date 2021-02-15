library(shiny)
library(tidyverse)
library("readxl")

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



# Load the original spreadsheets
# ----------------------------------

Q1 <- read_csv("../normalized_versions/corrected_translated/Q1_norm_background_04.10.18.csv") 

Q2 <- read_csv("../normalized_versions/corrected_translated/Q2_norm_Previous3months_15.08.20.csv")

Q3 <- read_csv("../normalized_versions/corrected_translated/Q3_norm_4to6months_26.01.20.csv")

Q4 <- read_csv("../normalized_versions/corrected_translated/Q4_norm_7to12months_27.01.21.csv")

Q5y1 <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearI_28.01.21.csv") 
Q5y2 <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearII_28.01.21.csv")
Q5y1B <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearI_27.01.21.csv")
Q5y2B <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearII_28.01.21.csv")

Q6F <- read_csv("../normalized_versions/corrected_translated/Q6_norm_ParentsF_FoodFrequency_28.01.21.csv")
Q6M <- read_csv("../normalized_versions/corrected_translated/Q6_norm_ParentsM_FoodFrequency_28.01.21.csv")
Q7 <- read_csv("../normalized_versions/corrected_translated/Q7_norm_depression_unkown.csv")

Q8 <- read_csv("../normalized_versions/corrected_translated/Q8_norm_HouseDustCollection_28.01.21.csv")

Q9 <- read_csv("../normalized_versions/corrected_translated/Q9_norm_0to3 months_02.02.2020.csv")

# ----------------------------------



# UI
ui <- fluidPage( 
  # ----------------------------------
  h1("HELMi contextual dataset", align = "center"),
  HTML('<br/>'),
  p("Explore and download the questionnaire datasets from the HELMi cohort"),
  HTML('<center><img src="microbes.jpg" width="100%"></center>'),
  HTML('<br/>'),
  # ----------------------------------
  wellPanel(fluidRow(
    h2("Questionnaire answered", align = "center"),
    HTML('<br/>'),
    # questionnaire inputs
    # ----------------------------------
    column(3,
           checkboxGroupInput("quest1", "Unique questionnaires:",
                               c("Background"="A_background",
                                 "MotorNutrition year 1"="F1_NutritionMotorDev_year1",
                                 "MotorNutrition year 2"="F2_NutritionMotorDev_year2"),
                               selected="A_background")),
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
    column(1,
           downloadButton("downloadData", "Download selection")
           ))),
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
  
  # ----------------------------------
  # Reactive elements search
  
  # getting familly ID from Background questionnaire selection
  ques1_dataset <- reactive({
    question_long %>% filter(questionnaire %in% input$quest1) %>% 
      group_by(familly_ID) %>% tally() %>% ungroup() %>% rename("nb_reg"="n") %>%
      filter(nb_reg==length(input$quest1)) %>% select(familly_ID) %>% unique()
  })
  
  # getting familly ID from Regular questionnaire selection
  Reg_dataset <- reactive({
    question_wide %>% filter(B_0to3months<=input$B_0to3months[2] & B_0to3months>=input$B_0to3months[1]) %>%
                      filter(C_4to6months<=input$C_4to6months[2] & C_4to6months>=input$C_4to6months[1]) %>%
                      filter(D_7to12months<=input$D_7to12months[2] & D_7to12months>=input$D_7to12months[1]) %>%
                      filter(E_Previous3Months<=input$E_Previous3Months[2] & E_Previous3Months>=input$E_Previous3Months[1]) %>%
                      select(familly_ID)
  })
  
  # get all concerned famillies
  familly_list <- reactive({
    if(nrow(ques1_dataset())==0){
      Reg_dataset()
    }else if(nrow(Reg_dataset())==0){
      ques1_dataset()
    }else{
      inner_join(Reg_dataset(), ques1_dataset())
    }
    
  })
  
  # ----------------------------------
  # Table output
  output$table1 <- renderTable({
    inner_join(familly_list(), question_long) %>% group_by(questionnaire) %>% tally() %>%
      filter(!questionnaire %in% c("all_inf", "all_parents", "all_main", "all_reg"))
  })
  
  # ----------------------------------
  # Text output
  output$text <- renderText({ 
    paste ('nb of famillies selected:', as.character(nrow(familly_list())))})
  # ----------------------------------

  
  # ----------------------------------
  # Download button

  
output$downloadData <- downloadHandler(
    filename = function() {
      paste("Q1_background-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(background_dataset(), file)
    }
  )

  
}

shinyApp(server = server, ui = ui)