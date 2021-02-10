library(shiny)
library(tidyverse)
library("readxl")

# Load the spreadsheets
# ----------------------------------
Voc <- read_excel("../questionnaire_vocabulary.xlsx")

Q1 <- read_csv("../normalized_versions/corrected_translated/Q1_norm_background_04.10.18.csv") %>% 
  add_column(questionnaire="A_background")
Q1$inf_DOB <- as.character(Q1$inf_DOB)

Q2 <- read_csv("../normalized_versions/corrected_translated/Q2_norm_Previous3months_15.08.20.csv") %>% 
  add_column(questionnaire="E_Previous3Months")

Q3 <- read_csv("../normalized_versions/corrected_translated/Q3_norm_4to6months_26.01.20.csv") %>% 
  add_column(questionnaire="C_4to6months")

Q4 <- read_csv("../normalized_versions/corrected_translated/Q4_norm_7to12months_27.01.21.csv") %>% 
  add_column(questionnaire="D_7to12months")

Q5y1 <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearI_28.01.21.csv") %>% 
  add_column(questionnaire="F1_NutritionMotorDev_year1")
Q5y2 <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_v2_yearII_28.01.21.csv") %>% 
  add_column(questionnaire="F2_NutritionMotorDev_year2")
Q5y1B <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearI_27.01.21.csv") %>%
  add_column(questionnaire="F1_NutritionMotorDev_year1")
Q5y2B <- read_csv("../normalized_versions/corrected_translated/Q5_norm_NutritionMotorDev_yearII_28.01.21.csv") %>% 
  add_column(questionnaire="F2_NutritionMotorDev_year2")

Q6F <- read_csv("../normalized_versions/corrected_translated/Q6_norm_ParentsF_FoodFrequency_28.01.21.csv") %>% 
  add_column(questionnaire="G_ParentF_FoodFrequency")
Q6M <- read_csv("../normalized_versions/corrected_translated/Q6_norm_ParentsM_FoodFrequency_28.01.21.csv") %>% 
  add_column(questionnaire="G_ParentM_FoodFrequency")
Q7 <- read_csv("../normalized_versions/corrected_translated/Q7_norm_depression_unkown.csv") %>% 
  add_column(questionnaire="G_MotherDepression")

Q8 <- read_csv("../normalized_versions/corrected_translated/Q8_norm_HouseDustCollection_28.01.21.csv") %>% 
  add_column(questionnaire="H_HouseCollection")

Q9 <- read_csv("../normalized_versions/corrected_translated/Q9_norm_0to3 months_02.02.2020.csv") %>% 
  add_column(questionnaire="B_0to3months")

question_wide <- read_csv("../normalized_versions/corrected_translated/per_familly_questionnaires.csv") 

question_long <- question_wide %>%
  pivot_longer(!familly_ID, names_to="questionnaire", values_to="nb_responses") %>%
  mutate(questionnaire=ifelse(questionnaire=="F1_NutritionMotorDev_year1_Version2", "F1_NutritionMotorDev_year1", 
                              ifelse(questionnaire=="F1_NutritionMotorDev_year1_Version1", "F1_NutritionMotorDev_year1", 
                              ifelse(questionnaire=="F2_NutritionMotorDev_year2_Version2", "F2_NutritionMotorDev_year2",
                              ifelse(questionnaire=="F2_NutritionMotorDev_year2_Version1", "F2_NutritionMotorDev_year2", questionnaire))))) %>%
  filter(nb_responses!=0)
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
    # questionnaire inputs
    # ----------------------------------
    column(3,offset = 1,
           checkboxGroupInput("quest1", "Questionnaire answered:",
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
                       value = c(0,8)),
    ))),
  # ----------------------------------
  fluidRow(
           column(5,offset = 1, h3(textOutput("text")),
                  HTML('<br/>'),
                  tableOutput("table1")),
           column(5,plotOutput("plot2")),
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
  # Reactive elements
  ques1_dataset <- reactive({
    question_long %>% filter(questionnaire %in% input$quest1) %>% 
      group_by(familly_ID) %>% tally() %>% ungroup() %>% rename("nb_reg"="n") %>%
      filter(nb_reg==length(input$quest1)) %>% select(familly_ID) %>% unique()
  })
  
  Reg_dataset <- reactive({
    question_wide %>% filter(B_0to3months<=input$B_0to3months[2] & B_0to3months>=input$B_0to3months[1]) %>%
                      filter(C_4to6months<=input$C_4to6months[2] & C_4to6months>=input$C_4to6months[1]) %>%
                      filter(D_7to12months<=input$D_7to12months[2] & D_7to12months>=input$D_7to12months[1]) %>%
                      filter(E_Previous3Months<=input$E_Previous3Months[2] & E_Previous3Months>=input$E_Previous3Months[1]) %>%
                      select(familly_ID)
      
  })
  
  familly_list <- reactive({
    inner_join(Reg_dataset(), ques1_dataset())
  })
  
  background_dataset <- reactive({
    left_join(familly_list(), Q1) %>% 
      select(familly_ID, inf_DOB, inf_Sex)
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
  # Plot output
  output$plot2 <- renderPlot({  
    Sex_data <- background_dataset() %>% mutate(Sex=ifelse(inf_Sex==1, "Girl", 
                                                   ifelse(inf_Sex==2, "Boy", "Unknown"))) %>%
                                      group_by(Sex) %>% tally()
    Sex_data$Sex <- as.factor(Sex_data$Sex)
    Sex_data  %>%  ggplot(aes(x="", y=n, fill=Sex)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void()
  })
  
  
  
}

shinyApp(server = server, ui = ui)