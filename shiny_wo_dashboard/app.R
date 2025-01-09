# Libraries --------------------------------------------------------------------

library(shiny)
library(bslib)
library(DT)
library(stringr)
library(dplyr)
library(fontawesome)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(googlesheets4)


# Connect to Google Sheet Database ---------------------------------------------

# the R script that connects to the database
source(file="db_connection.R")

# load in data using the function derived from the source file above
df <- get_data()

# create a df that includes html buttons for editing and deleting information. 
df <- df %>% 
  # rowwise() needed, otherwise the "id" includes all dates
  rowwise() %>%
  
  mutate(buttons = paste0(as.character(tags$button(type="button", id=paste0("edit_", date), class="action-button shiny-bound-input", onclick="get_id(this.id)", style="border: none; background: none;", fa("pencil", fill="yellow3", stroke=NULL))))) %>%
  
  mutate(buttons = paste0(buttons, as.character(tags$button(type="button", id=paste0("delete_", date), class="action-button shiny-bound-input", onclick="get_id(this.id)", style="border: none; background: none;", fa("trash", fill="red", stroke=NULL)))))

# a function needed to verticalize data for analysis
verticalize <- function(df, col_names, dates) {
  
  df <- df %>%
    pivot_longer(col_names, names_to = "activity", values_to = "value") %>%
    filter(date >= dates[1] & date <= dates[2])
  
  return(df)
  
}



# UI ---------------------------------------------------------------------------

ui <- page_navbar(
  fillable=FALSE,
  title = "Workout Dashboard",
  bg = "#558a36",
  
  ## Data Nav Panel -----------------------------------------------------------
  # This panel is mean to show the data and allow an interface for the user
  # to create, edit, and delete data.
  nav_panel(
    title = "Data",
    # include JavaScript to editing and deleting can work properly
    useShinyjs(),
    shiny::includeScript("utils.js"),
    # wrapped in a div so that the table and button stay together without unreasonable spacing
    # in between
    tags$div(
      DT::DTOutput("table"),
      actionButton("add_data", "Add Data", width = "20%", icon=icon("plus"), style="background-color: lightgreen; border-color: black; font-size: 16px;"))
    
  ),
  ## Analysis Nav Panel -------------------------------------------------------
  # This panel is intended to generate graphics and basic statistics on the data.
  nav_panel(
    title = "Analysis",
    layout_columns(
      ### Card: Choosing the Activities to Analyze -----------------------------
      card(
        card_header(
          
          "Choose the Activities You Would Like to Analyze"
        
        ),
        card_body(
          checkboxGroupInput("activity_select", "",
                             choices=c("Pushups" = "pushups", "Situps" = "situps",
                                       "Squats" = "squats", "Running" = "run_mi",
                                       "Walking" = "walk_mi", "Biking" = "bike_mi"))
        )
      ),
      
      card(
        card_header(
          
          "Choose the Date Range You Are Interested in:"
          
        ),
        card_body(
          
          dateRangeInput("date_range", "", start=Sys.Date()-7)
          
        )
        
      )
      
    ),
    
    card(
      
      plotOutput("plot")
      
    ),
    
    uiOutput("value_boxes", style="overflow: auto; height: auto; display: flex; flex-wrap: wrap; flex-direction: row-reverse; align-items: flex-end; justify-content: flex-end;")
    
  )
  
)
  


# Server -----------------------------------------------------------------------

server <- function(input, output) {

    output$table <- renderDT({datatable(df,
                              escape = FALSE,
                              editable = TRUE,
                              selection = "none",
                              extensions = "Responsive",
                              options = list(responsive = TRUE))

    })


    # observer that gets the id of the button that is clicked.
    observeEvent(input$current_id, {

      print(input$current_id)
      # store the id as a variable
      value <- input$current_id

      # if the id of the clicked button is edit...
      if(str_detect(value, "edit_\\d{4}-\\d{2}-\\d{2}")){

        # the date of the selected button
        selected_date <- as.Date(str_extract(value, "\\d.*"))

        # filtered dataframe
        filt_df <- df %>%
          filter(date == selected_date)

        #
        showModal(modalDialog(

          title = sprintf("You have clicked button %s", value),
          dateInput("edit_date", "Date", value=selected_date),
          numericInput("edit_pushups", "Pushups:", value=filt_df$pushups[1]),
          numericInput("edit_situps", "Situps:", value=filt_df$situps[1]),
          numericInput("edit_squats", "Squats:", value=filt_df$squats[1]),
          numericInput("edit_run", "Run (mi):", value=filt_df$run_mi[1]),
          numericInput("edit_walk", "Walk (mi):", value=filt_df$walk_mi[1]),
          numericInput("edit_bike", "Bike (mi):", value=filt_df$bike_mi[1]),
          textInput("edit_other", "Other:", value=filt_df$other[1]),
          textInput("edit_comments", "Comments:", value=filt_df$comments[1]),

          size="s",
          
          footer = tagList(
            actionButton("edit_data", "", style="background-color: lightgreen;", icon=icon("check")),
            modalButton("", icon = icon("cancel"))
          )

        ))
        
        
        
      }

      if(str_detect(value, "delete_\\d{4}-\\d{2}-\\d{2}")){

        # the date of the selected button
        selected_date <- as.Date(str_extract(value, "\\d.*"))

        # filtered dataframe
        filt_df <- df %>%
          filter(date == selected_date)

        showModal(modalDialog(

          title = sprintf("You have clicked button %s", value),
          "Are you sure you would like to delete this?"


        ))


      }

    })
  
  observeEvent(input$edit_data, {
    
    showModal(modalDialog(
      title = "Confirmation",
      strong("Are you sure you would to edit this data?"),
      footer = tagList(actionButton("confirm_edit", "Confirm"),
                       modalButton("Cancel"))))
    
    observeEvent(input$confirm_edit, {
      
      # write to googlesheet
      # update dataframe in rshiny
      # df <- df %>%
      #   mutate()
      # 
      # 
      # googlesheets4::sheet_write(df, ss=sheet)
      
      
    })
  
    
  })


  # reactive value that listens on the radio buttons
  selected_activities <- reactive({

    x <- input$activity_select
    return(x)

  })

  # reactive value that listens on the date range input
  dates <- reactive({

    range <- input$date_range

  })

  output$plot <- renderPlot({

    # require the selected activities reactive value to be truthy before
    # proceding with code below
    req(selected_activities())

    ggplot(verticalize(df, selected_activities(), dates()), aes(date, value)) +
      theme_bw() +
      theme(strip.text = element_text(face="bold", size=12)) +
      theme(strip.background = element_rect(fill="lightgreen")) +
      theme(axis.title = element_blank()) +
      theme(axis.text = element_text(size=12)) +
      geom_line() +
      geom_point() +
      facet_wrap(vars(activity),labeller= as_labeller(c(pushups="Pushups", situps="Situps",
                                                        squats="Squats", run_mi="Run (mi)",
                                                        walk_mi = "Walk (mi)", bike_mi="Bike (mi)")))


  })

  # create valueBoxOutputs dynamically based on what activities and date ranges
  # are selected
  output$value_boxes <- renderUI({

    req(selected_activities())

    activities <- selected_activities()

    html_elements <- lapply(activities, function(activity){

      element <- value_box(title = paste0("Average ", {{ activity }}),
                           value = round(mean(filtered_df()[[{{activity}}]]), digits=2),
                           fill = FALSE,
                           style = "width: 25%; margin: 10px")
      
      # populate the dynamically created valueBox's with values
      # output$pushups_vbox <- renderValueBox({value=valueBox(round(mean(filtered_df()$pushups), digits=2), "Average Pushups Per Day", icon=icon("heartbeat"), color="red")})
      # output$squats_vbox <- renderValueBox({value=valueBox(round(mean(filtered_df()$situps), digits=2), "Average Situps Per Day", icon=icon("heartbeat"), color="red")})
      # output$situps_vbox <- renderValueBox({value=valueBox(round(mean(filtered_df()$squats), digits=2), "Average Squats Per Day", icon=icon("heartbeat"), color="red")})
      # output$run_mi_vbox <- renderValueBox({value=valueBox(round(mean(filtered_df()$run_mi), digits=2), "Average Run Distance Per Day", icon=icon("heartbeat"), color="red")})
      # output$walk_mi_vbox <- renderValueBox({value=valueBox(round(mean(filtered_df()$walk_mi), digits=2), "Average Walk Distance Per Day", icon=icon("heartbeat"), color="red")})
      # output$bike_mi_vbox <- renderValueBox({value=valueBox(round(mean(filtered_df()$bike_mi), digits=2), "Average Biking Distance Per Day", icon=icon("heartbeat"), color="red")})
      # 
      # 

    })

    return(html_elements)

  })

  # create a filtered dataframe that reacts to the dates selected by the user
  filtered_df <- reactive({

    dates <- dates()
    start <- dates[1]
    end <- dates[2]

    df <- df %>%
      filter(date >= start & date <= end)

    return(df)

  })


} # server bracket

# Notes ------------------------------------------------------------------------

## Left off filling in functionality of add, edit, and delete

# Run Application -------------------------------------------------------------- 
shinyApp(ui = ui, server = server)
