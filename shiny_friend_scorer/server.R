shinyServer(function(input, output, session) {
  
  # Data
  rv <- reactiveValues(
    friend_score = readRDS("data/friend_score.RDS"),
    visit_log = readRDS("data/visit_log.RDS"),
    # Trigger
    ask_for_reason = TRUE,
    change_friend_score = TRUE,
    save_visit = TRUE,
    error = FALSE
  )
  
  # Friend form
  friend_data <- reactiveValues(
    name = NA,
    exp_time = NA,
    act_time = NA,
    reason = NA,
    good_reason = NA, 
    score = NA
  )
  
  # Error handler
  error <- reactiveValues(
    title = "",
    message = ""
  )
  
  # Interaction ----------------------------------------------------------------
  # Submit friend data ----
  observeEvent(input$submit, {
    # Friend exists?
    if (!input$name %in% rv$friend_score$name) {
      error$title <- "%s not found" %>% sprintf(., input$name)
      error$message <- h1("404")
      rv$error <- isolate(!rv$error)
      return()
    }
    
    # Collect data
    exp_time <- convert_to_time(input$exp_date, input$exp_time)
    act_time <- convert_to_time(input$act_date, input$act_time)
    friend_data <- set_data(friend_data, name = input$name, exp_time = exp_time,
                            act_time = act_time)
    
    is_delayed <- difftime(act_time, exp_time, units = "mins") > input$acceptance
    if (is_delayed) {
      rv$ask_for_reason <- isolate(!rv$ask_for_reason)
      return()
    }
    friend_data <- set_data(friend_data, score = 1)  
    rv$change_friend_score <- isolate(!rv$change_friend_score)

  })
  
  # Ask for reason of being late ----
  observeEvent(rv$ask_for_reason, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = "Enter reason",
      textInput("reason", "Reason", placeholder = "Enter reason ..."),
      checkboxInput("good_reason", "Good reason", value = FALSE),
      footer = actionButton("continue", "Continue", class = "btn-primary")
    ))
  })
  
  observeEvent(input$continue, {
    removeModal()
    
    # Reason entered?
    if (trimws(input$reason) == "") {
      error$title <- "No reason entered"
      error$message <- "You should enter a reason to protocol your suffering."
      rv$error <- isolate(!rv$error)
      return()
    }
    
    # Fetch data
    friend_data <- set_data(friend_data, reason = input$reason, 
                            good_reason = ifelse(input$good_reason, "yes", "no"))
    if (input$good_reason) {
      rv$save_visit <- isolate(!rv$save_visit)
    } else {
      friend_data <- set_data(friend_data, score = -1L)
      rv$change_friend_score <- isolate(!rv$change_friend_score)
    }
  })
  
  # Change friend score ----
  observeEvent(rv$change_friend_score, ignoreInit = TRUE, {
    rv$friend_score[friend_score$name == friend_data$name, "score"] <-
      isolate(rv$friend_score[friend_score$name == friend_data$name, "score"]) + 
      friend_data$score
    # Make change permanent
    saveRDS(rv$friend_score, "data/friend_score.RDS")
    rv$save_visit <- isolate(!rv$save_visit)
  })
  
  # Save visit in log file ----
  observeEvent(rv$save_visit, ignoreInit = TRUE, {
    rv$visit_log <- isolate(rv$visit_log) %>%
      rbind(tibble(
        name = friend_data$name,
        exp_time = friend_data$exp_time,
        act_time = friend_data$act_time,
        reason = friend_data$reason,
        good_reason = friend_data$good_reason
      ))
    # Make change permanent
    saveRDS(rv$visit_log, "data/visit_log.RDS")
    # Reset friend data
    friend_data <- reset_data(friend_data)
    # Done
    showNotification("Friend's visit added", type = "message")
  })
  
  # Error handling ----
  observeEvent(rv$error, ignoreInit = TRUE, {
    showModal(modalDialog(
      title = error$title,
      error$message,
      footer = actionButton("exit", "Ok", class = "btn-primary")
    ))
  })
  observeEvent(input$exit, {
    removeModal()
    friend_data <- reset_data(friend_data)
  })
  
  # Outputs --------------------------------------------------------------------
  output$friend_score <- renderTable({
    rv$friend_score %>% set_colnames(c("Name", "Score"))
  }, width = "100%", align = "lr", hover = TRUE, bordered = TRUE, digits = 0)
  
  output$visit_log <- renderTable({
    rv$visit_log %>%
      mutate(exp_time = as.character(exp_time),
             act_time = as.character(act_time)) %>%
      set_colnames(c("Name", "Expected Arrival Time", "Actual Arrival Time", 
                     "Reason for being late", "Good Reason?"))
  }, width = "100%", align = "lllll", hover = TRUE, bordered = TRUE)
})