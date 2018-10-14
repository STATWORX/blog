shinyUI(fluidPage(
  title = "Friend-Tracker",
  h1("Keep record of your friends"),
  sidebarPanel(
    h4("Log a friend's visit"),
    textInput("name", "Name", placeholder = "Name ..."),
    fluidRow(
      column(6, dateInput("exp_date", "Expected arrival date")),
      column(6, timeInput("exp_time", "Time", seconds = FALSE))
    ),
    fluidRow(
      column(6, dateInput("act_date", "Actual arrival date")),
      column(6, timeInput("act_time", "Time", seconds = FALSE))
    ),
    numericInput("acceptance", "How many minutes are you willing to forgive?",
                 value = 5, min = 0, max = NA, step = 1),
    actionButton("submit", "Submit", class = "btn-primary"),
    hr(),
    tableOutput("friend_score")
  ),
  mainPanel(
    tableOutput("visit_log")
  )
))