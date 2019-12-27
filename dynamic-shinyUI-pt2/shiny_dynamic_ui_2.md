# Dynamic UI Elements in Shiny - Part 2

At [STATWORX](http://www.statworx.com), we regularly deploy our project results with the help of Shiny. It's not only an easy way of letting potential users interact with your R-code, but it's also fun to design a good-looking app. 

One of Shiny's biggest strengths is its inherent reactivity after all being reactive to user input is a web-applications prime purpose. Unfortunately, many apps seem to only make use of Shiny's responsiveness on the server side while keeping the UI completely static. This doesn't have to be necessarily bad. Some apps wouldn't profit from having dynamic UI elements. Adding them regardless could result in the app feeling gimmicky. But in many cases adding reactivity to the UI can not only result in less clutter on the screen but also cleaner code. And we all like that, don't we? 

In have previously discussed the advantages of using `renderUI` in combination with `lapply()` and `do.call` in the [first part](https://www.statworx.com/at/blog/dynamic-ui-elements-in-shiny/) of this series on dynamic UI elements in Shiny. Building onto this I would like to expand our toolbox for reactive UI design with a few more options.



### `req()`

`req()` is a function from the `shiny` package whose purpose is to check whether certain requirements are met before proceeding with the calculations in a reactive environment. In most cases this is useful to avoid red error messages popping up in your ShinyApp UI when an element of your app depends on an input that doesn't have a set value yet. You may have seen one of these before:

![](/Users/oliverguggenbuehl/Intern/blog/dynamic-shinyUI-pt2/images/error.png)

These errors usually disappear once you have assigned a value to the needed inputs. `req()` makes it so that your desired output is only calculated once its required inputs have been set, thus offering an elegant way to avoid the rather garish looking error messages in your app UI. 

In terms of reactive UI design we can find other ways to make use of `req()`'s functionality. Let's assume we'd like to present the user with multiple options to choose from in the shape of a `selectInput`. Let's also assume that one of the options may call for more input from the user, let's say a comment, to more clearly explain the selection. One way to do this would be to add a static `textInput` or similar to the app. A much more elegant solution would be to conditionally render the second input to only appear if the proper option has been selected. `req()` offers an easy and intuitive way to add such reactivity to our UI. The image below shows how this looks in practice.

![example1](/Users/oliverguggenbuehl/Intern/blog/dynamic-shinyUI-pt2/images/example1.png)

The comment box isn't hidden or disabled in the left example, it simply doesn't exist unless the `selectInput()` takes on the value of "B". This is achieved by using `renderUI` and `req()` in combination as shown in the following example: 

```R
output$conditional_comment <- renderUI({
    # specify condition
    req(input$select == "B")
  
    # execute only if condition is met
    textAreaInput(inputId = "comment", 
                  label = "please add a comment", 
                  placeholder = "write comment here") 
  })
```

Within `req()` the condition to be met is specified and the rest of the code inside the reactive environment created by `renderUI()` is only executed if that condition is met. What is nice about this solution is that if the condition has not been met there will be no red error messages or other visual clutter popping up in your app, just like what we've seen at the beginning of this chapter.

Here's the complete code for a small example app:

```R
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(
    selectInput(inputId = "select", 
                label = "please select an option", 
                choices = LETTERS[1:3]),
    uiOutput("conditional_comment")
  ),
  dashboardBody(
    uiOutput("selection_text"),
    uiOutput("comment_text")
  )
)

server <- function(input, output) {
  
  output$selection_text <- renderUI({
    
    paste("The selected option is", input$select)
  })
  
  output$conditional_comment <- renderUI({
    req(input$select == "B")
    
    textAreaInput(inputId = "comment", 
                  label = "please add a comment", 
                  placeholder = "write comment here")
  })
  
  output$comment_text <- renderText({
    
    input$comment
  })
}

shinyApp(ui = ui, server = server)
```



### `conditionalPanel()`

Out of all the tools available for reactive UI design this is probably the most widely used. The results obtained with `conditionalPanel()` are quite similar to what `req()` allowed us to do in the example above, but there are a few key differences. 

`conditionalPanel()` was designed to specifially enable shiny programmers to conditionally show or hide UI elements. Unlike the `req()`-method, `conditionalPanel()` is evaluated within the UI-part of the app, meaning that it doesn't rely on `renderUI` to conditionally render the various inputs of the shinyverse. But wait, you might ask, how can shiny evaluate any conditions in the UI-side of the app? Isn't that sort of thing always done in the server-part? Well yes, that is true if the expression is written in R. To get around this, `conditionalPanel()` relies on JavaScript to evaluate its conditions. After stating the condition in JS we can add any given UI-elements to our `conditionalPanel()` as shown below:

```R
conditionalPanel(
      # specify condition
      condition = "input.select == 'B'",
  
      # execute only if condition is met
      textAreaInput(inputId = "comment", 
                    label = "please add a comment", 
                    placeholder = "write comment here")
    )
```

This code chunk displays the same behaviour as the example shown in the last chapter with one major difference: It is now part of our ShinyApp's UI-function unlike the `req()`-solution, which was a `uiOutput` calculated in the server-part of the app and later passed to our UI-function as a list-element. 

Rewriting the app to include `conditionalPanel()` instead of `req()` yields a script that looks something like this:

```R
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(),
  dashboardSidebar(
    selectInput(inputId = "select", 
                label = "please select an option", 
                choices = LETTERS[1:3]),
    conditionalPanel(
      condition = "input.select == 'B'",
      textAreaInput(inputId = "comment", 
                    label = "please add a comment", 
                    placeholder = "write comment here")
    )
  ),
  dashboardBody(
    uiOutput("selection_text"),
    textOutput("comment_text")
    )
)

server <- function(input, output) {
  
  output$selection_text <- renderUI({
    
    paste("The selected option is", input$select)
  })
  
  output$comment_text <- renderText({
    
    input$comment
  })
}

shinyApp(ui = ui, server = server)
```





[author class="mtl" title="Über den Autor"]
[global_block block="15819"]

