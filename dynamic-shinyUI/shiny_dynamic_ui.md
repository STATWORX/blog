# Dynamic UI Elements in Shiny

At STATWORX we regularly deploy our project results with the help of Shiny. It is not only an easy way of letting potential users interact with your R-code, it's also fun to design a good-looking app. 

One of the biggest strengths of Shiny is it's inherent reactivity, after all being reactive to user input is a web-applications prime purpose. It is unfortunate that many apps seem to only make use of Shiny's reactivity on the server side while keeping the UI completely static. This does not have to be necessarily bad - some apps would not profit from having dynamic UI elements and adding them regardless could quickly result in the app feeling gimmicky. But in many cases adding reactivity to the UI can not only result in less clutter on the screen, but also cleaner code. And we all like that, don't we? 

### The Tools

Shiny natively provides convenient tools to turn the UI of any app reactive to input. We are namely going to look at the `renderUI` (in conjunction with `lapply`) and `conditionalPanel` functions. 

`renderUI` is helpful because it frees us from the chains of having to define what kind of object we'd like to render in our `render`-function. `renderUI` can render any UI element, so we could, for example, let the type of the content of our `uiOutput` be reactive to an input instead of being set in stone.

`conditionalPanel` simply lets us conditionally show or hide UI elements and thus helps us reduce visual clutter. 

### `renderUI`

