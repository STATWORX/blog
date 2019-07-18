# Dynamic UI Elements in Shiny

At STATWORX we regularly deploy our project results with the help of Shiny. It is not only an easy way of letting potential users interact with your R-code, it's also fun to design a good-looking app. 

One of the biggest strengths of Shiny is it's inherent reactivity, after all being reactive to user input is a web-applications prime purpose. It is unfortunate that many apps seem to only make use of Shiny's reactivity on the server side while keeping the UI completely static. This does not have to be necessarily bad - some apps would not profit from having dynamic UI elements and adding them regardless could quickly result in the app feeling gimmicky. But in many cases adding reactivity to the UI can not only result in less clutter on the screen, but also cleaner code. And we all like that, don't we? 

## The Tools

