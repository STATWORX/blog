# Dynamic UI Elements in Shiny - Part 2

At [STATWORX](http://www.statworx.com), we regularly deploy our project results with the help of Shiny. It's not only an easy way of letting potential users interact with your R-code, but it's also fun to design a good-looking app. 

One of Shiny's biggest strengths is its inherent reactivity after all being reactive to user input is a web-applications prime purpose. Unfortunately, many apps seem to only make use of Shiny's responsiveness on the server side while keeping the UI completely static. This doesn't have to be necessarily bad. Some apps wouldn't profit from having dynamic UI elements. Adding them regardless could result in the app feeling gimmicky. But in many cases adding reactivity to the UI can not only result in less clutter on the screen but also cleaner code. And we all like that, don't we? 

In have previously discussed the advantages of using `renderUI` in combination with `lapply()` and `do.call` in the [first part](https://www.statworx.com/at/blog/dynamic-ui-elements-in-shiny/) of this series on dynamic UI elements in Shiny. 

[author class="mtl" title="Über den Autor"]
[global_block block="15819"]