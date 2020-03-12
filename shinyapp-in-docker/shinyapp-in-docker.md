# Deploying Shiny Docker images with ShinyProxy

In a [previous blog post](https://www.statworx.com/at/blog/running-your-r-script-in-docker/) I have shown you how to run your R-scripts inside a docker container. For many of the projects we work on here at [STATWORX](https://www.statworx.com/de/data-science/) we end up using the [RShiny framework](https://shiny.rstudio.com/) to build our R-scripts into interactive applications. Using containerization for the deployment of ShinyApps has a multitude of advantages. There are the usual suspects such as easy cloud deployemnt, scalability and easy scheduling, but it also addresses on of RShiny's essential drawbacks: Shiny creates only a single R session per app, meaning that if multiple users access the same app, they all work with the same R session, leading to a multitude of problems. With the help of Docker we can address this issue and and start a container instance for every user - circumventing this problem by giving every user access to their own instance of the app and their own corresponding R session. 

So let's move on from simple R-scripts and run entire ShinyApps in Docker now!

