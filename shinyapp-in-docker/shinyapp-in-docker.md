# Deploying Shiny Docker images with ShinyProxy

In a [previous blog post](https://www.statworx.com/at/blog/running-your-r-script-in-docker/) I have shown you how to run your R-scripts inside a docker container. For many of the projects we work on here at [STATWORX](https://www.statworx.com/de/data-science/) we end up using the [RShiny framework](https://shiny.rstudio.com/) to build our R-scripts into interactive applications. Using containerization for the deployment of ShinyApps has a multitude of advantages. There are the usual suspects such as easy cloud deployemnt, scalability and easy scheduling, but it also addresses on of RShiny's essential drawbacks: Shiny creates only a single R session per app, meaning that if multiple users access the same app, they all work with the same R session, leading to a multitude of problems. With the help of Docker we can address this issue and and start a container instance for every user - circumventing this problem by giving every user access to their own instance of the app and their own corresponding R session. 

So let's move on from simple R-scripts and run entire ShinyApps in Docker now!

### Project Setup

It is highly advisable to use RStudio's project setup when working with ShinyApps, especially when using Docker. Not only do projects make it easy to keep your RStudio neat and tidy, they also allow us to use the `renv` package to set up a package library for our specific project. This will come in especially handy when installing the needed packages for our app to the Docker image. 

