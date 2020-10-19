
<!-- README.md is generated from README.Rmd. Please edit that file -->

Wedding Risk Analyzer
=====================

This is the 2nd version of the original app I created using
`flexdashboard` when going through wedding planning. For this latest
version, I wanted to:

-   Reinforce learning from a recent Shiny with AWS course I completed
-   Practice object oriented programming with python
-   Learn how to dockerize a shiny app that uses both python and R

Run App
-------

To run the app, clone this repo locally and navigate to the directory.
Run the following command in your terminal to build the image:

    docker build -t wedding_risk .

Next, run the following command in your terminal:

    docker run wedding_risk:latest 

Then open your browser and go to `http://localhost:3838/` to interact
with the app.
