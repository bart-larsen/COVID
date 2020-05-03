#!/bin/bash
cd ~/Documents/BenProjects/covid19/covid-19-data
git pull
cd ~/Documents/BenProjects/covid19/

R -e "rmarkdown::render('CovidPlot.Rmd',output_file='CovidPlot.html')"

cp CovidPlot.html docs/index.html
cp ~/Documents/BenProjects/covid19/covid-19-data/us-counties.csv ~/Documents/BenProjects/covid19/shiny_app/us-counties.csv

git add --all
git commit -m "daily update"
git push

open -a "Google Chrome" https://bart-larsen.github.io/COVID/