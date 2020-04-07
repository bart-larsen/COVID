#!/bin/bash
pushd .
cd ~/Documents/BenProjects/covid19/covid-19-data
git pull
popd

R -e "rmarkdown::render('CovidPlot.Rmd',output_file='CovidPlot.html')"

cp CovidPlot.html docs/index.html

git add --all
git commit -m "daily update"

open -a "Google Chrome" https://bart-larsen.github.io/COVID/