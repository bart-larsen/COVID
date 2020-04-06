#!/bin/bash

git pull

R -e "rmarkdown::render('CovidPlot.Rmd',output_file='CovidPlot.html')"

cp CovidPlot.html docs/index.html

git add --all
git commit -m "daily update"
