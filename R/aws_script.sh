#! bin/bash

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb [arch=amd64,i386] https://cran.rstudio.com/bin/linux/ubuntu xenial/'
sudo apt-get install r-base

git init
git remote add simulation https://github.com/stijnmasschelein/complementarity-simulation.git
git fetch simulation

cd complementarity_simulation/overleaf
Rscript R/big_main_simulation_additional.R