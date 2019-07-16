#! /bin/bash

screen -S simulation

sudo apt install apt-transport-https software-properties-common 
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/'
sudo apt-get update
sudo apt-get install r-base-core

mkdir simulation
cd simulation
git init
git remote add simulation https://github.com/stijnmasschelein/complementarity-simulation.git
git fetch simulation master
git pull simulation master

Rscript R/big_main_simulation_additional.R