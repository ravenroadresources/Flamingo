---
title: "Flamingo"
author: "RavenRoad S.A.S."
date: "9/24/2017"
output: html_document
---


[![Travis-CI Build Status](https://travis-ci.org/ravenroadresources/Flamingo.svg?branch=master)](https://travis-ci.org/ravenroadresources/Flamingo)


## Flamingo - Fluid Properties Uncertainty Analysis 

This package is meant to be a support for the exploration of HC.
Its use is to charcterize the uncertainty of fluid properties based on a MonteCarlo
simulation. Given API and GOR ranges, together with reservoir pressure and temperature,
it calculates derived properties such as Bo, saturation pressures and viscosity and their
uncertainty ranges. 

The package relies on *Prosper (C)* to run the calculations.

&nbsp;

To install the package, run `devtools::install_github("ravenroadresources/Flamingo")`

To run the application, run `Flamingo::run()`

&nbsp;

Developed by: [RavenRoad S.A.S.](http://www.ravenroadresources.com)

