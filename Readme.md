# Simulation study comparing methods for Lasso selective inference

This repository provides example code to create simulated datasets and 
pre-process the real-world dataset used in:

Kammer M, Dunkler D, Michiels S, Heinze G. Evaluating methods for Lasso
selective inference in biomedical research by a comparative simulation study. 
(Pre-print version available at <https://arxiv.org/abs/2005.07484>)

## Usage

Each file provides standalone code which can be sourced to play around with the 
simulation designs or obtain the dataset used in the real-world example.

## Contents

- `Realistic_Setup_Demo.R`: code for the realistic simulation setup.
- `Toy_Setup_Demo.R`: code for the toy simulation setup.
- `Bodyfat_Demo.R`: code for processing the dataset for the real data example.

## Prerequisites

The code uses the statistical software `R` (>= 4.0) and the `simdata` 
(>= 0.3.0.9000) package available
[here](https://github.com/matherealize/simdata).
