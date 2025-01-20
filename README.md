# F4F: Forge4Flame

<p align="center">
  <img src="./inst/Shiny/www/F4Ficon.png" alt="F4F Logo" width="300">
</p>

Shiny application to build a customised environment wit different rooms for flame ABM simulations

**A cellular biologist’s toolbox for data analysis.**


## Required Installed Packages

The following R packages must be installed:

```r
install.packages(c(....))
```

## How to Install

To install F4F, you can use **devtools**:

```r
install.packages("devtools")
devtools::install_github("qBioTurin/F4F", ref="main", dependencies=TRUE)
```

## How to Run

To run the Shiny application:

```r
F4F::F4F.run()
```

## Docker

You need to have Docker installed on your machine. For more information, see the [Docker installation guide](https://docs.docker.com/engine/installation/).

To download all the Docker images used by **F4F**, you can use:

```r
library(F4F)
downloadContainers()
```

The Docker images are freely available at the following [link](https://hub.docker.com/r/qbioturin/).

## How to Run the Application with Docker

To run the F4F application through its Docker image, use the following R function:

```r
library(F4F)
docker.application.run()
```

<p align="center">
  <a href="https://qbio.di.unito.it/">
    <img src="./inst/Shiny/www/Logo_QBio.png" alt="QBio Logo" width="200">
  </a>
</p>

## Diclaimer:
F4F developers have no liability for any use of F4F functions, including without limitation, any loss of data, incorrect results, or any costs, liabilities, or damages that result from the use of F4F. 

## How to cite

```
Nature
```

