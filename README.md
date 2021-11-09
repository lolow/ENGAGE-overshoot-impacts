# ENGAGE - Impacts and overshoot of net-zero scenarios

This repository is released under the Apache License 2.0;
see the [LICENSE](LICENSE) for details. 

Copyright 2021 Laurent Drouet, Simone Padoan.

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

## Overview

This code has been used for the data analysis and the figures from Drouet et al. (2021). 

## Usage

It is written in R and relies on two packages for reproducibility: `drake` for the workflow and `renv` for the dependency management. To run the analysis, you need to install first the `renv` packages, as it will be suggested if you open R from the main directory. Then execute once the following command to install the missing dependencies:

```r
renv::restore()
```

To launch the data analysis, run
```
drake::r_make()
```

Note that the data analysis is heavily computing demanding, as it takes two days of computation using 85 nodes of 36 cores in the CMCC supercomputer. For this, you will need to adjust the drake configuration to your computing system. Please see the documentation at https://books.ropensci.org/drake/hpc.html. 


To build the figures, run
```r

# graphs
require("ggplot2")
require("viridisLite")
require("ggrepel")
require("ggsci")
require("scales")
require("patchwork")

#maps
require("rnaturalearth")
require("rnaturalearthdata")
require("rgeos")
require("sf")

make(plan2)

```

For any questions, please ask laurent.drouet [at] eiee.org. 

## Funding acknowledgement

<img src="./data/Flag_of_Europe.svg.png" width="80" height="54" align="left" alt="EU logo" />
This project has received funding from the European Union’s Horizon 2020 research
and innovation programme under grant agreement No. 821471 (ENGAGE).
