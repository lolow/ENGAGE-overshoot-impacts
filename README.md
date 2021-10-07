# ENGAGE impacts and overshoot


This code has been used for the data analysis and the figures from Drouet et al. (2021). It is written in R and relies on two packages for reproducibility: `drake` for the workflow and `renv` for the dependency management. To run the analysis, you need to install first the `renv` packages, as it will be suggested if you open R in the main directory. Then execute once the following command to install the missing dependencies:


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