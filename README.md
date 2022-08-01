# simulation_bacteria

`simmicrobes` is a R package


### R package skeleton with devtools
```R
library(devtools)
create_package() # Setup package structure
use_testthat() # Setup testing folder structure
```

To check and test packages in R
`cd simmicrobes`
```R
getwd() # Should be simmicrobes
library(devtools)
document()
check()
test()
```

To build and check package in outside R
```bash
# cd "One_level_above_simmicrobes/"
R CMD build simmicrobes/
R CMD check simmicrobes_version_number.tar.gz 
```