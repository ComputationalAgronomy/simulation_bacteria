# simulation_bacteria

`simmicrobes` is a R package

Most of the structure and setup are taking from https://r-pkgs.org/


## Use R package
```R
library(devtools)

## If you are one level above the simmicrobes/ folder
load_all("simmicrobes") 


## If you are in the package folder `cd simmicrobes/`
load_all() 

```

### R package skeleton with devtools
```R
library(devtools)
create_package() # Setup package structure
use_testthat() # Setup testing folder structure
```

To check and test packages in R. It's easier to run this inside the package dir `cd simmicrobes;`

```R
getwd() # Should be simmicrobes
library(devtools)
document()
check()
test()

## alternatively, if you are one level above the package folder
document("simmicrobes")
check("simmicrobes")
test("simmicrobes")
```

To build and check package in outside R
```bash
# cd "One_level_above_simmicrobes/"
R CMD build simmicrobes/
R CMD check simmicrobes_version_number.tar.gz 
```
