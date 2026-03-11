# DepAC: Deposition of Airborne Compounds

DepAC (Deposition of Airborne Compounds) is a scientific Fortran library and toolkit for modeling atmospheric deposition processes. Developed by RIVM, DepAC provides robust routines for calculating dry deposition velocities, resistances, conductances, and compensation points for a specific set of chemical species and land use types.

## Features
- Modular Fortran codebase with strict input validation and error handling.
- Calculation of canopy, soil, and stomatal resistances.
- Support for compensation point logic and effective resistance computation
- Configurable logging and verbosity for scientific debugging.
- Designed for integration with other atmospheric and environmental models.
- Fully compatible with [fpm](https://github.com/fortran-lang/fpm) for easy building and testing

## Getting Started
DepAC is intended for use by atmospheric scientists, modelers, and developers working on air quality, deposition, and ecosystem impact studies. The code is organized into modules for configuration, input validation, calculation routines, and output management. See the documentation (`doc`) and example usage for details on integrating DepAC into your workflow.

### Installing DepAC

To use DepAC within a Conda environment, follow these steps:

1. **Create and activate the Conda environment:**
    ```bash
    conda env create -f depac.yml
    conda activate depac
    ```
2. **Install fortitude**
    ```bash
    pip install fortitude-lint
    ```

2. **Verify the installation:**
    ```bash
    fpm test
    ```

4. **Build the application:**
    ```bash
    fpm build --profile release
    ```


# Code Coverage

Unit-tests currently at 100% code coverage for the calculation modules
with 100% of the subroutines tested.

Verify using:
```bash
fpm test --profile debug --flag --coverage
mkdir -p coverage_html && (   cd coverage_html &&   gcov ../build/gfortran_*/DepAC/src*.gcda -r ../src/ -b; )
geninfo ./build/gfortran_*/DepAC/src*.gcda -b . -o ./coverage_html/coverage.info
genhtml ./coverage_html/coverage.info -o ./coverage_html/temp
```

A coverage report will be generated in the `coverage_html` folder. Open `coverage_html/temp/index.html` in a web browser to view the detailed coverage report.

> **Note:**  
> It is wise to use the following pre-push hook to check if your push will pass code checks. Add to the `.git/hooks` folder. (filename: `pre-push`) 
>
> ```bash
> #!/bin/bash
>
> echo "Running fpm test..."
> fpm test
> FPM_STATUS=$?
>
> echo "Running fortitude check..."
> fortitude check
> FORTITUDE_STATUS=$?
>
> if [ $FPM_STATUS -ne 0 ] || [ $FORTITUDE_STATUS -ne 0 ]; then
>   echo "Pre-push hook failed: tests or style check did not pass."
>   exit 1
> fi
> ```

# Version History
- 1.4.0 First releasable version of DepAC with 100% code coverage and working CI/CD implementation. This version is compatible with the new DepAC module in OPS_LT.
- 1.3.2 Significant performace improvements by using indices comparisons instead of string comparisons in determining component types and land use types.
- 1.3.1 Added better documentation and comments to the code with publications where missing.
- 1.3.0 The newest version which is working with OPS_LT for the implementation of the new DepAC module in OPS_LT.
- 1.2.0 Added ra and rb calculation functions to the public interface of depac_calc module. Also added seperated DepAC calculation functions for partial and finishing calculations.
- 0.5.0 First version with working CI/CD implementation, documentation, code review and unit-testing with 92% path coverage and 100% of subroutines tested.
- 0.0.2 Initial release of the new DepAC module.
