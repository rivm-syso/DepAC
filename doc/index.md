# DepAC

## Overview


DepAC (Deposition of Airborne Compounds) is a scientific model designed to estimate the atmospheric deposition of airborne substances. A key feature of DepAC is its calculation of canopy resistance, which is essential for modeling dry deposition processes.

This project aims to make the DepAC model more transparent, publicly available, and thoroughly documented.


## Historical Context

DepAC was originally developed in the 1970s for the OPS (Operationele Prioritaire Stoffen) model at RIVM. Over the years, scientific advancements and multiple divergent versions led to the need for a comprehensive restructuring. This repository presents a definitive, stand-alone, and open-source version of DepAC, with clear documentation and rationale for its scientific choices.

DepAC remains written in Fortran, but is now structured as a modern, modular package that can be easily integrated into other models and workflows.


## Purpose

DepAC calculates dry deposition velocities and fluxes for key atmospheric pollutants, using meteorological data, land use characteristics, and component-specific physical and chemical properties. The model supports multiple land types and chemical species, and includes routines for input validation, error handling, and scientific logging.


This repository provides the model implementation and documentation, including typical parameter values used by RIVM. A stand-alone runner for DepAC will be developed in the future; example usage is available in the `example` folder.


## Key Features

- Modular Fortran implementation for scientific reliability and extensibility
- Input validation and error handling for robust operation
- Logging and missing value detection
- Support for multiple land use types and meteorological scenarios
- Designed for integration with larger air quality and deposition frameworks


## Further Documentation

Refer to the following sections for installation instructions, usage examples, and scientific background.

- [RIVM Default Parameter Values](RIVM_defaults.md)
- [Example Usage](examples.md)
