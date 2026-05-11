# DepAC Parameterizations Module

DepAC contains parameterization implementations in the `calc/params` folder. Parameterizations define how key physical and chemical processes are calculated during a DepAC run.

## Overview

A `depac_setup` brings together a component (e.g., NH3, O3), a land use type (e.g., grass, forest), and a set of parameterizations that control how deposition processes are computed. Each parameterization is allocated as a polymorphic class and can be swapped to use different scientific approaches or parameter sets.

### Parameterization Types

The following parameterizations must be allocated in a `depac_setup` for a complete calculation:

| Parameterization | Class | Purpose |
|---|---|---|
| **gw_param** | `depac_gw_param` | External leaf (wet surface) conductance calculations |
| **gstom_param** | `depac_gstom_param` | Stomatal conductance calculations |
| **comp_point_param** | `depac_comp_point_param` | Compensation point calculations (e.g., for NH3) |
| **gsoil_param** | `depac_gsoil_param` | Soil conductance calculations |
| **csoil_param** | `depac_csoil_param` | Soil compensation point calculations |
| **rinc_param** | `depac_rinc_param` | In-canopy resistance calculations |
| **rc_special_param** | `depac_rc_special_param` | Special resistance cases (e.g., snow, specific components) |

## Creating a DepAC Setup

The recommended way to create a `depac_setup` with parameterizations is using the `depac_factory` module:

```fortran
use depac_factory, only: make_setup

! Create a setup with default parameterizations
setup = make_setup(component, land_use)

! Create a setup with custom parameterizations
setup = make_setup(component, land_use, &
   gw_param = custom_gw_param(), &
   gstom_param = custom_gstom_param(), &
   comp_point_param = custom_comp_point_param())
```

The factory handles allocation and deallocation automatically. If a parameterization is not provided, defaults are used.

## Default Configurations

The `default_depac_config_rivm` module provides pre-configured setups matching RIVM standards for all component-landuse combinations. These defaults can be overridden for specific combinations when needed (e.g., NH3 uses special gw and stomatal parameterizations).

## Module Organization

Each parameterization type typically has:
- **Abstract base type** (e.g., `depac_gw_param`) with an abstract interface
- **Concrete implementations** (e.g., `gw_default`, `gw_nh3_sutton`) that provide specific algorithms or parameter sets

Implementations inherit from the abstract base and implement the required interface, allowing flexible substitution of different parameterization schemes.

## Scientific Context

Parameterizations in DepAC implement scientific models for:
- **Conductance calculations**: Based on plant physiology, meteorology, and component-specific properties
- **Compensation points**: NH3-specific; represents equilibrium concentration above soil/stomata
- **Resistance pathways**: Accounts for stomatal, external leaf, and soil deposition pathways

