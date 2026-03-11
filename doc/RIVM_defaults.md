# RIVM defaults

At RIVM the following inputs are used for each of the input types

## Indices for Components and Land Use Types

The DepAC model uses integer indices to refer to component and land use types. These indices are defined in the Fortran module and can be used in input files and model configuration.

### Component Indices

| Index | Name   | Description            |
|-------|--------|------------------------|
| 1     | NH3    | Ammonia                |
| 2     | O3     | Ozone                  |
| 3     | SO2    | Sulfur dioxide         |
| 4     | NO2    | Nitrogen dioxide       |
| 5     | NO     | Nitric oxide           |
| 6     | HNO3   | Nitric acid            |

These correspond to the Fortran parameters: `COMP_NH3`, `COMP_O3`, `COMP_SO2`, `COMP_NO2`, `COMP_NO`, `COMP_HNO3`.

### Land Use Indices

| Index | Name               |
|-------|--------------------|
| 1     | grass              |
| 2     | arable             |
| 3     | permanent_crops    |
| 4     | coniferous_forest  |
| 5     | deciduous_forest   |
| 6     | water              |
| 7     | urban              |
| 8     | other              |
| 9     | desert             |

These correspond to the Fortran parameters: `LU_GRASS`, `LU_ARABLE`, `LU_PERMANENT_CROPS`, `LU_CONIFEROUS_FOREST`, `LU_DECIDUOUS_FOREST`, `LU_WATER`, `LU_URBAN`, `LU_OTHER`, `LU_DESERT`.

*Note: Indices start at 1 and are used throughout the model to refer to these types.*

---

## Meteorology

Meteorology is usually provided by KNMI measurements for individual runs. The `meteorology` type requires the following inputs to run.

| Parameter   | Description                                 | Typical Value | Units         |
|-------------|---------------------------------------------|--------------|---------------|
| t           | Air temperature                             |              | °C            |
| tsurf       | Surface temperature                         | Same as t    | °C            |
| rh          | Relative humidity                           | 0-100        | %             |
| glrad       | Global radiation                            |              | J/cm²/hr      |
| pres_0      | Surface level pressure                      | 101500       | Pa            |
| nwet        | Wetness indicator (0=dry, 1=wet)            | 0/1          |               |
| sinphi      | Sine of solar elevation angle               | -1 - 1       | (dimensionless)|

*Note: `ws10`, `ust`, and `ol` are optional and only required if aerodynamic (`ra`) and quasi-laminar boundary layer (`rb`) resistances are calculated using the submodules. For standard model runs, only the parameters above are required.*

*Note: These are typical values used at RIVM for model input. Actual values may vary depending on location, season, and measurement.*

## Component

The following required parameters are inputs for the `component` input in the DepAC model.

| Parameter      | Description                                                                 | Units           |
|----------------|-----------------------------------------------------------------------------|-----------------|
| name           | Name of the component (e.g., "NH3", "O3")                                   | (string)        |
| comp_index     | Integer index for the component (see table above, e.g., `COMP_NH3`)         | (integer/enum)  |
| diffc          | Diffusion coefficient for stomatal conductivity                             | dimensionless   |
| rw_val         | Constant rw value (optional)                                                | s/m             |
| ipar_snow      | Snow resistance parametrisation (1=constant, 2=temperature dependent)        | (integer)       |
| rsoil_frozen   | Resistance for frozen soil                                                  | s/m             |
| rsoil_wet      | Resistance for wet soil                                                     | s/m             |

### NH3 values
| Parameter      | Description                                                                 | Typical Value | Units           |
|----------------|-----------------------------------------------------------------------------|--------------|-----------------|
| name           | Name of the component                                                       | "NH3"        | (string)        |
| comp_index     | Index of the component                                                      | COMP_NH3     | (integer/enum)  |
| diffc          | Diffusion coefficient for stomatal conductivity                             | 0.21e-4      | dimensionless   |
| rw_val         | Constant rw value (optional)                                                | -999.0       | s/m             |
| ipar_snow      | Snow resistance parametrisation (1=constant, 2=temperature dependent)        | 2            | (integer)       |
| rsoil_frozen   | Resistance for frozen soil                                                  | 1000.0       | s/m             |
| rsoil_wet      | Resistance for wet soil                                                     | 10.0         | s/m             |

### O3 values
| Parameter      | Description                                                                 | Typical Value | Units           |
|----------------|-----------------------------------------------------------------------------|--------------|-----------------|
| name           | Name of the component                                                       | "O3"         | (string)        |
| comp_index     | Index of the component                                                      | COMP_O3      | (integer/enum)  |
| diffc          | Diffusion coefficient for stomatal conductivity                             | 0.13e-4      | dimensionless   |
| rw_val         | Constant rw value (optional)                                                | 1000.0       | s/m             |
| ipar_snow      | Snow resistance parametrisation (1=constant, 2=temperature dependent)        | 1            | (integer)       |
| rsoil_frozen   | Resistance for frozen soil                                                  | 2000.0       | s/m             |
| rsoil_wet      | Resistance for wet soil                                                     | 2000.0       | s/m             |

### SO2 values
| Parameter      | Description                                                                 | Typical Value | Units           |
|----------------|-----------------------------------------------------------------------------|--------------|-----------------|
| name           | Name of the component                                                       | "SO2"        | (string)        |
| comp_index     | Index of the component                                                      | COMP_SO2     | (integer/enum)  |
| diffc          | Diffusion coefficient for stomatal conductivity                             | 0.11e-4      | dimensionless   |
| rw_val         | Constant rw value (optional)                                                | -999.0       | s/m             |
| ipar_snow      | Snow resistance parametrisation (1=constant, 2=temperature dependent)        | 2            | (integer)       |
| rsoil_frozen   | Resistance for frozen soil                                                  | 500.0        | s/m             |
| rsoil_wet      | Resistance for wet soil                                                     | 10.0         | s/m             |

### NO2 values
| Parameter      | Description                                                                 | Typical Value | Units           |
|----------------|-----------------------------------------------------------------------------|--------------|-----------------|
| name           | Name of the component                                                       | "NO2"        | (string)        |
| comp_index     | Index of the component                                                      | COMP_NO2     | (integer/enum)  |
| diffc          | Diffusion coefficient for stomatal conductivity                             | 0.13e-4      | dimensionless   |
| rw_val         | Constant rw value (optional)                                                | 2000.0       | s/m             |
| ipar_snow      | Snow resistance parametrisation (1=constant, 2=temperature dependent)        | 1            | (integer)       |
| rsoil_frozen   | Resistance for frozen soil                                                  | 2000.0       | s/m             |
| rsoil_wet      | Resistance for wet soil                                                     | 2000.0       | s/m             |

### NO values
| Parameter      | Description                                                                 | Typical Value | Units           |
|----------------|-----------------------------------------------------------------------------|--------------|-----------------|
| name           | Name of the component                                                       | "NO"         | (string)        |
| comp_index     | Index of the component                                                      | COMP_NO      | (integer/enum)  |
| diffc          | Diffusion coefficient for stomatal conductivity                             | 0.16e-4      | dimensionless   |
| rw_val         | Constant rw value (optional)                                                | -999.0       | s/m             |
| ipar_snow      | Snow resistance parametrisation (1=constant, 2=temperature dependent)        | 1            | (integer)       |
| rsoil_frozen   | Resistance for frozen soil                                                  | -999.0       | s/m             |
| rsoil_wet      | Resistance for wet soil                                                     | -999.0       | s/m             |

### HNO3 values
| Parameter      | Description                                                                 | Typical Value | Units           |
|----------------|-----------------------------------------------------------------------------|--------------|-----------------|
| name           | Name of the component                                                       | "HNO3"       | (string)        |
| comp_index     | Index of the component                                                      | COMP_HNO3    | (integer/enum)  |
| diffc          | Diffusion coefficient for stomatal conductivity                             | -999.0       | dimensionless   |
| rw_val         | Constant rw value (optional)                                                | -999.0       | s/m             |
| ipar_snow      | Snow resistance parametrisation (1=constant, 2=temperature dependent)        | -999         | (integer)       |
| rsoil_frozen   | Resistance for frozen soil                                                  | -999.0       | s/m             |
| rsoil_wet      | Resistance for wet soil                                                     | -999.0       | s/m             |

## Land class/land use options

Land use types are specified using the `land_use` input in the DepAC model. Each land use type defines parameters that influence deposition and resistance calculations.

| Parameter           | Description                                                                                   | Typical Value      | Units         |
|---------------------|-----------------------------------------------------------------------------------------------|--------------------|---------------|
| name                | Name of the land use type (e.g., "forest", "grass", "water")                                 | (string)           | -             |
| index               | Integer index for the land use type (see table above)                                        | (integer)          | -             |
| gamma_stom_c_fac    | Stomatal compensation point factor                                                            | -999.0 (default)   | (dimensionless)|
| gamma_soil_c_fac    | Gamma_soil c factor (only for water land use), otherwise -999.0                              | -999.0 (default)   | (dimensionless)|
| rsoil               | Soil resistance for this land use type and Component(-999.0 if undefined)                     | -999.0 (default)   | s/m           |
| stom_par            | Stomatal parameters (see stomatal_params type for details)                                   | -                  | -             |
| rc_rinc             | In-canopy resistance parameters (see rc_r_params type for details)                           | -                  | -             |

*Note: The `name` and `index` parameters identify the land use type. Some parameters may be undefined for certain types and use the default value of -999.0. The `stom_par` and `rc_rinc` fields are complex types containing additional parameters relevant to stomatal and resistance calculations.*

### Land use type values

| Index | Name               | gamma_stom_c_fac | gamma_soil_c_fac | Description/Notes         |
|-------|--------------------|------------------|------------------|---------------------------|
| LU_GRASS              | grass              | 362.0            | -999.0           | Typical managed grassland |
| LU_ARABLE             | arable             | 362.0            | -999.0           | Cropland/arable fields    |
| LU_PERMANENT_CROPS    | permanent_crops    | 362.0            | -999.0           | Orchards/vineyards        |
| LU_CONIFEROUS_FOREST  | coniferous_forest  | 362.0            | -999.0           | Pine/spruce forest        |
| LU_DECIDUOUS_FOREST   | deciduous_forest   | 362.0            | -999.0           | Oak/beech forest          |
| LU_WATER              | water              | -999.0           | 430.0            | Lakes/rivers              |
| LU_URBAN              | urban              | -999.0           | -999.0           | Built-up areas            |
| LU_OTHER              | other              | 362.0            | -999.0           | Miscellaneous vegetation  |
| LU_DESERT             | desert             | -999.0           | -999.0           | Bare soil/sand            |

*Note: Only the `gamma_stom_c_fac` and `gamma_soil_c_fac` parameters are shown here. Other parameters such as roughness length, canopy height, and stomatal/LAI parameters are omitted for brevity. See model configuration for full details.*

### In-canopy resistance parameters (`rc_rinc`)
| Index | Land Use Type        | b      | h      | Notes                       |
|-------|---------------------|--------|--------|-----------------------------|
| LU_GRASS              | grass               | -999.0 | -999.0 | Typical managed grassland   |
| LU_ARABLE             | arable              | 14.0   | 1.0    | Cropland/arable fields      |
| LU_PERMANENT_CROPS    | permanent_crops     | 14.0   | 1.0    | Orchards/vineyards          |
| LU_CONIFEROUS_FOREST  | coniferous_forest   | 14.0   | 20.0   | Pine/spruce forest          |
| LU_DECIDUOUS_FOREST   | deciduous_forest    | 14.0   | 20.0   | Oak/beech forest            |
| LU_WATER              | water               | -999.0 | -999.0 | Lakes/rivers                |
| LU_URBAN              | urban               | -999.0 | -999.0 | Built-up areas              |
| LU_OTHER              | other               | -999.0 | -999.0 | Miscellaneous vegetation    |
| LU_DESERT             | desert              | -999.0 | -999.0 | Bare soil/sand              |

*Note: `b` and `h` are parameters for in-canopy resistance. A value of -999.0 indicates undefined or not applicable for the land use type.*

### Stomatal conductance parameters (`stomatal_params`)

These parameters define the behavior of stomatal conductance for different land use types in the DepAC model.
| Land Use Index   | Land Class           | F_min   | alpha    | Topt  | Tmin  | Tmax  | g_max    | vpd_max | vpd_min |
|------------------|---------------------|---------|----------|-------|-------|-------|----------|---------|---------|
| LU_GRASS         | grass               | 0.01    | 0.04113  | 26.0  | 12.0  | 40.0  | 0.00659  | 1.3     | 3.0     |
| LU_ARABLE        | arable              | 0.01    | 0.04113  | 26.0  | 12.0  | 40.0  | 0.00732  | 0.9     | 2.8     |
| LU_PERMANENT_CROPS | permanent_crops   | 0.01    | 0.04113  | 26.0  | 12.0  | 40.0  | 0.00732  | 0.9     | 2.8     |
| LU_CONIFEROUS_FOREST | coniferous_forest | 0.1   | 0.02742  | 18.0  | 0.0   | 36.0  | 0.00341  | 0.5     | 3.0     |
| LU_DECIDUOUS_FOREST | deciduous_forest | 0.1     | 0.02742  | 20.0  | 0.0   | 35.0  | 0.00366  | 1.0     | 3.25    |
| LU_WATER         | water               | -999.0  | -999.0   | -999.0| -999.0| -999.0| -999.0   | -999.0  | -999.0  |
| LU_URBAN         | urban               | -999.0  | -999.0   | -999.0| -999.0| -999.0| -999.0   | -999.0  | -999.0  |
| LU_OTHER         | other               | 0.01    | 0.04113  | 26.0  | 12.0  | 40.0  | 0.00659  | 1.3     | 3.0     |
| LU_DESERT        | desert              | -999.0  | -999.0   | -999.0| -999.0| -999.0| -999.0   | -999.0  | -999.0  |

*Note: -999.0 indicates undefined or not applicable for the land class.*

### Rsoil values by Component and Land Use
| Land Use Index      | Land Use           | NH3   | O3    | SO2   | NO2   | NO     | HNO3  |
|---------------------|--------------------|-------|-------|-------|-------|--------|-------|
| LU_GRASS            | grass              | 100.0 | 1000.0| 1000.0| 1000.0| -999.0 | -999.0|
| LU_ARABLE           | arable             | 100.0 | 200.0 | 1000.0| 1000.0| -999.0 | -999.0|
| LU_PERMANENT_CROPS  | permanent_crops    | 100.0 | 200.0 | 1000.0| 1000.0| -999.0 | -999.0|
| LU_CONIFEROUS_FOREST| coniferous_forest  | 100.0 | 200.0 | 1000.0| 1000.0| -999.0 | -999.0|
| LU_DECIDUOUS_FOREST | deciduous_forest   | 100.0 | 200.0 | 1000.0| 1000.0| -999.0 | -999.0|
| LU_WATER            | water              | 10.0  | 2000.0| 10.0  | 2000.0| 2000.0 | -999.0|
| LU_URBAN            | urban              | 100.0 | 400.0 | 1000.0| 1000.0| 1000.0 | -999.0|
| LU_OTHER            | other              | 100.0 | 400.0 | 1000.0| 1000.0| -999.0 | -999.0|
| LU_DESERT           | desert             | 100.0 | 2000.0| 1000.0| 1000.0| 2000.0 | -999.0|

*Note: -999.0 indicates undefined or not applicable for the component/land use combination.*