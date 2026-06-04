!------------------------------------------------------------------------------
! Module:     t_depac_land_use_core
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines core land use parameters type with name, index, compensation point
!   factors, and soil resistance. The depac_land_use_core type serves as the
!   base type for extended land use parameter definitions.
!------------------------------------------------------------------------------
module t_depac_land_use_core
    implicit none (type, external)
    private
    public :: depac_land_use_core

    !> Type representing land use parameters for DEPAC calculations.
    !! Contains the following fields:
    !! - name: Name of the land use type, e.g. "forest", "grass", etc.
    !! - index: Index of the land use type (default -999). (required)
    !! - gamma_stom_c_fac: Factor in linear relation between gamma_stom and NH3 air concentration.
    !! - gamma_soil_c_fac: Gamma_soil c factor for water land use (default -999.0).
    !! - rsoil: Soil resistance for this land use type (default -999.0).
    !! - stom_par: Stomatal conductance parameters.
    !! - rc_rinc: Rinc calculation parameters.
    !! Note: The default values (-999.0) indicate missing or undefined data.
    type :: depac_land_use_core
        character(len=40) :: name
        integer :: index = -999

        ! required for NH3 compensation point calculation
        real :: gamma_stom_c_fac = -999.0
        real :: gamma_soil_c_fac = -999.0

        ! soil resistance if parameterisation requires it.
        real :: rsoil = -999.0
    end type depac_land_use_core
end module t_depac_land_use_core
