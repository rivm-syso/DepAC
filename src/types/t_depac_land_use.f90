!------------------------------------------------------------------------------
! Module:     t_depac_land_use
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-13
! Updated:    2026-02-27
! Description:
!   This module defines derived types for land use parameters used in
!   atmospheric deposition modeling. It includes types for stomatal parameters,
!   resistance parameters, and the main land use type containing all relevant
!   properties.
!------------------------------------------------------------------------------

module t_depac_land_use
    implicit none (type, external)
    public
    !> Type representing stomatal conductance parameters.
    !! Contains the following fields:
    !! - F_min: Minimum stomatal conductance (typical values 0.01, default -999.0).
    !! - alpha: Alpha for F_light calculation (default -999.0).
    !! - Topt: Optimal temperature for F_temp calculation (default -999.0).
    !! - Tmin: Minimum temperature for F_temp calculation (default -999.0).
    !! - Tmax: Maximum temperature for F_temp calculation (default -999.0).
    !! - g_max: Maximum stomatal conductance (default -999.0).
    !! - vpd_max: Upper VPD limit for stomatal conductance reduction (default -999.0).
    !! - vpd_min: Lower VPD limit for stomatal conductance reduction (default -999.0).
    !! Note: The default values (-999.0) indicate missing or undefined data.
    type :: depac_stomatal_params
        real :: F_min = -999.0
        real :: alpha = -999.0
        real :: Topt = -999.0
        real :: Tmin = -999.0
        real :: Tmax = -999.0
        real :: g_max = -999.0
        real :: vpd_max = -999.0
        real :: vpd_min = -999.0
    end type depac_stomatal_params

    !> Type representing parameters for rinc calculation.
    !! Contains the following fields:
    !! - b: Rinc parameter b (default -999).
    !! - h: Rinc parameter h (default -999).
    !! Note: The default values (-999) indicate missing or undefined data.
    type :: depac_rc_r_params
        integer :: b = -999
        integer :: h = -999
    end type depac_rc_r_params

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
    type :: depac_land_use
        character(len=40) :: name
        integer :: index = -999
        real :: gamma_stom_c_fac = -999.0
        real :: gamma_soil_c_fac = -999.0
        real :: rsoil = -999.0
        type(depac_stomatal_params) :: stom_par
        type(depac_rc_r_params) :: rc_rinc
    end type depac_land_use

    contains
end module t_depac_land_use