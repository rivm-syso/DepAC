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
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    ! use i_gstom_parameterisation, only: gstom_parameterisation

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
        ! procedure(gstom_parameterisation), pointer, nopass :: gstom_param => null() ! gstom parameterisation function pointer for this land use type
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

        procedure(rinc_parameterisation), pointer, nopass :: rinc_param => null()
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

        ! required for NH3 compensation point calculation
        real :: gamma_stom_c_fac = -999.0
        real :: gamma_soil_c_fac = -999.0

        ! soil resistance if parameterisation requires it.
        real :: rsoil = -999.0

        procedure(gsoil_parameterisation), pointer, nopass :: gsoil_param => null() ! gsoil parameterisation function pointer for this land use type

        ! function paths

        type(depac_stomatal_params) :: stom_par
        type(depac_rc_r_params) :: rc_rinc
    end type depac_land_use

    abstract interface
        pure function rinc_parameterisation(rc_rinc, meteo, dp_conf) result(rinc)
            import :: depac_rc_r_params, depac_meteorology, depac_config

            type(depac_rc_r_params), intent(in) :: rc_rinc
            type(depac_meteorology), intent(in) :: meteo
            type(depac_config), intent(in) :: dp_conf

            real :: rinc
        end function rinc_parameterisation

        pure function gsoil_parameterisation(lu_conf, meteo, dp_conf) result(gsoil)
            import :: depac_land_use, depac_meteorology, depac_config

            type(depac_land_use), intent(in) :: lu_conf
            type(depac_meteorology), intent(in) :: meteo
            type(depac_config), intent(in) :: dp_conf

            real :: gsoil
        end function gsoil_parameterisation

    end interface 

end module t_depac_land_use