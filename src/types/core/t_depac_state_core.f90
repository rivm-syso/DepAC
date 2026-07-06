!------------------------------------------------------------------------------
! Module:     t_depac_state_core
! Author:     Marte Voorneveld, RIVM
! Created:    November 14, 2025
! Modified:   May 12, 2026
! Description:
!   Defines state constants, diffusion coefficients, and compensation
!   point type for DEPAC model. The depac_state_core and depac_compensation_point
!   types provide model state and component-specific parameters.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
module t_depac_state_core
    implicit none (type, external)
    public

    !> Type representing compensation point inputs for DEPAC calculations.
    !! Contains the following fields:
    !! - c_ave_nh3: Average compensation point for NH3 over a period (ug/m3, default -999.0).
    !! - c_ave_so2: Average compensation point for SO2 over a period (ug/m3, default -999.0).
    !! - c_so2: SO2 concentration (ug/m3, default -999.0).
    !! - c_nh3: NH3 concentration (ug/m3, default -999.0).
    !! - iratns: Index for NH3/SO2 ratio (1=low, 2=high, 3=very low, default -999).
    !! Note: The default values (-999.0 or -999) indicate missing or undefined data.
    type :: depac_compensation_point
        real :: c_ave_nh3 = -999.0
        real :: c_ave_so2 = -999.0
        real :: c_so2 = -999.0
        real :: c_nh3 = -999.0
        integer :: iratns = -999
    end type depac_compensation_point

    !> Type representing configuration options for DEPAC calculations.
    !! Contains the following fields:
    !! - check_input: Check input values for validity (default .true.).
    !! - dwat: Diffusion coefficient of water vapour (m2/s, default 0.21e-4).
    !! - dO3: Diffusion coefficient of ozone (m2/s, default 0.13e-4).
    !! - rssnow: Constant snow resistance when ipar_snow = 1 (default 2000.0).
    !! - sai_grass_haarweg: Surface area index at experimental site Haarweg (default 3.5).
    !! - calc_comp_points: Calculate compensation points (default .false.).
    !! - calc_effective_rc: Calculate effective rc (default .false.).
    !! - log_level: Logging threshold (default LOG_LEVEL_WARN).
    !! - lai: Leaf Area Index
    !! - sai: Surface Area Index
    !! - ra_obs: Aerodynamic resistance at observation height (s/m, default -999.0).
    !! - rb: Boundary layer resistance (s/m, default -999.0).
    !! - has_leaves: Indicates if leaves are present (auto-set).
    !! - has_vegetation: Indicates if vegetation is present (auto-set).
    !! - comp_point: Compensation point configuration. (see depac_compensation_point type).
    !! - coord: Location data for DEPAC run. (see depac_location type).
    !! Note: The default values (-999.0 or -999) indicate missing or undefined data.
    type :: depac_state_core
        real :: lai = -999.0
        real :: sai = -999.0
        real :: ra_obs = -999.0
        real :: rb = -999.0
        type(depac_compensation_point) :: comp_point
    end type depac_state_core

end module t_depac_state_core
