!------------------------------------------------------------------------------
! Module:     t_depac_config
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-14
! Updated:    2026-02-27
! Description:
!   This module defines configuration variables and constants for the DepAC
!   atmospheric deposition model. It includes diffusion coefficients,
!   resistance constants, and logical options for calculation modes and
!   model behavior.
!------------------------------------------------------------------------------
module t_depac_config_core
    use m_logger, only: LOG_LEVEL_WARN
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
    type :: depac_config_core
        logical :: check_input = .true.
        real :: dwat = 0.21e-4
        real :: dO3  = 0.13e-4
        real :: rssnow = 2000.0
        real :: sai_grass_haarweg = 3.5
        logical :: calc_comp_points = .false.
        logical :: calc_effective_rc = .false.
        integer :: log_level = LOG_LEVEL_WARN
        real :: lai = -999.0
        real :: sai = -999.0
        real :: ra_obs = -999.0
        real :: rb = -999.0
        logical :: has_leaves
        logical :: has_vegetation
        type(depac_compensation_point) :: comp_point
    end type depac_config_core

end module t_depac_config_core