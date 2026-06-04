!------------------------------------------------------------------------------
! Module:     t_depac_config_core
! Author:     Marte Voorneveld, RIVM
! Created:    November 14, 2025
! Modified:   May 12, 2026
! Description:
!   Defines configuration constants, diffusion coefficients, and compensation
!   point type for DEPAC model. The depac_config_core and depac_compensation_point
!   types provide model configuration and component-specific parameters.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
module t_depac_config_core
    use m_logger, only: LOG_LEVEL_WARN
    implicit none (type, external)
    public

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
    end type depac_config_core

end module t_depac_config_core
