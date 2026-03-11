
!###############################################################################
!   National Institute of Public Health and Environment (RIVM) (2025)
!   The Netherlands
!
!  MODULE             : depac
!  AUTHORS            : Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
!  FIRM/INSTITUTE     : RIVM
!  DESCRIPTION        : Main driver for the DepAC atmospheric deposition model.
!                       Calculates canopy/surface resistance Rc, compensation points,
!                       and effective resistance for gaseous dry deposition.
!                       Implements classical and compensation point approaches.
!
!  SCIENTIFIC CONTEXT:
!    - Parameterizes Rc and related resistances for dry deposition.
!    - Supports classical, compensation point, and effective resistance schemes.
!    - Handles multiple gaseous components: HNO3, NO, NO2, O3, SO2, NH3.
!    - Missing deposition paths are represented by -999 and checked via the helper:
!       'missing'.
!
!    - Ra, Rb, fluxes, and new concentrations are computed outside DepAC but
!       routines are available in the DepAC package if needed.
!
!  For scientific details, we refer to the documentation.
!###############################################################################


module depac
  ! Types and subroutines for the DepAC model
  use t_depac_land_use, only: depac_land_use
  use t_depac_component, only: depac_component
  use t_depac_meteorology, only: depac_meteorology
  use t_depac_config, only: depac_config
  use t_depac_location, only: depac_location
  use t_depac_output, only: depac_output

  ! Helper modules
  use m_depac_error, only: clear_error, has_error, set_error
  use t_depac_error, only: ERR_INPUT, depac_error
  use m_version, only: VERSION, BUILD_DATE
  use m_logger, only: set_log_level
  use m_check_depac_input, only: &
    check_component_input, &
    check_land_use_input, &
    check_depac_config, &
    check_meteorology_input

  use default_depac_config_rivm, only: default_landuse_types, default_component_types, &
    default_rsoil_matrix, obtain_config
  use default_indices, only: COMP_NH3, COMP_O3, COMP_SO2, COMP_NO2, COMP_NO, COMP_HNO3, &
    LU_GRASS, LU_ARABLE, LU_PERMANENT_CROPS, LU_CONIFEROUS_FOREST, LU_DECIDUOUS_FOREST, &
    LU_WATER, LU_URBAN, LU_OTHER, LU_DESERT

  ! Calculation modules
  use m_rc_special, only: rc_special
  use m_rc_gw, only: rc_gw
  use m_rc_gstom, only: rc_gstom
  use m_rc_gsoil, only: rc_gsoil
  use m_rc_tot, only: rc_tot
  use m_comp_points, only: rc_comp_point
  use m_rc_eff, only: rc_eff
  use m_depac_calc, only: depac_calc_partial, depac_calc_finish, depac_calc
  implicit none (type, external)


  private

  ! exposed module entities including types
  public :: depac_calc, depac_land_use, depac_component, &
            depac_meteorology, depac_config, depac_output, depac_error, &
            has_error, VERSION, BUILD_DATE, depac_calc_partial, depac_calc_finish, &
            default_landuse_types, default_component_types, &
            default_rsoil_matrix, depac_location, &
            COMP_NH3, COMP_O3, COMP_SO2, COMP_NO2, COMP_NO, COMP_HNO3, &
            obtain_config,clear_error, &
            LU_GRASS, LU_ARABLE, LU_PERMANENT_CROPS, LU_CONIFEROUS_FOREST, LU_DECIDUOUS_FOREST, &
            LU_WATER, LU_URBAN, LU_OTHER, LU_DESERT




end module depac