
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

  use t_depac_setup, only: depac_setup
  use t_depac_context, only: depac_context

  use m_depac_calc, only: depac_calc, depac_calc_partial, depac_calc_finish

  use m_ra, only: depac_calc_ra,  depac_calc_ra_obs_h
  use m_rb, only: depac_calc_rb_hicks

  ! all depac parameterizations are available via this module
  use m_depac_params, only: comp_point_ammonia, comp_point_default, csoil_default, csoil_water, &
       gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance, &
       gstom_default, gstom_emberson, &
       gw_default, gw_so2, gw_nh3_sutton, &
       rc_tot_fixed, rc_tot_nitric_acid, rc_tot_nitric_oxide, rc_special_default
  ! the parameterization types are available via this module
  use c_depac_param_types, only: depac_comp_point_param, depac_csoil_param, depac_gsoil_param, &
                             depac_gstom_param, depac_gw_param, depac_rc_special_param, &
                             depac_rinc_param


  ! error
  use t_depac_error_core, only: depac_error_core, ERR_NONE, ERR_INPUT, &
    ERR_COMPUTATION, ERR_MEMORY, ERR_UNKNOWN

  use m_version, only: VERSION, BUILD_DATE
  implicit none (type, external)


  private
  public :: &
    ! types
    depac_setup, depac_context, &

    ! main DepAC calculation routines
    depac_calc, depac_calc_partial, depac_calc_finish, &
    ! Ra and Rb calculations
    depac_calc_ra, depac_calc_ra_obs_h, depac_calc_rb_hicks, &

    ! parameterizations
    comp_point_ammonia, comp_point_default, csoil_default, csoil_water, &
    gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance, &
    gstom_default, gstom_emberson, &
    gw_default, gw_so2, gw_nh3_sutton, &
    rc_tot_fixed, rc_tot_nitric_acid, rc_tot_nitric_oxide, rc_special_default, &

    ! parameterization types
    depac_comp_point_param, depac_csoil_param, depac_gsoil_param, &
    depac_gstom_param, depac_gw_param, depac_rc_special_param, &
    depac_rinc_param, &

    ! error handling
    depac_error_core, ERR_NONE, ERR_INPUT, ERR_COMPUTATION, ERR_MEMORY, ERR_UNKNOWN, &

    ! version info
    VERSION, BUILD_DATE



end module depac