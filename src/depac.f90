
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
  ! ! Types and subroutines for the DepAC model
  ! use t_depac_land_use, only: depac_land_use
  ! use t_depac_component, only: depac_component
  ! use t_depac_meteorology, only: depac_meteorology
  ! use t_depac_config, only: depac_config
  ! use t_depac_location, only: depac_location
  ! use t_depac_output, only: depac_output

  use t_depac_setup, only: depac_setup
  use t_depac_context, only: depac_context

  use m_depac_calc, only: depac_calc, depac_calc_partial, depac_calc_finish
  
  implicit none (type, external)


  private
  public :: depac_setup, depac_context, depac_calc, depac_calc_partial, depac_calc_finish



end module depac