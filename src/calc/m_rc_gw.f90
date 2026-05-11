!------------------------------------------------------------------------------
! Module:     m_rc_gw
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating external leaf resistance
!   (gw) for atmospheric deposition modeling components. The main subroutine,
!   rc_gw, computes conductance for NO, NO2, O3, SO2, and NH3, using
!   meteorological, component, and configuration parameters. Includes
!   parameterizations for SO2 and NH3 based on literature and RIVM defaults.
!------------------------------------------------------------------------------
module m_rc_gw
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context


   implicit none (type, external)
   public
contains

   subroutine rc_gw(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx


      ctx%output%gw = setup%gw_param%apply(setup, ctx)
   end subroutine rc_gw
end module m_rc_gw
