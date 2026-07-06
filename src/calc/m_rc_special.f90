!------------------------------------------------------------------------------
! Module:     m_rc_special
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    May 11 2026
! Description:
!   This module provides routines for calculating special resistance values
!   (rc_special) for atmospheric deposition modeling components. The main
!   subroutine, rc_special, handles special cases for snow surfaces and
!   specific components such as HNO3, NO, NO2, O3, SO2, and NH3, using
!   meteorological, land use, and component parameters. Results are used in
!   dry deposition calculations and model output.
!------------------------------------------------------------------------------
module m_rc_special
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context

    implicit none (type, external)
    private
    public :: rc_special
    contains
    subroutine rc_special(setup, ctx, ready)
        type(depac_setup), intent(in) :: setup
        type(depac_context), intent(inout) :: ctx
        logical, intent(inout) :: ready

        call setup%rc_special_param%apply(setup, ctx, ready)
    end subroutine rc_special

end module m_rc_special
