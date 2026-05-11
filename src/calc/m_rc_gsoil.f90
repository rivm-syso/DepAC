!------------------------------------------------------------------------------
! Module:     m_rc_gsoil
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating soil conductance (rc_gsoil)
! for atmospheric deposition modeling.
!   The rc_gsoil subroutine computes effective soil resistance and conductance
!   based on meteorological, land use, and component parameters, including
!   handling for frozen and wet soil conditions. The results are used in dry
!   deposition calculations and model output.
!------------------------------------------------------------------------------

module m_rc_gsoil
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context
    implicit none (type, external)
    public
    contains

    subroutine rc_gsoil(setup, ctx)
        type(depac_setup), intent(in) :: setup
        type(depac_context), intent(inout) :: ctx

        ctx%output%gsoil_eff = setup%gsoil_param%apply(setup, ctx)
    end subroutine rc_gsoil

end module m_rc_gsoil