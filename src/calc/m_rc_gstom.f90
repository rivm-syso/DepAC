!------------------------------------------------------------------------------
! Module:     m_rc_gstom
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 19 2025
! Description:
!   This module provides routines for calculating stomatal conductance and
!   related parameters for the DepAC atmospheric deposition model.
!------------------------------------------------------------------------------
module m_rc_gstom
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context
    implicit none (type, external)
    public
contains

    !------------------------------------------------------------------------------
    ! Subroutine: rc_gstom
    ! Purpose   : Compute stomatal conductance for a given component
    ! Notes     : Implements component-specific logic and vegetation checks.
    !------------------------------------------------------------------------------
    subroutine rc_gstom(setup, ctx)
        type(depac_setup), intent(in) :: setup
        type(depac_context), intent(inout) :: ctx

        ctx%output%gstom = setup%gstom_param%apply(setup, ctx)

    end subroutine rc_gstom

end module m_rc_gstom
