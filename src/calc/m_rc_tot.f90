!------------------------------------------------------------------------------
! Module:     m_rc_tot
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating total resistance (rc_tot)
!   and total conductance (gc_tot) for atmospheric deposition modeling.
!   The rc_tot subroutine computes these values based on stomatal,
!   soil, and external leaf conductances, and handles error cases for
!   negative or zero conductance. Results are used in dry deposition
!   calculations and model output.
!------------------------------------------------------------------------------
module m_rc_tot

    use t_depac_context, only: depac_context
    implicit none (type, external)
    public
contains
    subroutine rc_tot(ctx)
        type(depac_context), intent(inout) :: ctx
        ! Total conductance:
        ctx%output%gc_tot = ctx%output%gstom + ctx%output%gsoil_eff + ctx%output%gw
        ! Total resistance (note: gw can be negative, but no total emission allowed here):
        if (ctx%output%gc_tot <= 0.0 .or. ctx%output%gw < 0.0) then
            ctx%output%rc_tot = -9999.0
        else
            ctx%output%rc_tot = 1.0 / ctx%output%gc_tot
        endif
    end subroutine rc_tot
end module m_rc_tot