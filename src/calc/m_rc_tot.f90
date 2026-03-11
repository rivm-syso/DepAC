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

    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error
    implicit none (type, external)
    public
contains

    subroutine rc_tot(dp_out, err)
        type(depac_output), intent(inout) :: dp_out  ! output of this run
        type(depac_error), intent(inout) :: err      ! error handling
        ! Total conductance:
        dp_out%gc_tot = dp_out%gstom + dp_out%gsoil_eff + dp_out%gw
        ! Total resistance (note: gw can be negative, but no total emission allowed here):
        if (dp_out%gc_tot <= 0.0 .or. dp_out%gw < 0.0) then
            dp_out%rc_tot = -9999.0
        else
            dp_out%rc_tot = 1.0 / dp_out%gc_tot
        endif
    end subroutine rc_tot
end module m_rc_tot