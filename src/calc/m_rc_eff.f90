!------------------------------------------------------------------------------
! Module:     m_rc_eff
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating the effective resistance
!   (rc_eff) in atmospheric deposition modeling. The rc_eff subroutine
!   computes the effective resistance based on compensation points. The result
!   is used in dry deposition flux calculations and model output.
!------------------------------------------------------------------------------
module m_rc_eff

    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error
    use t_depac_config, only: depac_config
    implicit none (type, external)
    public
contains



    ! Calculates the effective canopy resistance (rc_eff) based on the total compensation point
    ! and the compensation point of the component. The formula used is:
    ! rc_eff = ((ra_obs + rb) * ccomp_tot + rc_tot * comp_point) / (comp_point - ccomp_tot)
    ! where ra_obs is the aerodynamic resistance at observation height
    ! rb is the boundary layer resistance
    ! rc_tot is the total canopy resistance
    ! comp_point is the compensation point of the component
    ! ccomp_tot is the total compensation point
    subroutine rc_eff(dp_out, dp_conf, err)
        type(depac_output), intent(inout) :: dp_out  ! output of this run
        type(depac_config), intent(in) :: dp_conf    ! configuration data
        type(depac_error), intent(inout) :: err      ! error handling

        if (dp_conf%comp_point%c_nh3 /= dp_out%ccomp_tot) then
            dp_out%rc_eff = ((dp_conf%ra_obs + dp_conf%rb) * dp_out%ccomp_tot + &
                dp_out%rc_tot * dp_conf%comp_point%c_nh3) / &
                (dp_conf%comp_point%c_nh3 - dp_out%ccomp_tot)
        else
            dp_out%rc_eff = -9999.0 ! no flux, resistance undefined
        end if
    end subroutine rc_eff
end module m_rc_eff