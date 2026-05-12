!------------------------------------------------------------------------------
! Module:     m_rc_eff
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    May 11 2026
! Description:
!   This module provides routines for calculating the effective resistance
!   (rc_eff) in atmospheric deposition modeling. The rc_eff subroutine
!   computes the effective resistance based on compensation points. The result
!   is used in dry deposition flux calculations and model output.
!------------------------------------------------------------------------------
module m_rc_eff

   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   implicit none (type, external)
   public
contains

   !----------------------------------------------------------------------------
   ! Subroutine: rc_eff
   ! Created:    November 14 2025
   ! Updated:    May 11 2026
   ! Description:
   !   Calculates the effective canopy resistance (rc_eff) based on the total compensation point
   !   and the compensation point of the component. The formula used is:
   !   rc_eff = ((ra_obs + rb) * ccomp_tot + rc_tot * comp_point) / (comp_point - ccomp_tot)
   !   where ra_obs is the aerodynamic resistance at observation height, rb is
   ! the boundary layer resistance,
   !   rc_tot is the total canopy resistance, comp_point is the compensation point of the
   !   component,
   !   and ccomp_tot is the total compensation point.
   !   If the compensation points are equal, rc_eff is set to -9999.0 to indicate no flux
   !   and undefined resistance.
   !----------------------------------------------------------------------------
   subroutine rc_eff(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      ! Use associate to simplify access to setup and context variables
      associate (cfg => setup%config, out => ctx%output)
         if (cfg%comp_point%c_nh3 /= out%ccomp_tot) then
            out%rc_eff = ((cfg%ra_obs + cfg%rb) * out%ccomp_tot + &
               out%rc_tot * cfg%comp_point%c_nh3) / &
               (cfg%comp_point%c_nh3 - out%ccomp_tot)
         else
            out%rc_eff = -9999.0 ! no flux, resistance undefined
         end if
      end associate
   end subroutine rc_eff
end module m_rc_eff
