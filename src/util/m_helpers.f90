!------------------------------------------------------------------------------
! Module:     m_helpers
! Author:     Marte Voorneveld, RIVM
! Created:    November 14 2025
! Updated:    November 18 2025
! Description:
!   This module provides helper routines for the DepAC atmospheric deposition
!   model, including missing-value detection for real and integer types.
!------------------------------------------------------------------------------

module m_helpers
      implicit none (type, external)
      public
      interface missing
            module procedure missing_real, missing_int
      end interface
contains

      ! Add helper functions or subroutines here as needed
      pure logical function missing_real(x)
         real, intent(in) :: x
         real, parameter :: EPS = 1.0e-5
         missing_real = (abs(x + 999.) <= EPS)
      end function missing_real

      pure logical function missing_int(x)
         integer, intent(in) :: x
         missing_int = (x == -999)
      end function missing_int


!***********************************************************************
!  FUNCTION           : fpsih
!  DESCRIPTION        : Stability correction function in the surface layer
!                       temperature profile. The present model is an empirical
!                       fit by Holtslag and De Bruin(1987) of data by Hicks
!                       (1976, Quart. J. R. Meteor. Soc., 102, 535-551).
!                       See also Holtslag (1984, BLM, 29, 225-250)
!  AUTHOR             : ANTON BELJAARS (KNMI 25-5-87) / Franka Loeve (Cap Volmac)
!
!  Feb 2010 MCvZ added to modmeteo_para, copied from ops_depu (OPS 4.1.17)
!***********************************************************************
      function fpsih(eta) result(res)
         real, intent(in) :: eta
         real :: res
         real :: y

         if (eta < 0.0) then
            y = sqrt(1.0 - 16.0 * eta)
            res = 2.0 * log((1.0 + y) / 2.0)
         else
            res = -(0.7 * eta) - (0.75 * eta - 10.72) * exp(-0.35 * eta) - 10.72
         end if
      end function fpsih


end module m_helpers