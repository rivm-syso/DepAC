!------------------------------------------------------------------------------
! Module:     m_version
! Author:     Marte Voorneveld, RIVM
! Description:
!   This module provides version and build date information for the DepAC
!   atmospheric deposition model. The VERSION should match the VERSION file
!   (part of the test suite).
!------------------------------------------------------------------------------
module m_version
  implicit none (type, external)
  public
  ! SHOULD BE THE SAME AS VERSION file
  character(len=*), parameter :: VERSION = "4.0.0"
  character(len=*), parameter :: BUILD_DATE = "2026-03-10"
end module m_version