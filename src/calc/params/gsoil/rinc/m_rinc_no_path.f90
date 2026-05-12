!------------------------------------------------------------------------------
! Module:     m_rinc_no_path
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Returns -999.0 (missing value) for soil moisture resistance (rinc),
!   effectively blocking soil moisture transport and eliminating the soil
!   pathway. This module extends depac_rinc_param and is used when the soil
!   is not accessible for deposition.
!------------------------------------------------------------------------------

module m_rinc_no_path
   use c_depac_param_types, only: depac_rinc_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: rinc_no_path

   type, extends(depac_rinc_param) :: rinc_no_path
   contains
      procedure :: apply => rinc_no_path_apply
   end type rinc_no_path
contains
   pure function rinc_no_path_apply(this, setup, ctx) result(rinc)
      class(rinc_no_path), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: rinc

      select type (setup)
       type is (depac_setup)
         ! This parameterisation returns a very high rinc value to effectively
         ! block soil moisture transport.
         rinc = -999.0
       class default
         ! If the setup is not of type depac_setup, return missing value
         rinc = -999.0
      end select
   end function rinc_no_path_apply

end module m_rinc_no_path
