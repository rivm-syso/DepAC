!------------------------------------------------------------------------------
! Module:     m_gw_default
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
! Parameterization that scales external leaf conductance (gw) based on the
! Specific Area Index (SAI). This module extends depac_gw_param and provides
! a method to calculate gw by considering the SAI when vegetation is present.
!------------------------------------------------------------------------------

module m_gw_sai_scaling
   use c_depac_param_types, only: depac_gw_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   use m_helpers, only: missing

   implicit none (type, external)
   private
   public :: gw_sai_scaling

   type, extends(depac_gw_param) :: gw_sai_scaling
   contains
      procedure :: apply => gw_sai_scaling_apply
   end type gw_sai_scaling
contains
   pure function gw_sai_scaling_apply(this, setup, ctx) result(gw)
      class(gw_sai_scaling), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: gw

      select type (setup)
       type is (depac_setup)

         ! Returns gw scaled by SAI (Specific Area Index) if vegetation is present,
         ! otherwise returns 0.0.
         if(missing(setup%component%rw_val)) then

            gw = 0.0
            return
         end if


         if(ctx%has_vegetation) then
            ! apply scaling based on SAI (Specific Area Index) if vegetation is present
            gw = ctx%state%sai/setup%component%rw_val
         else
            gw = 0.0
         end if

       class default
         ! If the setup is not of type depac_setup, return missing value
         gw = -999.0
      end select

   end function gw_sai_scaling_apply
end module m_gw_sai_scaling
