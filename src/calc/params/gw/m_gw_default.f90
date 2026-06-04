!------------------------------------------------------------------------------
! Module:     m_gw_default
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Default/placeholder parameterization that returns 0.0 for external leaf
!   conductance (gw). This module extends depac_gw_param and serves as a
!   fallback option for components where specific gw parameterizations are
!   not available or not applicable.
!------------------------------------------------------------------------------

module m_gw_default
   use c_depac_param_types, only: depac_gw_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   use m_helpers, only: missing

   implicit none (type, external)
   private
   public :: gw_default

   type, extends(depac_gw_param) :: gw_default
   contains
      procedure :: apply => gw_default_apply
   end type gw_default
contains
   pure function gw_default_apply(this, setup, ctx) result(gw)
      class(gw_default), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: gw

      select type (setup)
       type is (depac_setup)

         ! Default gw parameterisation: returns 0.0 for all components and conditions
         ! This can be used as a placeholder or for components that do not require gw.
         if(missing(setup%component%rw_val)) then

            gw = 0.0
            return
         end if


         if(ctx%has_vegetation) then
            gw = 1.0/setup%component%rw_val
         else
            gw = 0.0
         end if

         class default
            ! If the setup is not of type depac_setup, return missing value
            gw = -999.0
      end select

   end function gw_default_apply
end module m_gw_default
