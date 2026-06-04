!------------------------------------------------------------------------------
! Module:     m_gw_custom
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Example custom parameterisation for external leaf conductance (gw). 
!------------------------------------------------------------------------------

module m_gw_custom
   use c_depac_param_types, only: depac_gw_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   use m_helpers, only: missing

   implicit none (type, external)
   private
   public :: gw_custom

   type, extends(depac_gw_param) :: gw_custom
   contains
      procedure :: apply => gw_custom_apply
   end type gw_custom
contains
   pure function gw_custom_apply(this, setup, ctx) result(gw)
      class(gw_custom), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: gw

      select type (setup)
       type is (depac_setup)

         ! here we have access to all the context.
       
         associate(meteo => ctx%meteo)
            ! As an exmample see how the meteo can be used
            if (meteo%t < 0.0) then
               ! frozen atmos
               gw = 0.0
            else
               ! Custom parameterisation for unfrozen conditions:
               ! For demonstration, we set a fixed value. In a real case, this could be a function of meteo and state variables.
               gw = 10.0
            end if
         end associate
         
      end select

   end function gw_custom_apply
end module m_gw_custom
