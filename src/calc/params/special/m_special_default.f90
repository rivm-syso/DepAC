!------------------------------------------------------------------------------
! Module:     m_rc_special_default
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Default special case parameterization for total canopy resistance (rc_tot).
!   Handles general resistance calculations, with special treatment for snow
!   surfaces by delegating to the rc_snow module. This module extends
!   depac_rc_special_param and serves as the standard rc_tot implementation.
!------------------------------------------------------------------------------

module m_rc_special_default
   use c_depac_param_types, only: depac_rc_special_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   use m_rc_snow, only: rc_snow

   implicit none (type, external)
   private
   public :: rc_special_default



   type, extends(depac_rc_special_param) :: rc_special_default
   contains
      procedure :: apply => rc_special_default_apply
   end type rc_special_default

contains
   subroutine rc_special_default_apply(this, setup, ctx, ready)
      class(rc_special_default), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx
      logical, intent(inout) :: ready

      select type (setup)
       type is (depac_setup)

         if (ctx%meteo%nwet == 9) then
            ! snow surface:
            call rc_snow(setup, ctx)
            ready = .true.
         end if
       class default
         ! If the setup is not of type depac_setup, set ready to false and do not modify rc_tot
         ready = .false.
      end select

   end subroutine rc_special_default_apply

end module m_rc_special_default
