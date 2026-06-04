!------------------------------------------------------------------------------
! Module:     m_rc_tot_nitric_acid
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Calculates total canopy resistance (rc_tot) for nitric acid (HNO3) based
!   on temperature and surface wetness conditions. This module extends
!   depac_rc_special_param and implements HNO3-specific deposition pathways
!   with distinct resistance values for wet and dry conditions.
!------------------------------------------------------------------------------

module m_rc_tot_nitric_acid
   use c_depac_param_types, only: depac_rc_special_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   ! nitric acid parameterisation
   ! HNO3 is treated as a special case, with only one total canopy resistance
   implicit none (type, external)
   private
   public :: rc_tot_nitric_acid

   type, extends(depac_rc_special_param) :: rc_tot_nitric_acid
   contains
      procedure :: apply => rc_tot_nitric_acid_apply
   end type rc_tot_nitric_acid

contains

   subroutine rc_tot_nitric_acid_apply(this, setup, ctx, ready)
      class(rc_tot_nitric_acid), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx
      logical, intent(inout) :: ready

      select type (setup)
       type is (depac_setup)
         if (ctx%meteo%t < -5.0 .and. ctx%meteo%nwet == 9) then
            ! T < 5 C and snow:
            ctx%output%rc_tot = 50.
         else
            ! all other circumstances:
            ctx%output%rc_tot = 10.0
         end if
         ready = .true.
       class default
         ! If the setup is not of type depac_setup, set ready to false and do not modify rc_tot
         ready = .false.
      end select
   end subroutine rc_tot_nitric_acid_apply
end module m_rc_tot_nitric_acid
