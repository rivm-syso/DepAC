!------------------------------------------------------------------------------
! Module:     m_rinc_default
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Calculates soil moisture resistance (rinc) based on friction velocity
!   and land use configuration parameters. This module extends depac_rinc_param
!   and provides the standard parameterization for modeling the effect of soil
!   moisture on surface conductance in dry deposition calculations.
!------------------------------------------------------------------------------

module m_rinc_default
   use c_depac_param_types, only: depac_rinc_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   implicit none (type, external)
   private
   public :: rinc_default

   type, extends(depac_rinc_param) :: rinc_default
   contains
      procedure :: apply => rinc_default_apply
   end type rinc_default
contains
   pure function rinc_default_apply(this, setup, ctx) result(rinc)
      class(rinc_default), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: rinc

      select type (setup)
       type is (depac_setup)
         associate(meteo => ctx%meteo, dp_conf => setup%config, rc_rinc => setup%land_use%rc_rinc)
            if (meteo%ust > 0.0) then
               rinc = rc_rinc%b * rc_rinc%h * dp_conf%sai / meteo%ust
            else
               rinc = 1000.0
            endif
         end associate
       class default
         ! If the setup is not of type depac_setup, return missing value
         rinc = -999.0
      end select
   end function rinc_default_apply

end module m_rinc_default
