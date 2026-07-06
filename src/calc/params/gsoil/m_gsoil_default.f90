!------------------------------------------------------------------------------
! Module:     m_gsoil_default
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Calculates soil conductance (gsoil) by computing effective soil resistance
!   considering frozen/wet soil conditions and soil moisture effects through
!   rain interception resistance (rinc). This module extends depac_gsoil_param
!   and is essential for modeling pollutant deposition to soil surfaces.
!------------------------------------------------------------------------------

module m_gsoil_default
   use c_depac_param_types, only: depac_gsoil_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context
   use m_helpers, only: missing
   implicit none (type, external)
   private
   public :: gsoil_default

   type, extends(depac_gsoil_param) :: gsoil_default
   contains
      procedure :: apply => gsoil_default_apply
   end type gsoil_default
contains
   pure function gsoil_default_apply(this, setup, ctx) result(gsoil)
      class(gsoil_default), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: gsoil
      real :: rinc
      real :: rsoil_eff

      select type (setup)
       type is (depac_setup)
         ! Compute effective soil resistance:

         rinc = setup%rinc_param%apply(setup,ctx)

         associate(meteo => ctx%meteo, comp => setup%component, lu_conf => setup%land_use)
            if(missing(rinc)) then
               rsoil_eff = -9999.0
               gsoil = -9999.0
            else
               if (meteo%t < 0.0) then
                  if (missing(comp%rsoil_frozen)) then
                     rsoil_eff = -9999.0
                  else
                     rsoil_eff = comp%rsoil_frozen + rinc
                  end if
               else
                  if (meteo%nwet == 0) then
                     if (missing(lu_conf%rsoil)) then
                        rsoil_eff = -9999.0
                     else
                        rsoil_eff = lu_conf%rsoil + rinc
                     end if
                  else if (meteo%nwet == 1) then
                     if (missing(comp%rsoil_wet)) then
                        rsoil_eff = -9999.0
                     else
                        rsoil_eff = comp%rsoil_wet + rinc
                     end if
                  else
                     rsoil_eff = -9999.0 ! missing nwet value for wet soil condition
                  end if
               end if
            end if

            ! Compute conductance:
            if (rsoil_eff > 0.0) then
               gsoil = 1.0/rsoil_eff
            else
               ! No deposition path, or missing value:
               gsoil = 0.0
            end if
         end associate
       class default
         ! If the setup is not of type depac_setup, return missing value
         gsoil = -999.0
      end select
   end function gsoil_default_apply

end module m_gsoil_default
