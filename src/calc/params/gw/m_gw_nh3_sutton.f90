!------------------------------------------------------------------------------
! Module:     m_gw_nh3_sutton
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Calculates external leaf resistance (rw) for ammonia (NH3) using
!   the Sutton & Fowler (1993) parameterization based on relative humidity
!   and temperature. This module extends depac_gw_param and is essential for
!   modeling NH3 dry deposition through the quasi-laminar boundary layer.
!------------------------------------------------------------------------------

module m_gw_nh3_sutton
   use c_depac_param_types, only: depac_gw_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: gw_nh3_sutton

   type, extends(depac_gw_param) :: gw_nh3_sutton
   contains
      procedure :: apply => gw_nh3_sutton_apply
   end type gw_nh3_sutton
contains
   !------------------------------------------------------------------------------
   ! Function: gw_nh3_sutton
   ! Reference:  Sutton, M.A. & Fowler, D. (1993). A Model for NO, NO2 and NH3
   !             Exchange Between Vegetation and the Atmosphere. In: Exchange of
   !             Trace Gases Between Terrestrial Ecosystems and the Atmosphere,
   !             Andreae & Schimel (eds.), Wiley, pp. 301-313.
   ! Description:
   !   Calculates the external leaf resistance (rw) for NH3 using the parameterization
   !   from Sutton & Fowler (1993). The resistance is a function of relative humidity
   !   and vegetation state. For frozen soil, a fixed value is used. For unfrozen
   !   conditions, the resistance decreases exponentially with increasing relative
   !   humidity, reflecting enhanced cuticular uptake at higher humidity.
   !------------------------------------------------------------------------------
   pure function gw_nh3_sutton_apply(this, setup, ctx) result(gw)
      class(gw_nh3_sutton), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: rw                ! external leaf resistance (s/m)
      real :: gw                ! external leaf conductance (m/s)


      !                  100 - rh
      !  rw = 2.0 * exp(----------)
      !                    12
      select type (setup)
       type is (depac_setup)
         associate(meteo => ctx%meteo, dp_conf => setup%config)
            if (ctx%has_vegetation) then
               if (meteo%t < 0.0) then
                  ! Frozen soil (from Depac v1):
                  gw = dp_conf%sai / 200.0
               else
                  ! Use the gamma_w parameterization for NH3:
                  rw = dp_conf%sai_grass_haarweg * 2.0 *exp((100.0 - meteo%rh) / 12.0)
                  gw = dp_conf%sai / rw
               end if
            else
               ! no vegetation:
               gw = 0.0
            endif
         end associate
       class default
         ! If the setup is not of type depac_setup, return missing value
         gw = -999.0
      end select
   end function gw_nh3_sutton_apply
end module m_gw_nh3_sutton
