module m_gw_nh3_sutton
   use t_depac_component_core, only: depac_component_core
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_error, only: depac_error

   implicit none (type, external)
   private
   public :: gw_nh3_sutton
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
   function gw_nh3_sutton(meteo, comp, dp_conf, err) result(gw)
      type(depac_meteorology), intent(in) :: meteo
      class(depac_component_core), intent(in) :: comp
      type(depac_config), intent(in) :: dp_conf
      type(depac_error), intent(inout) :: err

      real :: rw                ! external leaf resistance (s/m)
      real :: gw                ! external leaf conductance (m/s)


      !                  100 - rh
      !  rw = 2.0 * exp(----------)
      !                    12


      if (dp_conf%has_vegetation) then
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

   end function gw_nh3_sutton
end module m_gw_nh3_sutton
