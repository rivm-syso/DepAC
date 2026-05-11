module m_gw_nh3_sutton
   use c_depac_params, only: depac_gw_param
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
   function gw_nh3_sutton_apply(this, setup, ctx) result(gw)
      class(gw_nh3_sutton), intent(in) :: this
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: rw                ! external leaf resistance (s/m)
      real :: gw                ! external leaf conductance (m/s)


      !                  100 - rh
      !  rw = 2.0 * exp(----------)
      !                    12

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

   end function gw_nh3_sutton_apply
end module m_gw_nh3_sutton
