module m_gw_so2
   use t_depac_component_core, only: depac_component_core
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_error, only: depac_error

   implicit none (type, external)
   private
   public :: gw_so2
contains

   function gw_so2(meteo, comp, dp_conf, err)  result(gw)
      type(depac_meteorology), intent(in) :: meteo
      class(depac_component_core), intent(in) :: comp
      type(depac_config), intent(in) :: dp_conf
      type(depac_error), intent(inout) :: err

      real :: rw ! external leaf resistance (s/m)
      real :: gw ! external leaf conductance (m/s)

      if (dp_conf%has_vegetation) then
         if (meteo%nwet == 0) then
            ! dry surface
            if (meteo%t > -1.0) then
               if (meteo%rh < 81.3) then
                  rw = 25000.0 * exp(-0.0693 * meteo%rh)
               else
                  rw = 0.58e12 * exp(-0.278 * meteo%rh) + 10.0
               endif
            else
               if (meteo%t > -5.0) then
                  rw = 200.0
               else
                  rw = 500.0
               endif
            endif
         else
            ! wet surface
            ! see Table 5, Erisman et al, 1994 Atm. Environment,
            ! 0 is impl. as 10
            rw = 10.0
         endif

         ! very low NH3/SO2 ratio
         if (dp_conf%comp_point%iratns == 3) rw = rw + 50.0

         gw = 1.0 / rw
      else
         gw = 0.0
      endif

   end function gw_so2
end module m_gw_so2
