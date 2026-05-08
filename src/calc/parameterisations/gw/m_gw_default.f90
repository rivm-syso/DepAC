module m_gw_default
   use t_depac_component_core, only: depac_component_core
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_error, only: depac_error
   use m_helpers, only: missing

   implicit none (type, external)
   private
   public :: gw_default
contains
   function gw_default(meteo, comp, dp_conf, err) result(gw)
      type(depac_meteorology), intent(in) :: meteo
      class(depac_component_core), intent(in) :: comp
      type(depac_config), intent(in) :: dp_conf
      type(depac_error), intent(inout) :: err
      real :: gw

      ! Default gw parameterisation: returns 0.0 for all components and conditions
      ! This can be used as a placeholder or for components that do not require gw.
      if(missing(comp%rw_val)) then

         gw = 0.0
         return
      end if


      if(dp_conf%has_vegetation) then
         gw = 1.0/comp%rw_val
      else
         gw = 0.0
      endif


   end function gw_default
end module m_gw_default
