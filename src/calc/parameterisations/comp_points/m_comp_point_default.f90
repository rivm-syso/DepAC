module m_comp_point_default
   use t_depac_component_core, only: depac_component_core
   use t_depac_land_use, only: depac_land_use
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_error, only: depac_error
   use t_depac_output, only: depac_output

   implicit none (type, external)
   private
   public :: comp_point_default
contains
   pure function comp_point_default(meteo, lu_conf, dp_conf, dp_out) result(ccomp_tot)
      type(depac_meteorology), intent(in) :: meteo
      type(depac_land_use), intent(in) :: lu_conf
      type(depac_config), intent(in) :: dp_conf
      type(depac_output), intent(in) :: dp_out

      real :: ccomp_tot

      ! Default component point parameterisation: returns 0.0 for all components and conditions
      ! This can be used as a placeholder or for components that do not require a specific parameterisation.
      ccomp_tot = 0.0

   end function comp_point_default


end module m_comp_point_default
