module i_gstom_parameterisation
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_stomatal_params
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
   implicit none (type, external)
   public :: gstom_parameterisation
   abstract interface
      pure function gstom_parameterisation(comp, stom_par, meteo, dp_conf) result(gstom)
         import :: depac_component, depac_stomatal_params, depac_meteorology, depac_config
         type(depac_component), intent(in) :: comp
         type(depac_stomatal_params), intent(in) :: stom_par
         type(depac_meteorology), intent(in) :: meteo
         type(depac_config), intent(in) :: dp_conf

         real :: gstom
      end function gstom_parameterisation
   end interface
end module i_gstom_parameterisation
