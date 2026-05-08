module m_gstom_default
    use t_depac_component_core, only: depac_component_core
    use t_depac_land_use, only: depac_stomatal_params
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_component, only: t_gstom_parameterisation
    implicit none (type, external)
    private
    public :: gstom_default

    type, extends(t_gstom_parameterisation) :: gstom_default
    contains
        procedure :: apply => gstom_default_apply
    end type gstom_default

contains
 pure function gstom_default_apply(this, comp, stom_par, meteo, dp_conf) result(gstom)
      class(gstom_default), intent(in) :: this
      class(depac_component_core), intent(in) :: comp
      type(depac_stomatal_params), intent(in) :: stom_par
      type(depac_meteorology), intent(in) :: meteo
      type(depac_config), intent(in) :: dp_conf
      real :: gstom

      ! Default gstom parameterisation: returns 0.0 for all components and conditions
      ! This can be used as a placeholder or for components that do not require gstom.
      gstom = 0.0
 end function gstom_default_apply

end module m_gstom_default
