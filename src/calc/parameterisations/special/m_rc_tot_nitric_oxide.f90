module m_rc_tot_nitric_oxide
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error
    use t_depac_component, only: t_rc_special_param
    use t_depac_component_core, only: depac_component_core
    use m_rc_snow, only: rc_snow
    use t_depac_config, only: depac_config
    ! nitric oxide parameterisation
    ! NO is treated as a special case, with only one total canopy resistance
    implicit none (type, external)
    private
    public :: rc_tot_nitric_oxide

    type, extends(t_rc_special_param) :: rc_tot_nitric_oxide
        real :: fixed_rc_tot = 2000.0
    contains
        procedure :: apply => rc_tot_nitric_oxide_apply
    end type rc_tot_nitric_oxide
contains
    subroutine rc_tot_nitric_oxide_apply(this, meteo, comp,dp_conf, dp_out, err,  ready)
        class(rc_tot_nitric_oxide), intent(in) :: this
        type(depac_meteorology), intent(in) :: meteo
        class(depac_component_core), intent(in) :: comp
        type(depac_config), intent(in) :: dp_conf
        type(depac_output), intent(inout) :: dp_out
        type(depac_error), intent(inout) :: err
        logical, intent(inout) :: ready

        if (meteo%nwet == 1) then
            dp_out%rc_tot = this%fixed_rc_tot
            ready = .true.
        else if (meteo%nwet == 9) then
            ! snow surface:
            call rc_snow(meteo, comp, dp_conf, dp_out, err)
            ready = .true.
        end if
            

        
    end subroutine rc_tot_nitric_oxide_apply

    
end module m_rc_tot_nitric_oxide