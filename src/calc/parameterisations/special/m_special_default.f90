module m_special_default
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error
    use t_depac_component, only: t_rc_special_param
    use t_depac_config, only: depac_config
    use t_depac_component_core, only: depac_component_core
    use m_rc_snow, only: rc_snow

    implicit none (type, external)
    private
    public :: rc_special_default


   
    type, extends(t_rc_special_param) :: rc_special_default
    contains
        procedure :: apply => rc_special_default_apply
    end type rc_special_default

contains
    subroutine rc_special_default_apply(this, meteo, comp, dp_conf, dp_out, err, ready)
        class(rc_special_default), intent(in) :: this
        type(depac_meteorology), intent(in) :: meteo
        class(depac_component_core), intent(in) :: comp
        type(depac_config), intent(in) :: dp_conf
        type(depac_output), intent(inout) :: dp_out
        type(depac_error), intent(inout) :: err
        logical, intent(inout) :: ready

        if (meteo%nwet == 9) then
            ! snow surface:
            call rc_snow(meteo, comp, dp_conf, dp_out, err)
            ready = .true.
        end if

    end subroutine rc_special_default_apply

end module m_special_default