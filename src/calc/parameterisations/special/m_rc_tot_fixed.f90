module m_rc_tot_fixed
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error
    use t_depac_component, only: t_rc_special_param
    use t_depac_config, only: depac_config
    use t_depac_component_core, only: depac_component_core

    implicit none (type, external)

    private
    public :: rc_tot_fixed


    type, extends(t_rc_special_param) :: rc_tot_fixed
        real :: rc_tot_fixed = 2000.0
    contains
        procedure :: apply => rc_tot_fixed_apply
    end type rc_tot_fixed

contains

    subroutine rc_tot_fixed_apply(this, meteo, comp, dp_conf, dp_out, err, ready)

        class(rc_tot_fixed), intent(in) :: this
        type(depac_meteorology), intent(in) :: meteo
        class(depac_component_core), intent(in) :: comp
        type(depac_config), intent(in) :: dp_conf
        type(depac_output), intent(inout) :: dp_out
        type(depac_error), intent(inout) :: err
        logical, intent(inout) :: ready

        dp_out%rc_tot = this%rc_tot_fixed
        ready = .true.

    end subroutine rc_tot_fixed_apply

end module m_rc_tot_fixed