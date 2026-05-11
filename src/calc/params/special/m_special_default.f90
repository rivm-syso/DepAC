module m_rc_special_default
    use c_depac_params, only: depac_rc_special_param
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context

    implicit none (type, external)
    private
    public :: rc_special_default



    type, extends(depac_rc_special_param) :: rc_special_default
    contains
        procedure :: apply => rc_special_default_apply
    end type rc_special_default

contains
    subroutine rc_special_default_apply(this, setup, ctx, ready)
        class(rc_special_default), intent(in) :: this
        type(depac_setup), intent(in) :: setup
        type(depac_context), intent(inout) :: ctx
        logical, intent(inout) :: ready

        if (ctx%meteo%nwet == 9) then
            ! snow surface:
            call rc_snow(setup, ctx)
            ready = .true.
        end if

    end subroutine rc_special_default_apply

end module m_rc_special_default
