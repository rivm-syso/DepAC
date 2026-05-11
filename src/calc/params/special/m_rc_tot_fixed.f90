module m_rc_tot_fixed
    use c_depac_params, only: depac_rc_special_param
    use t_depac_setup, only: depac_setup
    use t_depac_context, only: depac_context
    implicit none (type, external)

    private
    public :: rc_tot_fixed


    type, extends(depac_rc_special_param) :: rc_tot_fixed
        real :: rc_tot_fixed = 2000.0
    contains
        procedure :: apply => rc_tot_fixed_apply
    end type rc_tot_fixed

contains

    subroutine rc_tot_fixed_apply(this, setup, ctx, ready)

        class(rc_tot_fixed), intent(in) :: this
        type(depac_setup), intent(in) :: setup
        type(depac_context), intent(inout) :: ctx
        logical, intent(inout) :: ready

        ctx%output%rc_tot = this%rc_tot_fixed
        ready = .true.

    end subroutine rc_tot_fixed_apply

end module m_rc_tot_fixed