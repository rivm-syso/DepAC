module t_depac_gw_param
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_gw_param

    type, abstract :: depac_gw_param
    contains
        procedure(i_gw_parameterisation), deferred :: apply
    end type depac_gw_param

    abstract interface
        pure function i_gw_parameterisation(this, setup, ctx) result(gw)
            import :: depac_gw_param
            import :: depac_context
            implicit none (type, external)
            class(depac_gw_param), intent(in) :: this
            class(*), intent(in) :: setup ! only allow depac_setup, but prevent circular dependency
            type(depac_context), intent(in) :: ctx

            real :: gw
        end function i_gw_parameterisation
    end interface
end module t_depac_gw_param