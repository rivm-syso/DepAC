module t_depac_gstom_param
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_gstom_param

    type, abstract :: depac_gstom_param
    contains
        procedure(i_gstom_parameterisation), deferred :: apply
    end type depac_gstom_param
    abstract interface
        pure function i_gstom_parameterisation(this, setup, ctx) result(gstom)
            import :: depac_gstom_param
            import :: depac_context
            implicit none (type, external)
            class(depac_gstom_param), intent(in) :: this
            class(*), intent(in) :: setup ! only allow depac_setup, but prevent circular dependency
            type(depac_context), intent(in) :: ctx
            
            real :: gstom
        end function i_gstom_parameterisation
    end interface
end module t_depac_gstom_param