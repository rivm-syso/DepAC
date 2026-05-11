module t_depac_comp_point_param
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_comp_point_param

    type, abstract :: depac_comp_point_param
    contains
        procedure(i_comp_point_parameterisation), deferred :: apply
    end type depac_comp_point_param
    abstract interface
        pure function i_comp_point_parameterisation(this, setup, ctx) result(ccomp_tot)
            import :: depac_comp_point_param
            import :: depac_context
            implicit none (type, external)
            class(depac_comp_point_param), intent(in) :: this
            class(*), intent(in) :: setup ! only allow depac_setup, but prevent circular dependency
            type(depac_context), intent(in) :: ctx
            
            real :: ccomp_tot
        end function i_comp_point_parameterisation
    end interface
end module t_depac_comp_point_param