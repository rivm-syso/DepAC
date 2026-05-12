!------------------------------------------------------------------------------
! Module:     t_depac_comp_point_param
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines abstract base type for compensation point parameterisations.
!   The depac_comp_point_param type provides interface for component-specific
!   compensation point calculations.
!------------------------------------------------------------------------------

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