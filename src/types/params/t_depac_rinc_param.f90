!------------------------------------------------------------------------------
! Module:     t_depac_rinc_param
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines abstract base type for leaf internal resistance parameterisations.
!   The depac_rinc_param type provides interface for soil moisture resistance
!   calculations affecting surface conductance.
!------------------------------------------------------------------------------

module t_depac_rinc_param
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_rinc_param

    type, abstract :: depac_rinc_param
    contains
        procedure(i_rinc_parameterisation), deferred :: apply
    end type depac_rinc_param
    abstract interface
        pure function i_rinc_parameterisation(this, setup, ctx) result(rinc)
            import :: depac_rinc_param
            import :: depac_context
            implicit none (type, external)
            class(depac_rinc_param), intent(in) :: this
            class(*), intent(in) :: setup ! only allow depac_setup, but prevent circular dependency
            type(depac_context), intent(in) :: ctx

            real :: rinc
        end function i_rinc_parameterisation

    end interface

end module t_depac_rinc_param
