!------------------------------------------------------------------------------
! Module:     t_depac_csoil_param
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines abstract base type for soil compensation point parameterisations.
!   The depac_csoil_param type provides interface for component-specific
!   soil compensation point calculations.
!------------------------------------------------------------------------------

module t_depac_csoil_param
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_csoil_param

    type, abstract :: depac_csoil_param
    contains
        procedure(i_csoil_parameterisation), deferred :: apply
    end type depac_csoil_param
    abstract interface
        pure function i_csoil_parameterisation(this, setup, ctx, tfac) result(csoil)
            import :: depac_csoil_param
            import :: depac_context
            implicit none (type, external)
            class(depac_csoil_param), intent(in) :: this
            class(*), intent(in) :: setup ! only allow depac_setup, but prevent circular dependency
            type(depac_context), intent(in) :: ctx
            real, intent(in) :: tfac

            real :: csoil
        end function i_csoil_parameterisation
    end interface

end module t_depac_csoil_param
