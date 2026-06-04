!------------------------------------------------------------------------------
! Module:     t_depac_gsoil_param
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines abstract base type for soil conductance parameterisations.
!   The depac_gsoil_param type provides interface for land use and component-specific
!   soil conductance calculations in dry deposition modeling.
!------------------------------------------------------------------------------

module t_depac_gsoil_param
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_gsoil_param

    type, abstract :: depac_gsoil_param
    contains
        procedure(i_gsoil_parameterisation), deferred :: apply
    end type depac_gsoil_param
    abstract interface
        pure function i_gsoil_parameterisation(this, setup, ctx) result(gsoil)
            import :: depac_gsoil_param
            import :: depac_context
            implicit none (type, external)
            class(depac_gsoil_param), intent(in) :: this
            class(*), intent(in) :: setup ! only allow depac_setup, but prevent circular dependency
            type(depac_context), intent(in) :: ctx

            real :: gsoil
        end function i_gsoil_parameterisation
    end interface
end module t_depac_gsoil_param
