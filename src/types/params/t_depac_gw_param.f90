!------------------------------------------------------------------------------
! Module:     t_depac_gw_param
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines abstract base type for leaf/canopy conductance parameterisations.
!   The depac_gw_param type provides interface for component and land use-specific
!   quasi-laminar boundary layer conductance calculations.
!------------------------------------------------------------------------------

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
