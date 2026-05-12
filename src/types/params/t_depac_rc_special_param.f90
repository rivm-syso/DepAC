!------------------------------------------------------------------------------
! Module:     t_depac_rc_special_param
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines abstract base type for special canopy resistance parameterisations.
!   The depac_rc_special_param type provides interface for component-specific
!   resistance calculations used in dry deposition modeling.
!------------------------------------------------------------------------------

module t_depac_rc_special_param
    use t_depac_context, only: depac_context
    implicit none (type, external)
    private
    public :: depac_rc_special_param

    type, abstract :: depac_rc_special_param
    contains
        procedure(i_rc_special), deferred :: apply
    end type depac_rc_special_param
    abstract interface
        subroutine i_rc_special(this, setup, ctx, ready)
            import :: depac_rc_special_param
            import :: depac_context
            implicit none (type, external)
            class(depac_rc_special_param), intent(in) :: this
            class(*), intent(in) :: setup ! only allow depac_setup, but prevent circular dependency
            type(depac_context), intent(inout) :: ctx
            logical, intent(inout) :: ready
        end subroutine i_rc_special
    end interface
end module t_depac_rc_special_param