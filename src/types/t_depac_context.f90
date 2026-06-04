!------------------------------------------------------------------------------
! Module:     t_depac_context
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines context type holding runtime state including meteorological data,
!   calculation outputs, and error information. The depac_context type maintains
!   all dynamic information during a single model calculation step.
!------------------------------------------------------------------------------

module t_depac_context
    use t_depac_meteorology_core, only: depac_meteorology_core
    use t_depac_state_core, only: depac_state_core
    use t_depac_output_core, only: depac_output_core
    use t_depac_error_core, only: depac_error_core
    implicit none (type, external)
    private
    public :: depac_context

    type :: depac_context
        logical :: has_leaves = .false.
        logical :: has_vegetation = .false.

        type(depac_meteorology_core) :: meteo
        type(depac_state_core) :: state
        type(depac_output_core) :: output
        type(depac_error_core) :: error
    end type depac_context

end module t_depac_context
