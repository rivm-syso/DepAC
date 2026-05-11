module t_depac_context
    use t_depac_meteorology_core, only: depac_meteorology_core
    use t_depac_output_core, only: depac_output_core
    use t_depac_error_core, only: depac_error_core
    implicit none (type, external)
    private
    public :: depac_context

    type :: depac_context
        logical :: has_leaves = .false.
        logical :: has_vegetation = .false.
        
        type(depac_meteorology_core) :: meteo
        type(depac_output_core) :: output
        type(depac_error_core) :: error
    end type depac_context

end module t_depac_context