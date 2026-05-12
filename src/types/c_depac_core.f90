
!------------------------------------------------------------------------------
! Module:     c_depac_core
! Author:     Marte Voorneveld, RIVM
! Created:    November 28, 2025
! Modified:   May 12, 2026
! Description:
!   Collector module that imports and exports core derived types for the DepAC
!   atmospheric deposition model. Provides convenient access to fundamental
!   types including components, configuration, error handling, land use,
!   meteorology, and output structures.
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Module:     c_depac_core
! Author:     Marte Voorneveld, RIVM
! Created:    November 28, 2025
! Modified:   May 12, 2026
! Description:
!   Collector module that imports and exports core derived types for the DepAC
!   atmospheric deposition model. Provides convenient access to fundamental
!   types including components, configuration, error handling, land use,
!   meteorology, and output structures.
!------------------------------------------------------------------------------
module c_depac_core
    use t_depac_component_core, only: depac_component_core
    use t_depac_config_core, only: depac_config_core
    use t_depac_error_core, only: depac_error_core
    use t_depac_land_use_core, only: depac_land_use_core
    use t_depac_meteorology_core, only: depac_meteorology_core
    use t_depac_output_core, only: depac_output_core

    implicit none (type, external)
    private
    public :: depac_component_core, depac_config_core, depac_error_core, &
              depac_land_use_core, depac_meteorology_core, depac_output_core


end module c_depac_core