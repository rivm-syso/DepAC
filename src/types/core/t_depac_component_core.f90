!------------------------------------------------------------------------------
! Module:     t_depac_component_core
! Author:     Marte Voorneveld, RIVM
! Created:    May 12, 2026
! Modified:   May 12, 2026
! Description:
!   Defines core chemical component type with name, index, diffusion coefficient,
!   and snow/soil resistance parameters. The depac_component_core type serves as
!   the base type for extended component parameter definitions.
!------------------------------------------------------------------------------
module t_depac_component_core
    implicit none (type, external)
    private
    public :: depac_component_core

    !> Type representing a chemical component for DEPAC calculations.
   !! Contains the following fields:
   !! - name: Name of the component (character(len=20)), e.g. "NH3", "O3", etc.
   !! - diffc: Diffusion coefficient for gstom parametrisation.
   !!      Dimensionless
   !! - rw_val: Constant rw value (default -999.0). (s/m)
   !! - ipar_snow: Snow resistance parametrisation (1=constant, 2=temperature dependent).
   !! - rsoil_frozen: Frozen soil resistance (default -999.0).
   !! - rsoil_wet: Wet soil resistance (default -999.0).
   !! Note: The default values (-999.0 or -999) indicate missing or undefined data.
   ! First define the type for the core component parameters, then extend it to
   ! include parameterisation function pointers.
   type depac_component_core
      character(len=20) :: name
      integer :: index = -999
      real :: diffc = -999.0
      real :: rw_val = -999.0
      integer :: ipar_snow = -999
      real :: rsoil_frozen = -999.0
      real :: rsoil_wet = -999.0
   end type depac_component_core
end module t_depac_component_core
