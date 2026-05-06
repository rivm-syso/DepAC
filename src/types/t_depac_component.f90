!------------------------------------------------------------------------------
! Module:     t_depac_component
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-28
! Updated:    2026-02-27
! Description:
!   This module defines derived types for atmospheric deposition modeling
!   components. It includes the 'lu_rsoil' type for land use-specific soil
!   resistance values, and the 'component' type for chemical species and
!   their associated parameters such as diffusion coefficient, snow/soil
!   resistance, and land use-dependent soil resistance.
!------------------------------------------------------------------------------

module t_depac_component
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use t_depac_error, only: depac_error

   implicit none (type, external)

   private
   public :: depac_component, gw_parameterisation


   
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
   ! First define the type
   type :: depac_component
      character(len=20) :: name
      integer :: index = -999
      real :: diffc = -999.0
      real :: rw_val = -999.0
      integer :: ipar_snow = -999
      real :: rsoil_frozen = -999.0
      real :: rsoil_wet = -999.0
      procedure(gw_parameterisation), pointer, nopass :: gw_param => null()
   end type depac_component

   abstract interface
      function gw_parameterisation(meteo, comp, dp_conf, err) result(gw)
         import :: depac_meteorology, depac_component, depac_config, depac_error

         type(depac_meteorology), intent(in) :: meteo
         type(depac_component), intent(in) :: comp
         type(depac_config), intent(in) :: dp_conf
         type(depac_error), intent(inout) :: err

         real :: gw
      end function gw_parameterisation
   end interface
   
end module t_depac_component
