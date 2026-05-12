!------------------------------------------------------------------------------
! Module:     t_depac_component
! Author:     Marte Voorneveld, RIVM
! Created:    November 28, 2025
! Modified:   May 12, 2026
! Description:
!   Defines derived type for atmospheric deposition modeling components
!   (chemical species) and their properties. The depac_component type extends
!   depac_component_core with additional parameters for deposition calculations.
!------------------------------------------------------------------------------
! Description:
!   This module defines derived types for atmospheric deposition modeling
!   components. It includes the 'lu_rsoil' type for land use-specific soil
!   resistance values, and the 'component' type for chemical species and
!   their associated parameters such as diffusion coefficient, snow/soil
!   resistance, and land use-dependent soil resistance.
!------------------------------------------------------------------------------

module t_depac_component
   use c_depac_core, only: depac_component_core

   implicit none (type, external)

   private
   public :: depac_component


   type, extends(depac_component_core) :: depac_component
   end type depac_component

end module t_depac_component
