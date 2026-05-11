!------------------------------------------------------------------------------
! Module:     t_depac_land_use
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-13
! Updated:    2026-02-27
! Description:
!   This module defines derived types for land use parameters used in
!   atmospheric deposition modeling. It includes types for stomatal parameters,
!   resistance parameters, and the main land use type containing all relevant
!   properties.
!------------------------------------------------------------------------------

module t_depac_land_use
   use c_depac_core, only: depac_land_use_core

   implicit none (type, external)
   private
   public :: depac_land_use, depac_stomatal_params, depac_rc_r_params


   !> Type representing stomatal conductance parameters.
   !! Contains the following fields:
   !! - F_min: Minimum stomatal conductance (typical values 0.01, default -999.0).
   !! - alpha: Alpha for F_light calculation (default -999.0).
   !! - Topt: Optimal temperature for F_temp calculation (default -999.0).
   !! - Tmin: Minimum temperature for F_temp calculation (default -999.0).
   !! - Tmax: Maximum temperature for F_temp calculation (default -999.0).
   !! - g_max: Maximum stomatal conductance (default -999.0).
   !! - vpd_max: Upper VPD limit for stomatal conductance reduction (default -999.0).
   !! - vpd_min: Lower VPD limit for stomatal conductance reduction (default -999.0).
   !! Note: The default values (-999.0) indicate missing or undefined data.
   type :: depac_stomatal_params
      real :: F_min = -999.0
      real :: alpha = -999.0
      real :: Topt = -999.0
      real :: Tmin = -999.0
      real :: Tmax = -999.0
      real :: g_max = -999.0
      real :: vpd_max = -999.0
      real :: vpd_min = -999.0
   end type depac_stomatal_params

   !> Type representing parameters for rinc calculation.
   !! Contains the following fields:
   !! - b: Rinc parameter b (default -999.0).
   !! - h: Rinc parameter h (default -999.0).
   !! Note: The default values (-999.0) indicate missing or undefined data.
   type :: depac_rc_r_params
      real :: b = -999.0
      real :: h = -999.0
   end type depac_rc_r_params


   type , extends(depac_land_use_core) :: depac_land_use
      type(depac_stomatal_params) :: stom_par
      type(depac_rc_r_params) :: rc_rinc
   end type depac_land_use

end module t_depac_land_use
