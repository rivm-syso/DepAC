!------------------------------------------------------------------------------
! Module:     t_depac_meteorology
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-14
! Updated:    2026-02-27
! Description:
!   This module defines derived types for meteorological parameters used in
!   atmospheric deposition modeling. It includes the 'meteorology' type,
!   which stores weather and surface variables such as temperature, humidity,
!   wind speed, radiation, and calculated parameters relevant for model input
!   and simulation.
!------------------------------------------------------------------------------
module t_depac_meteorology
    implicit none (type, external)
    public
   !> Type representing meteorological inputs for DEPAC calculations.
   !! Contains the following fields:
   !! - t: Air temperature (C, provided by meteo, alias: temp, default -999.0).
   !! - tsurf: Surface temperature (C, provided by meteo, default -999.0).
   !! - rh: Relative humidity (percent, provided by meteo, alias: hum, default -999.0).
   !! - glrad: Global radiation (J/cm2/hr, alias: jcm2, default -999.0).
   !! - pres_0: Surface level pressure (Pa, for ops set to 101500, default -999.0).
   !! - ws10: Wind speed at 10 meters (m/s, provided by meteo, optional for ra/rb, default -999.0).
   !! - nwet: Wetness indicator (0=dry, 1=wet, 9=unused snow, default -999).
   !! - ust: Friction velocity (m/s, required for depac_finish, default -999.0).
   !! - ol: Monin-Obukhov length (m, for Ra calculation, default -999.0).
   !! - z0: Surface roughness length (m, for Ra calculation, default -999.0).
   !! - sinphi: Sine of solar elevation angle (0-1, default -999.0).
   !! Note: The default values (-999.0 or -999) indicate missing or undefined data.
   type :: depac_meteorology
      real :: t = -999.0
      real :: tsurf = -999.0
      real :: rh = -999.0
      real :: glrad = -999.0
      real :: pres_0 = 101500.
      real :: ws10 = -999.0
      integer :: nwet = -999
      real :: ust = -999.0
      real :: ol = -999.0
      real :: z0 = -999.0
      real :: sinphi = -999.0
   end type depac_meteorology
end module t_depac_meteorology
