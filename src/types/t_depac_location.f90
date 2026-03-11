

!------------------------------------------------------------------------------
! Module:     t_depac_location
! Author:     Marte Voorneveld, RIVM
! Created:    2025-11-14
! Updated:    2026-11-18
! Description:
!   This module defines the location type. It is not required for the DepAC model,
!   but it is used in the test suite to provide location information,
!   and it is used in DepAC-1D.
!------------------------------------------------------------------------------
module t_depac_location
    implicit none (type, external)
    public

    !> Type representing a geographical location for DEPAC calculations.
    !! Contains the following fields:
    !! - name: Name of the location (allocatable character string). OPTIONAL.
    !! - lat: Latitude in degrees (default -999.0). OPTIONAL.
    !! - lon: Longitude in degrees (default -999.0). OPTIONAL.
    !! - elev: Elevation in meters (default -999.0), OPTIONAL..
    !! Note: The default values (-999.0) indicate missing or undefined data.
    type :: depac_location
        character(:), allocatable :: name
        real :: lat = -999.0
        real :: lon = -999.0
        real :: elev = -999.0
    end type depac_location

end module t_depac_location