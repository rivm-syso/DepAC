!------------------------------------------------------------------------------
! Module:     m_ra
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    January 14 2026
! Description:
!   This module provides routines for calculating aerodynamic resistance (Ra)
!   for atmospheric deposition modeling. The depac_calc_ra subroutine computes
!   aerodynamic resistance based on meteorological and surface parameters,
!   supporting dry deposition calculations and model output.
!------------------------------------------------------------------------------
module m_ra
    use m_logger, only: log_debug
    use c_depac_core, only: depac_meteorology_core
    implicit none (type, external)
    public
contains

    ! --------------------------------------------------------------------------
    ! Function:   depac_calc_ra
    ! Description:
    !   Calculates aerodynamic resistance (ra) using meteorological data.
    !   The formula used is ra = ws10 / ust^2, where ws10 is the wind speed at
    !   10 meters and ust is the friction velocity.
    ! --------------------------------------------------------------------------
    function depac_calc_ra(meteo) result(ra)
        type(depac_meteorology_core), intent(in) :: meteo
        real :: ra

        if (meteo%ust <= 0.0) then
            ra = -999.0
            call log_debug('Invalid friction velocity (ust <= 0). Returning -999.0 for ra.')
            return
        end if

        if (meteo%ws10 < 0.0) then
            ra = -999.0
            call log_debug('Invalid wind speed at 10m (ws10 < 0). Returning -999.0 for ra.')
            return
        end if

        ra = meteo%ws10 / meteo%ust**2

    end function depac_calc_ra

    ! --------------------------------------------------------------------------
    ! Function:   depac_calc_ra_obs_h
    ! Description:
    !   Calculates aerodynamic resistance (Ra) at a specified observation height
    !   using meteorological data. The formula used is:
    !   Ra = 1 / (k * ust) * [ln(obs_h/z0) - fpsih(obs_h/ol) + fpsih(z0/ol)]
    !   where k is the von Karman constant, ust is the friction velocity,
    !   z0 is the surface roughness length, ol is the Monin-Obukhov length,
    !   and obs_h is the observation height.
    !   fpsih is the stability correction function in the surface layer
    !   temperature profile. Empirical fit by Holtslag and De Bruin (1987).
    ! --------------------------------------------------------------------------
    function depac_calc_ra_obs_h(meteo, obs_h) result(ra)
        type(depac_meteorology_core), intent(in) :: meteo
        real, intent(in) :: obs_h
        real :: ra
        ! Local variables
        real, parameter :: k = 0.4 ! von Karman constant

        ! Check for valid inputs
        if (meteo%ust <= 0.0) then
            ra = -999.0
            call log_debug('Invalid friction velocity (ust <= 0). Returning -999.0 for ra.')
            return
        end if

        if (meteo%z0 <= 0.0) then
            ra = -999.0
            call log_debug('Invalid surface roughness length (z0 <= 0). Returning -999.0 for ra.')
            return
        end if

        ra = 1.0 / (k * meteo%ust) * &
            (log((obs_h/meteo%z0)) - fpsih(obs_h/meteo%ol) + fpsih(meteo%z0/meteo%ol))

    end function depac_calc_ra_obs_h

end module m_ra