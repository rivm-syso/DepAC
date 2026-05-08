!------------------------------------------------------------------------------
! Module:     m_rc_snow
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating snow resistance (rc_snow)
!   for atmospheric deposition modeling. The rc_snow subroutine computes
!   total resistance over snow surfaces using either a constant or
!   temperature-dependent parameterization, based on component and
!   meteorological input. Results are used in dry deposition calculations
!   and model output.
!------------------------------------------------------------------------------
module m_rc_snow
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_component_core, only: depac_component_core
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use m_logger, only: log_info, log_error
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_depac_error, only: set_error

    implicit none (type, external)
    private
    public :: rc_snow
contains
    !------------------------------------------------------------------------------
    ! Subroutine: rc_snow
    ! Purpose:    Calculates total resistance over snow surfaces (rc_tot) for
    !             atmospheric deposition modeling, using either a constant or
    !             temperature-dependent parameterization.
    ! Arguments:
    !   - meteo   : Meteorological input (type depac_meteorology)
    !   - comp    : Component-specific parameters (type depac_component)
    !   - dp_conf : Model configuration (type depac_config)
    !   - dp_out  : Output structure, rc_tot is set here (type depac_output)
    !   - err     : Error handling structure (type depac_error)
    ! Notes:
    !   - Used in dry deposition calculations and model output.
    !------------------------------------------------------------------------------
    subroutine rc_snow(meteo, comp, dp_conf, dp_out, err)
        type(depac_meteorology), intent(in) :: meteo       ! meteorology
        class(depac_component_core), intent(in) :: comp          ! current computed component
        type(depac_config), intent(in) :: dp_conf    ! depac config
        type(depac_output), intent(inout) :: dp_out  ! output of this run
        type(depac_error), intent(inout) :: err      ! error handling

        ! Choose parameterisation with constant or temperature dependent parameterisation:
        if (comp%ipar_snow == 1) then
            dp_out%rc_tot = dp_conf%rssnow
        elseif (comp%ipar_snow == 2) then
            if (meteo%t < -1.) then
                dp_out%rc_tot = 500.
            elseif (meteo%t >  1.) then
                dp_out%rc_tot = 70.
            else
                dp_out%rc_tot = 70.*(2.-meteo%t)
            endif
        else
            call set_error(err, ERR_INPUT, &
                'Programming error in rc_snow: unknown value of ipar_snow: '//trim(comp%name))
            call log_error('Programming error in rc_snow: unknown value of ipar_snow: ' &
                    //trim(comp%name))
            return
        endif
    end subroutine rc_snow
end module m_rc_snow
