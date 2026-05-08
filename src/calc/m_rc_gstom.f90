!------------------------------------------------------------------------------
! Module:     m_rc_gstom
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 19 2025
! Description:
!   This module provides routines for calculating stomatal conductance and
!   related parameters for the DepAC atmospheric deposition model.
!------------------------------------------------------------------------------
module m_rc_gstom
    use t_depac_output, only: depac_output
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use, depac_stomatal_params
    use t_depac_meteorology,  only: depac_meteorology
    use t_depac_location, only: depac_location
    use t_depac_config, only: depac_config
    use m_logger, only: log_info, log_warn, log_error
    use m_depac_error, only: set_error
    use t_depac_error, only: ERR_INPUT, depac_error
    use default_indices, only: COMP_NO, COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3

    implicit none (type, external)
    public
contains

    !------------------------------------------------------------------------------
    ! Subroutine: rc_gstom
    ! Purpose   : Compute stomatal conductance for a given component
    ! Arguments :
    !   comp      - component type (chemical species)
    !   lu_conf   - land use configuration
    !   meteo     - meteorological data
    !   dp_conf   - DepAC configuration
    !   dp_out    - output structure
    !   err       - error handling
    ! Notes     : Implements component-specific logic and vegetation checks.
    !------------------------------------------------------------------------------
    subroutine rc_gstom(comp, lu_conf, meteo, dp_conf, dp_out, err)
        type(depac_component),      intent(in)    :: comp      ! current computed component
        type(depac_land_use),       intent(in)    :: lu_conf   ! current land-use type
        type(depac_meteorology),    intent(inout) :: meteo     ! meteorology
        type(depac_config),   intent(in)    :: dp_conf   ! depac config
        type(depac_output),   intent(inout) :: dp_out    ! depac output
        type(depac_error),    intent(inout) :: err       ! error handling


        dp_out%gstom = comp%gstom_param%apply(comp, lu_conf%stom_par, meteo, dp_conf)

    end subroutine rc_gstom

end module m_rc_gstom