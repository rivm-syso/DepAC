!------------------------------------------------------------------------------
! Module:     m_rc_gsoil
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating soil conductance (rc_gsoil)
! for atmospheric deposition modeling.
!   The rc_gsoil subroutine computes effective soil resistance and conductance
!   based on meteorological, land use, and component parameters, including
!   handling for frozen and wet soil conditions. The results are used in dry
!   deposition calculations and model output.
!------------------------------------------------------------------------------

module m_rc_gsoil
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_config, only: depac_config
    use m_depac_error, only: set_error
    use t_depac_error, only: ERR_INPUT
    use m_logger, only: log_info, log_error
    use m_helpers, only: missing
    use default_indices, only: LU_GRASS, LU_OTHER
    implicit none (type, external)
    public
    contains

    subroutine rc_gsoil(lu_conf, meteo, comp, dp_conf, dp_out, err)
        type(depac_land_use), intent(in) :: lu_conf    ! current computed land-use type
        type(depac_meteorology), intent(inout) :: meteo ! meteorology
        type(depac_component), intent(in) :: comp      ! current computed component
        type(depac_config), intent(in) :: dp_conf ! depac configs
        type(depac_output), intent(inout) :: dp_out ! output of this run
        type(depac_error), intent(inout) :: err  ! error handling

        real :: rinc           ! in canopy resistance  (s/m)
        real :: rsoil_eff      ! effective soil resistance (s/m)

        dp_out%gsoil_eff = lu_conf%gsoil_param(lu_conf, meteo, dp_conf)
    end subroutine rc_gsoil

end module m_rc_gsoil