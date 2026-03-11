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

        ! Compute in canopy (in crop) resistance:
        call rc_rinc(lu_conf, meteo, dp_conf, dp_out, rinc)

        ! Check for missing deposition path:
        if (missing(rinc)) then
            rsoil_eff = -9999.
        else
            ! Frozen soil (temperature below 0):
            if (meteo%t < 0.0) then
                if (missing(comp%rsoil_frozen)) then
                    rsoil_eff = -9999.
                else
                    rsoil_eff = comp%rsoil_frozen + rinc
                endif
            else
                ! Non-frozen soil; dry:
                if (meteo%nwet == 0) then
                    if (missing(lu_conf%rsoil)) then
                        rsoil_eff = -9999.
                    else
                        rsoil_eff = lu_conf%rsoil + rinc
                    endif
                ! Non-frozen soil; wet:
                elseif (meteo%nwet == 1) then
                    if (missing(comp%rsoil_wet)) then
                        rsoil_eff = -9999.
                    else
                        rsoil_eff = comp%rsoil_wet + rinc
                    endif
                else
                    call set_error(err, ERR_INPUT, &
                        'Programming error in rc_gsoil: nwet can only be 0 or 1')
                    call log_error('Programming error in rc_gsoil: nwet can only be 0 or 1')
                    return
                endif
            endif
        endif
        ! Compute conductance:
        if (rsoil_eff > 0.0) then
            dp_out%gsoil_eff = 1.0/rsoil_eff
        else
            ! No deposition path, or missing value:
            dp_out%gsoil_eff = 0.0
        endif
    end subroutine rc_gsoil

    subroutine rc_rinc(lu_conf, meteo, dp_conf, dp_out, rinc)
        type(depac_land_use), intent(in) :: lu_conf    ! current computed land-use type
        type(depac_meteorology), intent(in) :: meteo   ! meteorology
        type(depac_config), intent(in) :: dp_conf ! depac config
        type(depac_output), intent(inout) :: dp_out ! output of this run
        real, intent(out) :: rinc        ! in canopy resistance (s/m)

        ! Compute Rinc only for arable land, perm. crops, forest; otherwise Rinc = 0:
        if (lu_conf%rc_rinc%b > 0.0) then
            ! Check for u* > 0 (otherwise denominator = 0):
            if (meteo%ust > 0.0) then
                rinc = lu_conf%rc_rinc%b * lu_conf%rc_rinc%h * dp_conf%sai / meteo%ust
            else
                rinc = 1000.0
            endif
        else
            if (lu_conf%index == LU_GRASS .or. lu_conf%index == LU_OTHER) then
                rinc = -999.0 ! no deposition path for grass and other
            else
                rinc = 0.0   ! no in-canopy resistance
            endif
        endif
    end subroutine rc_rinc

end module m_rc_gsoil