!------------------------------------------------------------------------------
! Module:     m_rc_special
! Authors:    Marte Voorneveld, RIVM
!             Addo van Pul, Jan Willem Erisman,
!             Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:    November 14 2025
! Updated:    November 20 2025
! Description:
!   This module provides routines for calculating special resistance values
!   (rc_special) for atmospheric deposition modeling components. The main
!   subroutine, rc_special, handles special cases for snow surfaces and
!   specific components such as HNO3, NO, NO2, O3, SO2, and NH3, using
!   meteorological, land use, and component parameters. Results are used in
!   dry deposition calculations and model output.
!------------------------------------------------------------------------------
module m_rc_special
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    ! modules
    use m_depac_error, only: set_error
    use t_depac_error, only: ERR_INPUT, depac_error
    use m_logger, only: log_info, log_error
    use m_rc_snow,  only: rc_snow
    use default_indices, only: LU_WATER, COMP_HNO3, COMP_NO, COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3

    implicit none (type, external)
    private
    public :: rc_special
    contains
    subroutine rc_special(comp, lu, meteo, dp_conf, dp_out, ready, err)
        type(depac_component), intent(in) :: comp      ! current computed component
        type(depac_land_use), intent(in)  :: lu        ! current computed land-use type
        type(depac_meteorology), intent(in) :: meteo     ! meteorology
        type(depac_config), intent(in) :: dp_conf  ! depac config
        type(depac_output), intent(inout)  :: dp_out ! output of this run
        logical, intent(out) :: ready               ! readiness flag
        type(depac_error), intent(inout) :: err ! error handling
        ready = .false.
        select case(comp%index)

        case(COMP_HNO3)
        ! No separate resistances for HNO3; just one total canopy resistance:
        if (meteo%t < -5.0 .and. meteo%nwet == 9) then
            ! T < 5 C and snow:
            dp_out%rc_tot = 50.
        else
            ! all other circumstances:
            dp_out%rc_tot = 10.0
        endif
        ready = .true.

        case(COMP_NO)
            if (lu%index == LU_WATER) then ! water
                dp_out%rc_tot = 2000.
                ready = .true.
            elseif (meteo%nwet == 1) then ! wet
                dp_out%rc_tot = 2000.
                ready = .true.

            elseif (meteo%nwet == 9) then ! snow
                call log_info("Calculating special rc for NO on snow surface.")
                call rc_snow(meteo, comp, dp_conf, dp_out, err)
                ready = .true.
            endif

        case(COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3)
        ! snow surface:
        if (meteo%nwet== 9) then
            call log_info("Calculating special rc for "//trim(comp%name)//" on snow surface.")

            call rc_snow(meteo, comp, dp_conf, dp_out, err)
            ready = .true.
        endif

        case default
        call set_error(err, ERR_INPUT, &
          "Component "//trim(lu%name)//" not supported in rc_special.")
        call log_error("Component "//trim(lu%name)//" not supported in rc_special.")
        return
        end select
    end subroutine rc_special

end module m_rc_special