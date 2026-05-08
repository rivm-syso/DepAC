!------------------------------------------------------------------------------
! Module:     m_check_depac_input
! Author:     Marte Voorneveld, RIVM
! Created:    November 14 2025
! Updated:    November 18 2025
! Description:
!   This module provides input validation routines for the DepAC atmospheric
!   deposition model. It checks required fields for components, land use,
!   configuration, location, and meteorology, setting errors for missing or
!   invalid values. This is useful to ensure that all necessary input data is
!   provided before running the model.
!------------------------------------------------------------------------------

module m_check_depac_input

    use m_depac_error, only: set_error
    use t_depac_error, only: ERR_INPUT, depac_error
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_config, only: depac_config
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_location, only: depac_location
    use m_logger, only: log_warn, log_error
    use m_helpers, only: missing
    implicit none (type, external)
    public
contains

    subroutine check_component_input(comp, dp_err)
        type(depac_component), intent(in) :: comp
        type(depac_error), intent(inout) :: dp_err

        ! make sure name is set
        if (trim(comp%name) == '') then
            call set_error(dp_err, ERR_INPUT, 'Component name is empty.')
            return
        end if







        ! now we check if all parameterisations are set

        if (.not. allocated(comp%gw_param)) then
            call set_error(dp_err, ERR_INPUT, 'gw_param is not allocated for component '//trim(comp%name))
            return
        end if

        if (.not. allocated(comp%gstom_param)) then
            call set_error(dp_err, ERR_INPUT, 'gstom_param is not allocated for component '//trim(comp%name))
            return
        end if
        if (.not. allocated(comp%comp_point_param)) then
            call set_error(dp_err, ERR_INPUT, 'comp_point_param is not allocated for component '//trim(comp%name))
            return
        end if
        if (.not. allocated(comp%rc_special)) then
            call set_error(dp_err, ERR_INPUT, 'rc_special is not allocated for component '//trim(comp%name))
            return
        end if




    end subroutine check_component_input

    subroutine check_land_use_input(lu, dp_err)
        type(depac_land_use), intent(in) :: lu
        type(depac_error), intent(inout) :: dp_err

        if (trim(lu%name) == '') then
            call set_error(dp_err, ERR_INPUT, 'Land use name is empty.')
            return
        end if

        if(.not. allocated(lu%gsoil_param)) then
            call set_error(dp_err, ERR_INPUT, 'gsoil_param is not allocated for land use '//trim(lu%name))
            return
        end if

        if(.not. allocated(lu%rc_rinc%rinc_param)) then
            call set_error(dp_err, ERR_INPUT, 'rc_rinc_param is not allocated for land use '//trim(lu%name))
            return
        end if

        if(.not. allocated(lu%stom_par%csoil_param)) then
            call set_error(dp_err, ERR_INPUT, 'csoil_param is not allocated for land use '//trim(lu%name))
            return
        end if

         ! now we check if all parameterisations are set

        ! all other parameters can be missing (-999.0) as
        ! they may not be needed for all components and are checked elsewhere

    end subroutine check_land_use_input

    subroutine check_depac_config(dp_conf, dp_err)
        type(depac_config), intent(in) :: dp_conf
        type(depac_error), intent(inout) :: dp_err

        ! lai and sai can be missing (-999.0) as they may not be needed
        !for all components and are checked elsewhere
        ! but we provide a warning here
        if (missing(dp_conf%lai)) then
            call log_warn('depac_config%lai is missing (-999.0). This may lead to incorrect ' // &
                'calculations for components that require LAI.')
        end if

        if (missing(dp_conf%sai)) then
            call log_warn('depac_config%sai is missing (-999.0). This may lead to incorrect ' // &
                'calculations for components that require SAI.')
        end if

        if(missing(dp_conf%rssnow)) then
            call set_error(dp_err, ERR_INPUT, 'rssnow is missing in depac_config.')
            return
        end if

        if(missing(dp_conf%sai_grass_haarweg)) then
            call set_error(dp_err, ERR_INPUT, 'sai_grass_haarweg is missing in depac_config.')
            return
        end if

        if(dp_conf%calc_effective_rc) then
            ! RA and Rb are required
            if(missing(dp_conf%ra_obs)) then
                call set_error(dp_err, ERR_INPUT,&
                 'ra is missing in depac_config while calc_effective_rc is true.')
                return
            end if
            if(missing(dp_conf%rb)) then
                call set_error(dp_err, ERR_INPUT,&
                 'rb is missing in depac_config while calc_effective_rc is true.')
                return
            end if
        end if

        if(dp_conf%calc_comp_points) then
            if(missing(dp_conf%comp_point%iratns)) then
                call set_error(dp_err, ERR_INPUT, &
                'comp_point%iratns is missing in depac_config while calc_comp_points is true.')
                return
            end if

            if(missing(dp_conf%comp_point%c_nh3)) then
                call set_error(dp_err, ERR_INPUT, &
                'comp_point%c_nh3 is missing in depac_config while calc_comp_points is true.')
                return
            end if

            if(missing(dp_conf%comp_point%c_so2)) then
                call set_error(dp_err, ERR_INPUT, &
                'comp_point%c_so2 is missing in depac_config while calc_comp_points is true.')
                return
            end if

            if(missing(dp_conf%comp_point%c_ave_nh3)) then
                call set_error(dp_err, ERR_INPUT, &
                'comp_point%c_ave_nh3 is missing in depac_config while calc_comp_points is true.')
                return
            end if

            if(missing(dp_conf%comp_point%c_ave_so2)) then
                call set_error(dp_err, ERR_INPUT, &
                'comp_point%c_ave_so2 is missing in depac_config while calc_comp_points is true.')
                return
            end if
        end if

        ! all other parameters can be missing (-999.0)
        ! as they may not be needed for all components and are checked elsewhere

    end subroutine check_depac_config



    subroutine check_meteorology_input(meteo, dp_err)
        type(depac_meteorology), intent(in) :: meteo
        type(depac_error), intent(inout) :: dp_err

        if(missing(meteo%t)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology temperature (t) is missing.')
            return
        end if

        if(missing(meteo%rh)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology relative humidity (rh) is missing.')
            return
        end if

        if(missing(meteo%glrad)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology global radiation (glrad) is missing.')
            return
        end if

        if(missing(meteo%pres_0)) then
            call set_error(dp_err, ERR_INPUT, &
                'Meteorology surface level pressure (pres_0) is missing.')
            return
        end if

        if(missing(meteo%tsurf)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology surface temp (tsurf) is missing.')
            return
        end if


        if(missing(meteo%sinphi)) then
            call set_error(dp_err, ERR_INPUT, &
                'Meteorology sine of solar elevation angle (sinphi) is missing.')
            return
        end if

        if(missing(meteo%nwet)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology nwet (wetness indicator) is missing.')
            return
        end if

    end subroutine check_meteorology_input

end module m_check_depac_input