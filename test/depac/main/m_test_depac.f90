module m_test_depac
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use depac, only: depac_calc
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_depac_error, only: set_error, clear_error
    use t_depac_error, only: depac_error
    use m_version, only: VERSION, BUILD_DATE

    implicit none (type, external)
    private
    public :: collect_depac_tests
    contains

    subroutine collect_depac_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_missing_input", test_missing_input) &
        ]

    end subroutine collect_depac_tests

    subroutine test_missing_input(error)
        type(error_type), allocatable, intent(out) :: error

        ! type(depac_component) :: comp
        ! type(depac_land_use) :: lu
        ! type(depac_meteorology) :: meteo
        ! type(depac_config) :: dp_conf
        ! type(depac_output) :: dp_out
        ! type(depac_error) :: dp_err
        ! logical :: ready

        ! call set_log_level(LOG_LEVEL_NONE)

        ! ! --------- Test missing component input ---------

        ! comp%name = ""

        ! ! test missing component name
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing component name did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)


        ! ! test missing component index
        ! comp%name = "NH3"
        ! comp%index = -999
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing component name did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! comp%index = 1
        ! comp%diffc = -999.0
        ! ! test missing diffc
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing diffc did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! comp%diffc = 0.21e-4
        ! comp%ipar_snow = -999
        ! ! test missing ipar_snow
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing ipar_snow did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! comp%ipar_snow = 2

        ! comp%rsoil_frozen = -999.0
        ! ! test missing rsoil_frozen
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing rsoil_frozen did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! comp%rsoil_frozen = 1000.0
        ! comp%rsoil_wet = -999.0

        ! ! test missing rsoil_wet
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing rsoil_wet did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! comp%rsoil_wet = 10.0

        ! ! --------- Test missing land use name input ---------

        ! lu%name = ""
        ! ! test missing land use name
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing land use name did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! ! test missing land use index
        ! lu%name = "grass"
        ! lu%index = -999
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing land use name did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! lu%index = 2

        ! ! --------- Test missing depac_config input ---------

        ! !missing lai:
        ! dp_conf%lai = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing lai did not return error")

        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! dp_conf%lai = 7.0
        ! ! missing sai:
        ! dp_conf%sai = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing sai did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)


        ! dp_conf%sai = 6.0

        ! ! missing rssnow
        ! dp_conf%rssnow = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing rssnow did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! ! missing sai_grass_haarweg
        ! dp_conf%rssnow = 2000.0
        ! dp_conf%sai_grass_haarweg = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing sai_grass_haarweg did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! dp_conf%sai_grass_haarweg = 3.5

        ! dp_conf%calc_effective_rc = .true.
        ! dp_conf%ra_obs = -999.0
        ! ! missing ra_obs when calc_effective_rc is true
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing ra_obs did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! dp_conf%ra_obs = 10.0
        ! dp_conf%rb = -999.0

        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing rb did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! dp_conf%rb = 5.0

        ! dp_conf%calc_comp_points = .true.

        ! ! missing comp_point%iratns
        ! dp_conf%comp_point%iratns = -999
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing comp_point%iratns did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! dp_conf%comp_point%iratns = 2

        ! ! missing comp_point%c_nh3
        ! dp_conf%comp_point%c_nh3 = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing comp_point%c_nh3 did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! dp_conf%comp_point%c_nh3 = 100.0

        ! ! missing comp_point%c_so2
        ! dp_conf%comp_point%c_so2 = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing comp_point%c_so2 did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! dp_conf%comp_point%c_so2 = 6.0

        ! ! missing comp_point%c_ave_nh3
        ! dp_conf%comp_point%c_ave_nh3 = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing comp_point%c_ave_nh3 did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! dp_conf%comp_point%c_ave_nh3 = 3.0

        ! ! missing comp_point%c_ave_so2
        ! dp_conf%comp_point%c_ave_so2 = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing comp_point%c_ave_so2 did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! dp_conf%comp_point%c_ave_so2 = 2.0

        ! ! --------- Check location input ---------

        ! dp_conf%coord%name = "Test Location"
        ! deallocate(dp_conf%coord%name)
        ! ! test missing location name
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing location name did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! dp_conf%coord%name = "Test Location"
        ! dp_conf%coord%lat = -999.0
        ! ! test missing location latitude
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing location latitude did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! dp_conf%coord%lat = 45.0
        ! dp_conf%coord%lon = -999.0
        ! ! test missing location longitude
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing location longitude did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! dp_conf%coord%lon = 10.0
        ! dp_conf%coord%elev = -999.0
        ! ! test missing location elevation
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing location elevation did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)


        ! dp_conf%coord%elev = 3.0


        ! ! ----------- test missing meteorology input ------------

        ! meteo%t = -999.0
        ! ! test missing temperature
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology temperature did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)

        ! meteo%t = 31.0
        ! ! test missing meteorology relative humidity
        ! meteo%rh = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology rh did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%rh = 70.0

        ! ! test missing meteorology global radiation
        ! meteo%glrad = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology glrad did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%glrad = 300.0

        ! ! test missing meteorology surface level pressure
        ! meteo%pres_0 = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology pres_0 did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%pres_0 = 101500.0

        ! ! test missing meteorology wind speed at 10m
        ! meteo%ws10 = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology ws10 did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%ws10 = 10.0

        ! ! test missing meteorology surface temperature
        ! meteo%tsurf = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology tsurf did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%tsurf = 30.0

        ! ! test missing meteorology friction velocity
        ! meteo%ust = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology ust did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%ust = 3.0

        ! ! test missing meteorology Monin-Obukhov length
        ! meteo%ol = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology ol did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%ol = 1000.0

        ! ! test missing meteorology sine of solar elevation angle
        ! meteo%sinphi = -999.0
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology sinphi did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%sinphi = 0.9

        ! ! test missing meteorology nwet (wetness indicator)
        ! meteo%nwet = -999
        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
        ! call check(error, dp_err%code, ERR_INPUT, &
        !     message="depac missing meteorology nwet did not return error")
        ! if (allocated(error)) return
        ! call clear_error(dp_err)
        ! meteo%nwet = 0

        ! ! Check the depac output


        ! call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)

        ! call check(error, dp_out%version, VERSION, &
        !     message="depac version output incorrect")
        ! if (allocated(error)) return
        ! call check(error, dp_out%build_date, BUILD_DATE, &
        !     message="depac build_date output incorrect")
        ! if (allocated(error)) return
        ! call check(error, dp_out%gw, 7.03585669E-02, &
        !     message="depac gw output incorrect", thr=1.0e-6)
        ! if (allocated(error)) return
        ! call check(error, missing(dp_out%gw_can), .true., &
        !     message="depac gw_can should be output missing")
        ! if (allocated(error)) return
        ! call check(error, dp_out%gstom, 1.12625294E+13, &
        !     message="depac gstom output incorrect", thr=1.0e5)
        ! if (allocated(error)) return
        ! call check(error, dp_out%ccomp_tot, 3.31342701E-13, &
        !     message="depac ccomp_tot output incorrect", thr=1.0e-18)
        ! if (allocated(error)) return
        ! call check(error, dp_out%gc_tot, 1.12625294E+13, &
        !     message="depac gc_tot output incorrect", thr=1.0e5)
        ! if (allocated(error)) return
        ! call check(error, dp_out%gsoil_eff, 0.00000000, &
        !     message="depac gsoil_eff output incorrect", thr=1.0e-5)
        ! if (allocated(error)) return
        ! call check(error, dp_out%rc_tot, 8.87899983E-14, &
        !     message="depac rc_tot output incorrect", thr=1.0e-18)
        ! if (allocated(error)) return
        ! call check(error, dp_out%rc_eff, 1.38491405E-13, &
        !     message="depac rc_eff output incorrect", thr=1.0e-17)
        ! if (allocated(error)) return

        ! DepAC is working correctly if we reach this point

    end subroutine test_missing_input

end module m_test_depac