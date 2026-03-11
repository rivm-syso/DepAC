module m_test_rc_snow
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_rc_gw, only: rc_gw, rw_so2, rw_nh3_sutton
    use m_depac_error, only: set_error
    use m_rc_snow, only: rc_snow
    use default_indices, only: COMP_NO, COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3

    implicit none (type, external)
    private
    public :: collect_rc_snow_tests
    contains
    subroutine collect_rc_snow_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_snow", test_rc_snow) &
        ]

    end subroutine collect_rc_snow_tests
    subroutine test_rc_snow(error)
        type(error_type), allocatable, intent(out) :: error


        type(depac_component) :: comp
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: err
        call set_log_level(LOG_LEVEL_NONE)


        ! Test 1: ipar_snow = 1, should return constant value
        comp%ipar_snow = 1
        dp_conf%rssnow = 150.
        call rc_snow(meteo, comp, dp_conf, dp_out, err)
        call check(error,dp_out%rc_tot, 150., 'ipar_snow=1 test failed', thr=1.0e-5)
        if (allocated(error)) return

        ! Test 2: ipar_snow = 2, t < -1, should return 500
        comp%ipar_snow = 2
        meteo%t = -2.0
        call rc_snow(meteo, comp, dp_conf, dp_out, err)
        call check(error, dp_out%rc_tot, 500., 'ipar_snow=2, t<-1 test failed', thr=1.0e-5)
        if (allocated(error)) return

        ! Test 3: ipar_snow = 2, t > 1, should return 70
        meteo%t = 2.0
        call rc_snow(meteo, comp, dp_conf, dp_out, err)
        call check(error, dp_out%rc_tot, 70., 'ipar_snow=2, t>1 test failed', thr=1.0e-5)
        if (allocated(error)) return

        ! Test 4: ipar_snow = 2, -1 <= t <= 1, should return 70*(2-t)
        meteo%t = 0.0
        call rc_snow(meteo, comp, dp_conf, dp_out, err)
        call check(error, dp_out%rc_tot, 140., 'ipar_snow=2, -1<=t<=1 test failed', thr=1.0e-5)
        if (allocated(error)) return

        ! test 5: invalid ipar_snow value, should set error
        comp%ipar_snow = 3
        call rc_snow(meteo, comp, dp_conf, dp_out, err)
        call check(error, err%code, ERR_INPUT, 'Invalid ipar_snow did not set error code')
        if (allocated(error)) return


    end subroutine test_rc_snow

end module m_test_rc_snow