module m_test_rc_special
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_rc_special, only: rc_special
    use default_indices, only: COMP_HNO3, COMP_O3, COMP_NO, LU_WATER
    implicit none (type, external)
    private
    public :: collect_rc_special_tests
    contains

    subroutine collect_rc_special_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_special", test_rc_special) &
        ]

    end subroutine collect_rc_special_tests



    subroutine test_rc_special(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_component) :: comp
        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready

        call set_log_level(LOG_LEVEL_NONE)

        comp%name = "HNO3"
        comp%index = COMP_HNO3
        meteo%t = -6.0
        meteo%nwet = 9

        call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, dp_err)

        call check(error, dp_out%rc_tot, 50., &
            message="rc_special with t <-5  and nwet=9 failed", thr=1.0e-5)
        if (allocated(error)) return


        meteo%t = -4.0
        meteo%nwet = 1

        call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, dp_err)
        call check(error, dp_out%rc_tot, 10., &
            message="rc_special with t > -5  and nwet=1 failed", thr=1.0e-5)
        if (allocated(error)) return



        comp%name = "NO"
        comp%index = COMP_NO
        lu%name = "water"
        lu%index = LU_WATER

        call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, dp_err)
        call check(error, dp_out%rc_tot, 2000., &
            message="rc_special with lu = water failed", thr=1.0e-5)
        if (allocated(error)) return
        call check(error, ready, .true., message="rc_special with lu = water failed")
        if (allocated(error)) return

        lu%name = "land"
        meteo%nwet = 1

        call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, dp_err)
        call check(error, dp_out%rc_tot, 2000., &
        message="rc_special with lu = land and nwet=1 failed", thr=1.0e-5)
        if (allocated(error)) return
        call check(error, ready, .true., message="rc_special with lu = land and nwet=1 failed")
        if (allocated(error)) return

        meteo%nwet = 9

        call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, dp_err)
        call check(error, dp_out%rc_tot, 2000., &
            message="rc_special with lu = land and nwet=9 failed", thr=1.0e-5)
        if (allocated(error)) return


        comp%name = "O3"
        comp%index = COMP_O3
        comp%ipar_snow = 1

        call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, dp_err)
        call check(error, dp_out%rc_tot, 2000.,&
            message="rc_special with lu = land and nwet=9 failed", thr=1.0e-5)
        if (allocated(error)) return
        call check(error, ready, .true., message="rc_special with lu = land and nwet=9 failed")
        if (allocated(error)) return


        ! non supported component
        comp%name = "XYZ"
        comp%index = 999

        call rc_special(comp, lu, meteo, dp_conf, dp_out, ready, dp_err)
        call check(error, dp_err%code, ERR_INPUT, &
            message="rc_special with unsupported component failed")
        if (allocated(error)) return



    end subroutine test_rc_special
end module m_test_rc_special