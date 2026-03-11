module m_test_rc_eff
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_rc_eff, only: rc_eff
    use m_depac_error, only: set_error

    implicit none (type, external)
    private
    public :: collect_rc_eff_tests
    contains

    subroutine collect_rc_eff_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rc_eff", test_rc_eff) &
        ]

    end subroutine collect_rc_eff_tests

    subroutine test_rc_eff(error)
        type(error_type), allocatable, intent(out) :: error

        type(depac_component) :: comp
        type(depac_land_use) :: lu
        type(depac_meteorology) :: meteo
        type(depac_config) :: dp_conf
        type(depac_output) :: dp_out
        type(depac_error) :: dp_err
        logical :: ready

        call set_log_level(LOG_LEVEL_NONE)

        dp_conf%comp_point%c_nh3 = 10.0
        dp_out%ccomp_tot = 2.0

        dp_conf%ra_obs = 5.0
        dp_conf%rb = 3.0
        dp_out%rc_tot = 4.0

        call rc_eff(dp_out, dp_conf, dp_err)
        call check(error, dp_out%rc_eff, 7.0,&
             message="rc_eff normal case failed", thr=1.0e-5)
        if (allocated(error)) return

        ! comp_tot equals c_nh3 case
        dp_out%ccomp_tot = 10.0
        call rc_eff(dp_out, dp_conf, dp_err)
        call check(error, dp_out%rc_eff, -9999.0,&
             message="rc_eff equal comp_tot case failed", thr=1.0e-5)
        if (allocated(error)) return
    end subroutine test_rc_eff
end module m_test_rc_eff