module m_test_rc_special
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_test_rc_special_param, only: collect_rc_special_param_tests
    use m_helpers, only: missing
    use m_rc_special, only: rc_special
    use default_indices, only: COMP_HNO3, COMP_O3, COMP_NO, LU_WATER
    implicit none (type, external)
    private
    public :: collect_rc_special_tests
    contains

    subroutine collect_rc_special_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        call collect_rc_special_param_tests(testsuite)
        testsuite = [testsuite, &
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

        ! Initialize the structures with some default values

    end subroutine test_rc_special
end module m_test_rc_special