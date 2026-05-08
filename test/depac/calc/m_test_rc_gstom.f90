module m_test_rc_gstom
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use m_test_gstom_param, only: collect_gstom_param_tests
    ! use t_depac_component, only: depac_component
    ! use t_depac_land_use, only: depac_land_use
    ! use t_depac_meteorology, only: depac_meteorology
    ! use t_depac_config, only: depac_config
    ! use t_depac_output, only: depac_output
    ! use t_depac_error, only: depac_error, ERR_INPUT
    ! use m_logger, only: set_log_level, LOG_LEVEL_NONE
    ! use m_helpers, only: missing
    ! use m_rc_gstom, only: rc_gstom, rc_get_vpd, par_dir_diff, rc_gstom_emb
    ! use m_depac_error, only: set_error
    ! use default_indices, only: COMP_NO, COMP_NO2, COMP_O3, COMP_SO2, COMP_NH3
    implicit none (type, external)
    private
    public :: collect_rc_gstom_tests
    contains

    subroutine collect_rc_gstom_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        call collect_gstom_param_tests(testsuite)

        testsuite = [testsuite, &
            new_unittest("rc_gstom", test_rc_gstom) &
        ]


    end subroutine collect_rc_gstom_tests

    subroutine test_rc_gstom(error)
        type(error_type), allocatable, intent(out) :: error
        ! TODO implement test for the main rc_gstom function


    end subroutine test_rc_gstom


end module m_test_rc_gstom