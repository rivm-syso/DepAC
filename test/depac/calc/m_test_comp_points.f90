module m_test_comp_points
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use t_depac_component, only: depac_component
    use t_depac_land_use, only: depac_land_use
    use t_depac_meteorology, only: depac_meteorology
    use t_depac_config, only: depac_config
    use t_depac_output, only: depac_output
    use t_depac_error, only: depac_error, ERR_INPUT
    use m_logger, only: set_log_level, LOG_LEVEL_NONE
    use m_helpers, only: missing
    use m_depac_error, only: set_error, has_error
    use m_comp_points, only: rc_comp_point
    use default_indices, only: LU_GRASS, LU_OTHER, COMP_HNO3, &
     COMP_NH3, COMP_O3, COMP_SO2, COMP_NO2, COMP_NO, LU_WATER

    implicit none (type, external)
    private
    public :: collect_comp_points_tests
    contains

    
    subroutine collect_comp_points_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        ! test the compensation point parameterisations 
        ! call collect_comp_point_param_tests(testsuite)


        
        testsuite = [testsuite, &
            new_unittest("rc_comp_point", test_rc_comp_point) &
        ]

    end subroutine collect_comp_points_tests

    subroutine test_rc_comp_point(error)
        type(error_type), allocatable, intent(out) :: error

        ! TODO Write a test testing the general function rc_comp_point


    end subroutine test_rc_comp_point

end module m_test_comp_points