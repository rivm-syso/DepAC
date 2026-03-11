module m_test_loggers
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use t_depac_meteorology, only: depac_meteorology
    use m_logger, only: set_log_level, LOG_LEVEL_DEBUG, LOG_LEVEL_INFO, LOG_LEVEL_WARN, &
        LOG_LEVEL_ERROR, log_debug, log_info, log_warn, log_error


    implicit none (type, external)
    private
    public :: collect_test_loggers_tests
    contains
    subroutine collect_test_loggers_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("loggers", test_log_levels) &
        ]

    end subroutine collect_test_loggers_tests

    subroutine test_log_levels(error)
        type(error_type), allocatable, intent(out) :: error

        call set_log_level(LOG_LEVEL_DEBUG)
        call log_debug("This is a debug message")
        call set_log_level(LOG_LEVEL_INFO)
        call log_info("This is an info message")
        call set_log_level(LOG_LEVEL_WARN)
        call log_warn("This is a warning message")
        call set_log_level(LOG_LEVEL_ERROR)
        call log_error("This is an error message")


    end subroutine test_log_levels


end module m_test_loggers