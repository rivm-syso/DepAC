program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type


    use m_test_rc_special, only: collect_rc_special_tests
    use m_test_rc_gw, only: collect_rc_gw_tests
    use m_test_rc_gstom, only: collect_rc_gstom_tests
    use m_test_rc_gsoil, only: collect_rc_gsoil_tests
    use m_test_rc_tot, only: collect_rc_tot_tests
    use m_test_comp_points, only: collect_comp_points_tests
    use m_test_rc_eff, only: collect_rc_eff_tests
    use m_test_version, only: collect_version_tests
    use m_test_depac, only: collect_depac_tests
    use m_test_ra, only: collect_calc_ra_tests
    use m_test_rb, only: collect_calc_rb_tests
    use m_test_helpers, only: collect_test_helpers_tests
    use m_test_loggers, only: collect_test_loggers_tests
    implicit none (type, external)

    ! Initialize test suites
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    integer :: stat, is





    stat = 0

    testsuites = [ &
        new_testsuite("version_tests", collect_version_tests), &
        new_testsuite("rc_special_tests", collect_rc_special_tests), &
        new_testsuite("rc_gw_tests", collect_rc_gw_tests), &
        new_testsuite("rc_gstom_tests", collect_rc_gstom_tests), &
        new_testsuite("rc_gsoil_tests", collect_rc_gsoil_tests), &
        new_testsuite("rc_tot_tests", collect_rc_tot_tests), &
        new_testsuite("comp_points_tests", collect_comp_points_tests), &
        new_testsuite("rc_eff_tests", collect_rc_eff_tests), &
        new_testsuite("depac_tests", collect_depac_tests), &
        new_testsuite("calc_ra_tests", collect_calc_ra_tests), &
        new_testsuite("calc_rb_tests", collect_calc_rb_tests), &
        new_testsuite("test_helpers_tests", collect_test_helpers_tests), &
        new_testsuite("test_loggers_tests", collect_test_loggers_tests) &
    ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
        error stop
    end if

end program tester
