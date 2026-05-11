module m_test_ra
    use testdrive, only : new_unittest, unittest_type, error_type, check

    use m_ra, only: depac_calc_ra, depac_calc_ra_obs_h

    use c_depac_core, only: depac_meteorology_core


    implicit none (type, external)
    private
    public :: collect_calc_ra_tests
    contains

    subroutine collect_calc_ra_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("calc_ra", test_calc_ra) &
        ]

    end subroutine collect_calc_ra_tests

    subroutine test_calc_ra(error)
        type(error_type), allocatable, intent(out) :: error
        ! internal variables
        real :: ra

        type(depac_meteorology_core) :: meteo
        real :: obs_h

        meteo%ust = -0.1
        meteo%ws10 = 3.0

        ra = depac_calc_ra(meteo)
        call check(error, ra, -999.0,&
             message="ra calculation with invalid ust failed", thr=1.0e-5)

        meteo%ust = 3.0
        meteo%ws10 = -1.0
        ra = depac_calc_ra(meteo)
        call check(error, ra, -999.0,&
             message="ra calculation with invalid ws10 failed", thr=1.0e-5)


        meteo%ust = 0.5
        meteo%ws10 = 3.0

        ra = depac_calc_ra(meteo)

        call check(error, ra, 12.0,&
             message="ra calculation failed", thr=1.0e-5)

        ra = -999.0
        obs_h = 10.0

        meteo%ust = -0.1
        ra = depac_calc_ra_obs_h(meteo, obs_h)
        call check(error, ra, -999.0,&
             message="ra_obs_h calculation with invalid ust failed", thr=1.0e-5)

        meteo%ust = 3.0
        meteo%z0 = -0.01
        ra = depac_calc_ra_obs_h(meteo, obs_h)
        call check(error, ra, -999.0,&
             message="ra_obs_h calculation with invalid ust failed", thr=1.0e-5)

        meteo%z0 = 0.1

        ra = depac_calc_ra_obs_h(meteo, obs_h)
        call check(error, ra, 3.77526045,&
             message="ra_obs_h calculation failed", thr=1.0e-5)

    end subroutine test_calc_ra
end module m_test_ra