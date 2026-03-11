program performance_depac
    use depac, only: depac_calc, depac_component, depac_land_use, &
                     depac_meteorology, depac_config, depac_output, depac_error, has_error, &
                     VERSION, BUILD_DATE, COMP_NH3, LU_GRASS, VERSION, BUILD_DATE, obtain_config



    implicit none (type, external)

    integer :: n_runs = 10000000
    real, allocatable :: rand_vals(:)


    integer :: i
    real :: rand_val
    real :: start_time, end_time

    type(depac_component) :: dp_comp
    type(depac_land_use) :: dp_lu
    type(depac_meteorology) :: dp_meteo
    type(depac_config) :: dp_conf
    type(depac_output) :: dp_out
    type(depac_error) :: dp_err


    allocate(rand_vals(n_runs))

    print *, "Basic DepAC example program"
    print *, "DepAC version:", VERSION
    print *, "DepAC build date:", BUILD_DATE

    ! Initialize component and land use with example values
    call obtain_config(LU_GRASS, COMP_NH3, dp_comp, dp_lu, dp_err)

    if (has_error(dp_err)) then
        print *, "Error obtaining configuration:", dp_err%message
        stop 1
    end if

    ! Set configuration parameters for this example
    dp_conf%lai = 3.0
    dp_conf%sai = 3.5
    dp_conf%calc_comp_points = .true.
    dp_conf%calc_effective_rc = .true.
    dp_conf%ra_obs = 100.0
    dp_conf%rb = 50.0


    ! Set meteorological conditions for this example


    dp_meteo%tsurf = 15.0
    dp_meteo%rh = 60.0
    dp_meteo%nwet = 0
    dp_meteo%glrad = 200.0
    dp_meteo%sinphi = 0.5
    dp_meteo%pres_0 = 101500.0

    ! set compenstation point for this component (example value)

    dp_conf%comp_point%c_ave_nh3 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_conf%comp_point%c_ave_so2 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_conf%comp_point%c_so2 = 1.2          ! Concentration in ug/m³
    dp_conf%comp_point%c_nh3 = 4.0          ! Concentration in ug/m³
    dp_conf%comp_point%iratns = 3

    call random_number(rand_vals)

    call cpu_time(start_time)


    do i = 1, n_runs
        dp_meteo%t = -10.0 + 40.0 * rand_vals(i)
        dp_meteo%tsurf = dp_meteo%t

        call depac_calc(dp_comp, dp_lu, dp_meteo, dp_conf, dp_out, dp_err)
        if (has_error(dp_err)) then
            print *, "Error during DepAC calculation:", dp_err%message
            stop 1
        end if
    end do
    call cpu_time(end_time)


    print *, "Completed", n_runs, "DepAC calculations."
    print *, "Total time taken (s):", end_time - start_time
    print *, "Average time per DepAC calculation (ns):", (end_time - start_time) / n_runs * 1.0e9
    ! We obtain a speed of 230 ns per calculation on a single CPU core
    ! Only when optimized with -O3 and -march=native, otherwise it is around 400 ns per calculation



    print *, "DepAC output:"
    print *, "Total canopy resistance (s/m):", dp_out%rc_tot
    print *, "Total canopy conductance (cm/s):", dp_out%gc_tot
    print *, "Compensation point (ug/m³):", dp_out%ccomp_tot

    print *, "Effective canopy resistance (s/m):", dp_out%rc_eff



end program performance_depac