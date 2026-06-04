program performance_depac
    use depac, only: depac_calc,  depac_context, depac_setup, &
        has_error, VERSION, BUILD_DATE

    use default_depac_config_rivm, only: default_depac_setup, &
        init_default_depac_config_rivm, &
        finalize_default_depac_config_rivm, &
        RIVM_LU_GRASS, RIVM_COMP_NH3


    implicit none (type, external)

    integer :: n_runs = 10000000
    real, allocatable :: rand_vals(:)


    integer :: i
    real :: rand_val
    real :: start_time, end_time

    type(depac_context) :: dp_ctx
    type(depac_setup) :: dp_setup

    allocate(rand_vals(n_runs))

    print *, "Basic DepAC example program"
    print *, "DepAC version:", VERSION
    print *, "DepAC build date:", BUILD_DATE

    call init_default_depac_config_rivm()
    dp_setup = default_depac_setup(RIVM_LU_GRASS, RIVM_COMP_NH3)


    ! Set configuration parameters for this example

    dp_setup%config%calc_comp_points = .true.
    dp_setup%config%calc_effective_rc = .true.


    dp_ctx%state%lai = 3.0
    dp_ctx%state%sai = 3.5
    dp_ctx%state%ra_obs = 100.0
    dp_ctx%state%rb = 50.0

    ! set compensation point
    dp_ctx%state%comp_point%c_ave_nh3 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_ctx%state%comp_point%c_ave_so2 = 1.0      ! Average concentration in ug/m³
                                            ! (analogous to concentration inside leafs)
    dp_ctx%state%comp_point%c_so2 = 1.2          ! Concentration in ug/m³
    dp_ctx%state%comp_point%c_nh3 = 4.0          ! Concentration in ug/m³
    dp_ctx%state%comp_point%iratns = 3


    ! Set meteorological conditions for this example


    dp_ctx%meteo%tsurf = 15.0
    dp_ctx%meteo%rh = 60.0
    dp_ctx%meteo%nwet = 0
    dp_ctx%meteo%glrad = 200.0
    dp_ctx%meteo%sinphi = 0.5
    dp_ctx%meteo%pres_0 = 101500.0

    ! set compenstation point for this component (example value)


    call random_number(rand_vals)

    call cpu_time(start_time)


    do i = 1, n_runs
        dp_ctx%meteo%t = -10.0 + 40.0 * rand_vals(i)
        dp_ctx%meteo%tsurf = dp_ctx%meteo%t

        call depac_calc(dp_setup, dp_ctx)
        if (has_error(dp_ctx%error)) then
            print *, "Error during DepAC calculation:", dp_ctx%error%message
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
    print *, "Total canopy resistance (s/m):", dp_ctx%output%rc_tot
    print *, "Total canopy conductance (cm/s):", dp_ctx%output%gc_tot
    print *, "Compensation point (ug/m³):", dp_ctx%output%ccomp_tot

    print *, "Effective canopy resistance (s/m):", dp_ctx%output%rc_eff

end program performance_depac