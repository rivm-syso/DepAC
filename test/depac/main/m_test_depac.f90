module m_test_depac
   use testdrive, only : new_unittest, unittest_type, error_type, check
   use depac, only: depac_calc

   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   use m_logger, only: set_log_level, LOG_LEVEL_NONE
   use m_helpers, only: missing
   use m_depac_error, only: set_error, clear_error
   use t_depac_error_core, only: ERR_NONE, ERR_INPUT
   use m_version, only: VERSION, BUILD_DATE

   use t_depac_config_core, only: depac_config_core

   use m_depac_params, only: gw_nh3_sutton,comp_point_default, csoil_default, gw_default,&
      gstom_default, gsoil_default, rinc_no_path, rc_special_default


   ! default configuration
   use default_depac_config_rivm, only: init_default_depac_config_rivm,&
      finalize_default_depac_config_rivm, default_depac_setup

   implicit none (type, external)
   private
   public :: collect_depac_tests
contains

   subroutine collect_depac_tests(testsuite)
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
         new_unittest("test_missing_input", test_missing_input), &
         new_unittest("test_depac_calc", test_depac_calc) &
         ]

   end subroutine collect_depac_tests

   subroutine test_depac_calc(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx
      type(depac_config_core) :: tmp_config
      logical :: ready
      integer :: i, j


      real, dimension(6,9), parameter :: expected_rc_eff = reshape([ &
         119.335388, 58.3976135, 53.3102913, 60.1540413, -9999.00000, -999.000000, &
         123.168701, 66.0943069, 46.6053505, 68.3531799, 428.000000, -999.000000, &
         123.168701, 66.0943069, 55.4214401, 68.3531799, 1028.00000, -999.000000, &
         178.033310, 204.583099, 128.807205, 216.112320, 1560.00000, -999.000000, &
         171.734299, 163.854858, 120.202988, 213.775787, 1560.00000, -999.000000, &
         5.85804029E-13, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, -999.000000, -999.000000, &
         5.85804029E-13, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 100.000000, -999.000000, &
         119.335388, 58.3976135, 53.3102913, 60.1540413, -9999.00000, -999.000000, &
         5.85804029E-13, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 999.999939, -999.000000 &
         ], [6,9])

      real, dimension(6,9), parameter :: expected_rc_tot = reshape([ &
         10.3729048, 58.3976135, 53.3102951, 60.1540413, -9999.00000, 10.0000000, &
         10.6206102, 66.0943069, 46.6053505, 68.3531799, 428.000000, 10.0000000, &
         10.6206102, 66.0943069, 55.4214401, 68.3531799, 1028.00000, 10.0000000, &
         13.3631525, 204.583099, 128.807205, 216.112320, 1560.00000, 10.0000000, &
         13.0759459, 163.854858, 120.202988, 213.775787, 1560.00000, 10.0000000, &
         8.87899983E-14, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 2000.00000, 10.0000000, &
         8.87899983E-14, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 100.000000, 10.0000000, &
         10.3729048, 58.3976135, 53.3102951, 60.1540413, -9999.00000, 10.0000000, &
         8.87899983E-14, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 999.999939, 10.0000000 &
         ], [6,9])


      call init_default_depac_config_rivm()
      call set_log_level(LOG_LEVEL_NONE)
      setup%config%lai = 7.0
      setup%config%sai = 6.0
      setup%config%rssnow = 2000.0
      setup%config%sai_grass_haarweg = 3.5

      ctx%meteo%t = 31.0
      ctx%meteo%rh = 70.0
      ctx%meteo%glrad = 300.0
      ctx%meteo%pres_0 = 101500.0
      ctx%meteo%tsurf = 30.0
      ctx%meteo%ust = 3.0
      ctx%meteo%ol = 1000.0
      ctx%meteo%sinphi = 0.9
      ctx%meteo%nwet = 0


      setup%config%calc_comp_points = .true.

      setup%config%comp_point%iratns = 2
      setup%config%comp_point%c_nh3 = 100.0
      setup%config%comp_point%c_so2 = 6.0
      setup%config%comp_point%c_ave_nh3 = 3.0
      setup%config%comp_point%c_ave_so2 = 2.0

      setup%config%calc_effective_rc = .true.

      setup%config%ra_obs = 100.0
      setup%config%rb = 50.0

      tmp_config = setup%config

      do i = 1, 9
         do j = 1, 6
            setup = default_depac_setup(i, j)
            setup%config = tmp_config
            call clear_error(ctx%error)
            call depac_calc(setup, ctx)

            if(ctx%error%code /= ERR_NONE) then
               return
            end if
            call check(error, ctx%output%rc_eff, expected_rc_eff(j,i), &
               message="depac_calc did not return expected rc_eff for component "&
               //trim(setup%component%name)// &
               " and land use "//trim(setup%land_use%name), thr=1.0e-5)
            if (allocated(error)) then
               return
            end if

            call check(error, ctx%output%rc_tot, expected_rc_tot(j,i), &
               message="depac_calc did not return expected rc_tot for component "&
               //trim(setup%component%name)// &
               " and land use "//trim(setup%land_use%name), thr=1.0e-5)
            if (allocated(error)) then
               return
            end if
         end do
      end do

      call finalize_default_depac_config_rivm()
   end subroutine test_depac_calc

   subroutine test_missing_input(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx
      type(depac_config_core) :: tmp_config
      logical :: ready
      integer :: i, j

      real :: st

      call clear_error(ctx%error)
      call set_log_level(LOG_LEVEL_NONE)

      ! ! --------- Test missing component input ---------

      setup%component%name = "NH3"
      setup%component%diffc = 0.21e-4
      setup%component%ipar_snow = 1
      setup%component%rsoil_frozen = 1000.0
      setup%component%rsoil_wet = 10.0

      setup%land_use%name = "grass"
      setup%config%lai = 7.0
      setup%config%sai = 6.0
      setup%config%rssnow = 2000.0
      setup%config%sai_grass_haarweg = 3.5

      ctx%meteo%t = 31.0
      ctx%meteo%rh = 70.0
      ctx%meteo%glrad = 300.0
      ctx%meteo%pres_0 = 101500.0
      ctx%meteo%tsurf = 30.0
      ctx%meteo%ust = 3.0
      ctx%meteo%ol = 1000.0
      ctx%meteo%sinphi = 0.9
      ctx%meteo%nwet = 0


      allocate(setup%gw_param, source=gw_default())
      allocate(setup%gstom_param, source=gstom_default())
      allocate(setup%comp_point_param, source=comp_point_default())
      allocate(setup%rc_special_param, source=rc_special_default())
      allocate(setup%gsoil_param, source=gsoil_default())
      allocate(setup%rinc_param, source=rinc_no_path())
      allocate(setup%csoil_param, source=csoil_default())



      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_NONE, &
         message="depac returned error when all inputs were set correctly")

      print *, "Testing missing input parameters for depac_calc..."
      print *, ctx%error%message
      if (allocated(error)) return
      call clear_error(ctx%error)

      ! test missing component name
      setup%component%name = ""
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing component name did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)

      setup%component%name = "NH3"






      ! ! --------- Test missing land use name input ---------

      setup%land_use%name = ""
      ! test missing land use name
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing land use name did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)

      ! test missing land use index
      setup%land_use%name = "grass"


      ! ! --------- Test missing depac_config input ---------
      call set_log_level(LOG_LEVEL_NONE)
      !missing lai:
      setup%config%lai = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_NONE, &
         message="depac missing lai returned error, but it should just be a warning")

      if (allocated(error)) return
      call clear_error(ctx%error)
      call set_log_level(LOG_LEVEL_NONE)
      setup%config%lai = 7.0
      ! missing sai:
      setup%config%sai = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_NONE, &
         message="depac missing sai returned error, but it should just be a warning")
      if (allocated(error)) return
      call clear_error(ctx%error)


      setup%config%sai = 6.0

      ! missing rssnow
      setup%config%rssnow = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing rssnow did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)

      ! missing sai_grass_haarweg
      setup%config%rssnow = 2000.0
      setup%config%sai_grass_haarweg = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing sai_grass_haarweg did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)

      setup%config%sai_grass_haarweg = 3.5



      ! Check meteo


      ctx%meteo%t = -999.0
      ! test missing temperature
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing meteorology temperature did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)

      ctx%meteo%t = 31.0
      ! test missing meteorology relative humidity
      ctx%meteo%rh = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing meteorology rh did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      ctx%meteo%rh = 70.0

      ! test missing meteorology global radiation
      ctx%meteo%glrad = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing meteorology glrad did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      ctx%meteo%glrad = 300.0

      ! test missing meteorology surface level pressure
      ctx%meteo%pres_0 = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing meteorology pres_0 did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      ctx%meteo%pres_0 = 101500.0


      ! test missing meteorology surface temperature
      ctx%meteo%tsurf = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing meteorology tsurf did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      ctx%meteo%tsurf = 30.0


      ! test missing meteorology sine of solar elevation angle
      ctx%meteo%sinphi = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing meteorology sinphi did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      ctx%meteo%sinphi = 0.9

      ! test missing meteorology nwet (wetness indicator)
      ctx%meteo%nwet = -999
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing meteorology nwet did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      ctx%meteo%nwet = 0



      setup%config%calc_comp_points = .true.

      setup%config%comp_point%iratns = 2
      setup%config%comp_point%c_nh3 = 100.0
      setup%config%comp_point%c_so2 = 6.0
      setup%config%comp_point%c_ave_nh3 = 3.0
      setup%config%comp_point%c_ave_so2 = 2.0

      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_NONE, &
         message="depac returned error when all comp_point parameters were set correctly")
      print *, ctx%error%message
      if (allocated(error)) return

      call clear_error(ctx%error)

      ! missing comp_point%iratns
      setup%config%comp_point%iratns = -999
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing comp_point%iratns did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      setup%config%comp_point%iratns = 2

      ! missing comp_point%c_nh3
      setup%config%comp_point%c_nh3 = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing comp_point%c_nh3 did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      setup%config%comp_point%c_nh3 = 100.0

      ! missing comp_point%c_so2
      setup%config%comp_point%c_so2 = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing comp_point%c_so2 did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      setup%config%comp_point%c_so2 = 6.0

      ! missing comp_point%c_ave_nh3
      setup%config%comp_point%c_ave_nh3 = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing comp_point%c_ave_nh3 did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      setup%config%comp_point%c_ave_nh3 = 3.0

      ! missing comp_point%c_ave_so2
      setup%config%comp_point%c_ave_so2 = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing comp_point%c_ave_so2 did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      setup%config%comp_point%c_ave_so2 = 2.0

      setup%config%calc_effective_rc = .true.

      setup%config%ra_obs = 100.0
      setup%config%rb = 50.0

      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_NONE, &
         message="depac returned error when all effective rc parameters were set correctly")
      if (allocated(error)) return
      call clear_error(ctx%error)

      ! missing ra_obs
      setup%config%ra_obs = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing ra_obs did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      setup%config%ra_obs = 100.0

      ! missing rb
      setup%config%rb = -999.0
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing rb did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      setup%config%rb = 50.0

      deallocate(setup%gsoil_param)
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing gsoil_param did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      allocate(setup%gsoil_param, source=gsoil_default())

      deallocate(setup%csoil_param)
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing csoil_param did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      allocate(setup%csoil_param, source=csoil_default())

      deallocate(setup%rinc_param)
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing rinc_param did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      allocate(setup%rinc_param, source=rinc_no_path())

      deallocate(setup%gw_param)
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing gw_param did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      allocate(setup%gw_param, source=gw_default())

      deallocate(setup%gstom_param)
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing gstom_param did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      allocate(setup%gstom_param, source=gstom_default())

      deallocate(setup%comp_point_param)
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing comp_point_param did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      allocate(setup%comp_point_param, source=comp_point_default())

      deallocate(setup%rc_special_param)
      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_INPUT, &
         message="depac missing rc_special_param did not return error")
      if (allocated(error)) return
      call clear_error(ctx%error)
      allocate(setup%rc_special_param, source=rc_special_default())





      ! Check the depac output

      call init_default_depac_config_rivm()

      tmp_config = setup%config
      setup = default_depac_setup(1,1)
      setup%config = tmp_config


      call depac_calc(setup, ctx)
      call check(error, ctx%error%code, ERR_NONE, &
         message="depac returned error when all inputs were set correctly for output check")
      if (allocated(error)) return


      call check(error, ctx%output%version, VERSION, &
         message="depac version output incorrect")
      if (allocated(error)) return
      call check(error, ctx%output%build_date, BUILD_DATE, &
         message="depac build_date output incorrect")
      if (allocated(error)) return

      call check(error, ctx%output%gw, 7.03585669E-02, &
         message="depac gw output incorrect", thr=1.0e-8)
      if (allocated(error)) return
      call check(error, missing(ctx%output%gw_can), .true., &
         message="depac gw_can should be output missing")
      if (allocated(error)) return

      call check(error, ctx%output%gstom, 2.60464419E-02, &
         message="depac gstom output incorrect", thr=1.0e-8)
      if (allocated(error)) return

      call check(error, ctx%output%gsoil_eff, 0.00000000, &
         message="depac gsoil_eff output incorrect", thr=1.0e-8)
      if (allocated(error)) return

      call check(error, ctx%output%ccomp_tot, 40.4560585, &
         message="depac ccomp_tot output incorrect", thr=1.0e-6)
      if (allocated(error)) return

      call check(error, ctx%output%gc_tot, 9.64050069E-02, &
         message="depac gc_tot output incorrect", thr=1.0e-6)
      if (allocated(error)) return


      call check(error, ctx%output%rc_tot, 10.3729048, &
         message="depac rc_tot output incorrect", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, ctx%output%rc_eff, 119.335388, &
         message="depac rc_eff output incorrect", thr=1.0e-6)
      if (allocated(error)) return



      ! test another case for 3,3 SO2, permanent crops, which has different parameterisations

      setup = default_depac_setup(3,3)
      setup%config = tmp_config


      call depac_calc(setup, ctx)

      call check(error, ctx%output%gw, 5.11472952E-03, &
         message="depac gw output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return

      call check(error, ctx%output%gstom, 1.19560668E-02, &
         message="depac gstom output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, ctx%output%gsoil_eff, 9.72762646E-04, &
         message="depac gsoil_eff output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, ctx%output%rc_tot, 55.4214401, &
         message="depac rc_tot output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, ctx%output%rc_eff, 55.4214401, &
         message="depac rc_eff output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return

      call finalize_default_depac_config_rivm()

   end subroutine test_missing_input

end module m_test_depac
