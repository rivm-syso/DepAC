module m_test_depac
   use testdrive, only : new_unittest, unittest_type, error_type, check
   use depac, only: depac_calc
   use t_depac_component, only: depac_component
   use t_depac_land_use, only: depac_land_use
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use t_depac_output, only: depac_output
   use t_depac_error, only: ERR_INPUT, ERR_NONE
   use m_logger, only: set_log_level, LOG_LEVEL_NONE
   use m_helpers, only: missing
   use m_depac_error, only: set_error, clear_error
   use t_depac_error, only: depac_error
   use m_version, only: VERSION, BUILD_DATE

   ! parameterisation
   use m_comp_point_param, only: comp_point_default, csoil_default
   use m_gw_param, only: gw_default
   use m_gstom_param, only: gstom_default
   use m_gsoil_param, only: gsoil_default, rinc_no_path
   use m_rc_special_param, only: rc_special_default

   ! default configuration
   use default_depac_config_rivm, only: default_land_use_matrix, &
    default_component_matrix, init_default_depac_config_rivm

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
      type(depac_component) :: comp
      type(depac_land_use) :: lu
      type(depac_meteorology) :: meteo
      type(depac_config) :: dp_conf
      type(depac_output) :: dp_out
      type(depac_error) :: dp_err
      logical :: ready
      integer :: i, j

      real, dimension(6,9), parameter :: expected_rc_eff = reshape([ &
         119.335388, 47.0838165, 73.2956467, 60.1540413, -9999.00000, -999.000000, &
         123.168701, 51.9625587, 61.1918983, 68.3531799, 428.000000, -999.000000, &
         123.168701, 51.9625587, 77.3465195, 68.3531799, 1028.00000, -999.000000, &
         178.033310, 111.077545, 377.527802, 216.112320, 1560.00000, -999.000000, &
         171.734299, 97.8694611, 312.058136, 213.775787, 1560.00000, -999.000000, &
         5.85804029E-13, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, -999.000000, -999.000000, &
         5.85804029E-13, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 100.000000, -999.000000, &
         119.335388, 47.0838165, 73.2956467, 60.1540413, -9999.00000, -999.000000, &
         5.85804029E-13, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 999.999939, -999.000000 &
         ], [6,9])

      real, dimension(6,9), parameter :: expected_rc_tot = reshape([ &
         10.3729048, 47.0838203, 73.2956467, 60.1540413, -9999.00000, 10.0000000, &
         10.6206102, 51.9625587, 61.1918983, 68.3531799, 428.000000, 10.0000000, &
         10.6206102, 51.9625587, 77.3465195, 68.3531799, 1028.00000, 10.0000000, &
         13.3631525, 111.077553, 377.527832, 216.112320, 1560.00000, 10.0000000, &
         13.0759459, 97.8694611, 312.058136, 213.775787, 1560.00000, 10.0000000, &
         8.87899983E-14, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 2000.00000, 10.0000000, &
         8.87899983E-14, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 100.000000, 10.0000000, &
         10.3729048, 47.0838203, 73.2956467, 60.1540413, -9999.00000, 10.0000000, &
         8.87899983E-14, 1.43430027E-13, 1.69508193E-13, 1.43430027E-13, 999.999939, 10.0000000 &
         ], [6,9])
      dp_conf%lai = 7.0
      dp_conf%sai = 6.0
      dp_conf%rssnow = 2000.0
      dp_conf%sai_grass_haarweg = 3.5

      meteo%t = 31.0
      meteo%rh = 70.0
      meteo%glrad = 300.0
      meteo%pres_0 = 101500.0
      meteo%tsurf = 30.0
      meteo%ust = 3.0
      meteo%ol = 1000.0
      meteo%sinphi = 0.9
      meteo%nwet = 0


      dp_conf%calc_comp_points = .true.

      dp_conf%comp_point%iratns = 2
      dp_conf%comp_point%c_nh3 = 100.0
      dp_conf%comp_point%c_so2 = 6.0
      dp_conf%comp_point%c_ave_nh3 = 3.0
      dp_conf%comp_point%c_ave_so2 = 2.0

      dp_conf%calc_effective_rc = .true.

      dp_conf%ra_obs = 100.0
      dp_conf%rb = 50.0


      do i = 1, 9
         do j = 1, 6
            lu = default_land_use_matrix(i,j)
            comp = default_component_matrix(i,j)
            call clear_error(dp_err)
            call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)

            if(dp_err%code /= ERR_NONE) then
               return
            end if
            call check(error, dp_out%rc_eff, expected_rc_eff(j,i), &
               message="depac_calc did not return expected rc_eff for component "&
               //trim(comp%name)// &
               " and land use "//trim(lu%name), thr=1.0e-5)
            if (allocated(error)) then
               return
            end if

            call check(error, dp_out%rc_tot, expected_rc_tot(j,i), &
               message="depac_calc did not return expected rc_tot for component "&
               //trim(comp%name)// &
               " and land use "//trim(lu%name), thr=1.0e-5)
            if (allocated(error)) then
               return
            end if
         end do
      end do
   end subroutine test_depac_calc

   subroutine test_missing_input(error)
      type(error_type), allocatable, intent(out) :: error

      type(depac_component) :: comp
      type(depac_land_use) :: lu
      type(depac_meteorology) :: meteo
      type(depac_config) :: dp_conf
      type(depac_output) :: dp_out
      type(depac_error) :: dp_err
      logical :: ready
      integer :: i, j

      call clear_error(dp_err)
      call set_log_level(LOG_LEVEL_NONE)

      ! ! --------- Test missing component input ---------

      comp%name = "NH3"
      comp%diffc = 0.21e-4
      comp%ipar_snow = 1
      comp%rsoil_frozen = 1000.0
      comp%rsoil_wet = 10.0

      lu%name = "grass"
      dp_conf%lai = 7.0
      dp_conf%sai = 6.0
      dp_conf%rssnow = 2000.0
      dp_conf%sai_grass_haarweg = 3.5

      meteo%t = 31.0
      meteo%rh = 70.0
      meteo%glrad = 300.0
      meteo%pres_0 = 101500.0
      meteo%tsurf = 30.0
      meteo%ust = 3.0
      meteo%ol = 1000.0
      meteo%sinphi = 0.9
      meteo%nwet = 0



      allocate(comp%gw_param, source=gw_default())
      allocate(comp%gstom_param, source=gstom_default())
      allocate(comp%comp_point_param, source=comp_point_default())
      allocate(comp%rc_special, source=rc_special_default())

      allocate(lu%gsoil_param, source=gsoil_default())
      allocate(lu%rc_rinc%rinc_param, source=rinc_no_path())
      allocate(lu%stom_par%csoil_param, source=csoil_default())


      ! no error check for all parameters set correctly

      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_NONE, &
         message="depac returned error when all inputs were set correctly")

      print *, "Testing missing input parameters for depac_calc..."
      print *, dp_err%message
      if (allocated(error)) return
      call clear_error(dp_err)

      ! test missing component name
      comp%name = ""
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing component name did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)

      comp%name = "NH3"






      ! ! --------- Test missing land use name input ---------

      lu%name = ""
      ! test missing land use name
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing land use name did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)

      ! test missing land use index
      lu%name = "grass"


      ! ! --------- Test missing depac_config input ---------

      !missing lai:
      dp_conf%lai = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_NONE, &
         message="depac missing lai returned error, but it should just be a warning")

      if (allocated(error)) return
      call clear_error(dp_err)

      dp_conf%lai = 7.0
      ! missing sai:
      dp_conf%sai = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_NONE, &
         message="depac missing sai returned error, but it should just be a warning")
      if (allocated(error)) return
      call clear_error(dp_err)


      dp_conf%sai = 6.0

      ! missing rssnow
      dp_conf%rssnow = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing rssnow did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)

      ! missing sai_grass_haarweg
      dp_conf%rssnow = 2000.0
      dp_conf%sai_grass_haarweg = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing sai_grass_haarweg did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)

      dp_conf%sai_grass_haarweg = 3.5



      ! Check meteo


      meteo%t = -999.0
      ! test missing temperature
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing meteorology temperature did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)

      meteo%t = 31.0
      ! test missing meteorology relative humidity
      meteo%rh = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing meteorology rh did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      meteo%rh = 70.0

      ! test missing meteorology global radiation
      meteo%glrad = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing meteorology glrad did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      meteo%glrad = 300.0

      ! test missing meteorology surface level pressure
      meteo%pres_0 = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing meteorology pres_0 did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      meteo%pres_0 = 101500.0


      ! test missing meteorology surface temperature
      meteo%tsurf = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing meteorology tsurf did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      meteo%tsurf = 30.0


      ! test missing meteorology sine of solar elevation angle
      meteo%sinphi = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing meteorology sinphi did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      meteo%sinphi = 0.9

      ! test missing meteorology nwet (wetness indicator)
      meteo%nwet = -999
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing meteorology nwet did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      meteo%nwet = 0



      dp_conf%calc_comp_points = .true.

      dp_conf%comp_point%iratns = 2
      dp_conf%comp_point%c_nh3 = 100.0
      dp_conf%comp_point%c_so2 = 6.0
      dp_conf%comp_point%c_ave_nh3 = 3.0
      dp_conf%comp_point%c_ave_so2 = 2.0

      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_NONE, &
         message="depac returned error when all comp_point parameters were set correctly")
      print *, dp_err%message
      if (allocated(error)) return

      call clear_error(dp_err)

      ! missing comp_point%iratns
      dp_conf%comp_point%iratns = -999
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing comp_point%iratns did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      dp_conf%comp_point%iratns = 2

      ! missing comp_point%c_nh3
      dp_conf%comp_point%c_nh3 = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing comp_point%c_nh3 did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      dp_conf%comp_point%c_nh3 = 100.0

      ! missing comp_point%c_so2
      dp_conf%comp_point%c_so2 = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing comp_point%c_so2 did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      dp_conf%comp_point%c_so2 = 6.0

      ! missing comp_point%c_ave_nh3
      dp_conf%comp_point%c_ave_nh3 = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing comp_point%c_ave_nh3 did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      dp_conf%comp_point%c_ave_nh3 = 3.0

      ! missing comp_point%c_ave_so2
      dp_conf%comp_point%c_ave_so2 = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing comp_point%c_ave_so2 did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      dp_conf%comp_point%c_ave_so2 = 2.0

      dp_conf%calc_effective_rc = .true.

      dp_conf%ra_obs = 100.0
      dp_conf%rb = 50.0

      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_NONE, &
         message="depac returned error when all effective rc parameters were set correctly")
      if (allocated(error)) return
      call clear_error(dp_err)

      ! missing ra_obs
      dp_conf%ra_obs = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing ra_obs did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      dp_conf%ra_obs = 100.0

      ! missing rb
      dp_conf%rb = -999.0
      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)
      call check(error, dp_err%code, ERR_INPUT, &
         message="depac missing rb did not return error")
      if (allocated(error)) return
      call clear_error(dp_err)
      dp_conf%rb = 50.0



      ! Check the depac output

      call init_default_depac_config_rivm()
      i = 1
      j = 1
      lu = default_land_use_matrix(i,j)
      comp = default_component_matrix(i,j)


      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)

      call check(error, dp_err%code, ERR_NONE, &
         message="depac returned error when all inputs were set correctly for output check")
      if (allocated(error)) return

      call check(error, dp_out%version, VERSION, &
         message="depac version output incorrect")
      if (allocated(error)) return
      call check(error, dp_out%build_date, BUILD_DATE, &
         message="depac build_date output incorrect")
      if (allocated(error)) return

      call check(error, dp_out%gw, 7.03585669E-02, &
         message="depac gw output incorrect", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, missing(dp_out%gw_can), .true., &
         message="depac gw_can should be output missing")
      if (allocated(error)) return

      call check(error, dp_out%gstom, 2.60464419E-02, &
         message="depac gstom output incorrect", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%ccomp_tot, 40.4560585, &
         message="depac ccomp_tot output incorrect", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%gc_tot, 9.64050069E-02, &
         message="depac gc_tot output incorrect", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%gsoil_eff, 0.00000000, &
         message="depac gsoil_eff output incorrect", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%rc_tot, 10.3729048, &
         message="depac rc_tot output incorrect", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%rc_eff, 119.335388, &
         message="depac rc_eff output incorrect", thr=1.0e-6)
      if (allocated(error)) return



      ! test another case for 3,3 SO2, permanent crops, which has different parameterisations

      i = 3
      j = 3
      lu = default_land_use_matrix(i,j)
      comp = default_component_matrix(i,j)

      call depac_calc(comp, lu, meteo, dp_conf, dp_out, dp_err)


      call check(error, dp_out%gw, 0.00000000, &
         message="depac gw output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%gstom, 1.19560668E-02, &
         message="depac gstom output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%gsoil_eff, 9.72762646E-04, &
         message="depac gsoil_eff output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%rc_tot, 77.3465195, &
         message="depac rc_tot output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return
      call check(error, dp_out%rc_eff, 77.3465195, &
         message="depac rc_eff output incorrect for permanent crops", thr=1.0e-6)
      if (allocated(error)) return


   end subroutine test_missing_input

end module m_test_depac
