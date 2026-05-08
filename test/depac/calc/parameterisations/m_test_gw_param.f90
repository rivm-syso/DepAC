module m_test_gw_param
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_gw_param, only: gw_default, gw_so2, gw_nh3_sutton

   use t_depac_meteorology, only: depac_meteorology
   use t_depac_component_core, only: depac_component_core
   use t_depac_component, only: t_gw_parameterisation
   use t_depac_config, only: depac_config
   use t_depac_error, only: depac_error

   implicit none (type, external)
   private
   public :: collect_gw_param_tests
contains
   subroutine collect_gw_param_tests(testsuite)
      type(unittest_type), allocatable, intent(inout) :: testsuite(:)

      testsuite = [testsuite, &
         new_unittest("gw_default", test_gw_default), &
         new_unittest("gw_so2", test_gw_so2), &
         new_unittest("gw_nh3_sutton", test_gw_nh3_sutton) &
         ]
   end subroutine collect_gw_param_tests

   subroutine test_gw_default(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_meteorology) :: meteo
      type(depac_component_core) :: comp
      type(depac_config) :: dp_conf
      type(depac_error) :: dp_err
      class(t_gw_parameterisation), allocatable :: gw_f
      real:: gw
      comp%rw_val = -999.0
      

      allocate(gw_f, source=gw_default())
      gw = gw_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, 0.0, gw, message="gw_default missing rw_val test failed", thr=1.0e-5)
      if (allocated(error)) return


      dp_conf%has_vegetation = .false.
      comp%rw_val = 2.0
      gw = gw_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, 0.0, gw, message="gw_default no vegetation test failed", thr=1.0e-5)
      if (allocated(error)) return

      dp_conf%has_vegetation = .true.
      comp%rw_val = 2.0
      gw = gw_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, 0.5, gw, message="gw_default with vegetation test failed", thr=1.0e-5)
      if (allocated(error)) return
   end subroutine test_gw_default

   subroutine test_gw_so2(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_meteorology) :: meteo
      type(depac_component_core) :: comp
      type(depac_config) :: dp_conf
      type(depac_error) :: dp_err
      real :: gw
      class(t_gw_parameterisation), allocatable :: gw_so2_f
! call set_log_level(LOG_LEVEL_NONE)

      ! ! Test: has_vegetation true, nwet = 0
      dp_conf%has_vegetation = .true.
      meteo%nwet = 0
      meteo%t = 0.
      meteo%rh = 80.
      dp_conf%comp_point%iratns = 1
      allocate(gw_so2_f, source=gw_so2())
      gw = gw_so2_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 1.0227e-2, message="gw_so2 with nwet=0 failed", thr=1.0e-5)
      if (allocated(error)) return
      ! Test: rh > 81.3
      meteo%rh = 90.
      gw = gw_so2_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 5.58799e-2, &
         message="gw_so2 with nwet=0 and rh > 81.3 failed", thr=1.0e-5)
      if (allocated(error)) return

      ! Test: T < -1
      meteo%t = -2.
      gw = gw_so2_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 5e-3, &
         message="gw_so2 with nwet=0 and T < -1 failed", thr=1.0e-5)
      if (allocated(error)) return

      ! Test: T < -5
      meteo%t = -5.3
      gw = gw_so2_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 2e-3, &
         message="gw_so2 with nwet=0 and T < -5 failed", thr=1.0e-5)
      if (allocated(error)) return

      ! Test: nwet = 1
      meteo%nwet = 1
      gw = gw_so2_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 0.1, &
         message="gw_so2 with nwet=1 failed", thr=1.0e-5)
      if (allocated(error)) return


      ! Test: iratns = 3
      dp_conf%comp_point%iratns = 3

      gw = gw_so2_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 1.66666e-2, &
         message="gw_so2 with iratns=3 failed", thr=1.0e-5)
      if (allocated(error)) return


      ! Test: no vegetation
      dp_conf%has_vegetation = .false.
      gw = gw_so2_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 0.0, message="gw_so2 with no vegetation failed", thr=1.0e-5)
      if (allocated(error)) return


   end subroutine test_gw_so2

   subroutine test_gw_nh3_sutton(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_meteorology) :: meteo
      type(depac_component_core) :: comp
      type(depac_config) :: dp_conf
      type(depac_error) :: dp_err
      real :: gw
      class(t_gw_parameterisation), allocatable :: gw_nh3_sutton_f
      ! ! Test: vegetation, non-frozen soil
      dp_conf%has_vegetation = .true.
      dp_conf%sai = 0.6
      dp_conf%sai_grass_haarweg = 0.5 ! typical value for test
      meteo%rh = 80.
      meteo%tsurf = 0.1

      allocate(gw_nh3_sutton_f, source=gw_nh3_sutton())
      gw = gw_nh3_sutton_f%apply(meteo, comp, dp_conf, dp_err)

      call check(error, gw, 3.00000003E-03, &
         message="gw_nh3_sutton with vegetation", thr=1.0e-5)
      if (allocated(error)) return

      gw = gw_nh3_sutton_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, dp_conf%sai / 200.0, &
         message="gw_nh3_sutton with frozen soil", thr=1.0e-5)
      if (allocated(error)) return

      dp_conf%has_vegetation = .false.
      gw = gw_nh3_sutton_f%apply(meteo, comp, dp_conf, dp_err)
      call check(error, gw, 0.0, message="gw_nh3_sutton with no vegetation", thr=1.0e-5)
      if (allocated(error)) return

   end subroutine test_gw_nh3_sutton
end module m_test_gw_param
