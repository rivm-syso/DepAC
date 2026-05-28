module m_test_gw_param
    use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_depac_params, only: gw_default, gw_so2, gw_nh3_sutton

   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   use c_depac_param_types, only: depac_gw_param


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
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      class(depac_gw_param), allocatable :: gw_f
      real:: gw

      setup%component%rw_val = -999.0


      allocate(gw_f, source=gw_default())

      gw = gw_f%apply(setup, ctx)
      call check(error, 0.0, gw, message="gw_default missing rw_val test failed", thr=1.0e-5)
      if (allocated(error)) return


      ctx%has_vegetation = .false.
      setup%component%rw_val = 2.0
      gw = gw_f%apply(setup, ctx)
      call check(error, 0.0, gw, message="gw_default no vegetation test failed", thr=1.0e-5)
      if (allocated(error)) return

      ctx%has_vegetation = .true.
      setup%component%rw_val = 2.0
      gw = gw_f%apply(setup, ctx)
      call check(error, 0.5, gw, message="gw_default with vegetation test failed", thr=1.0e-5)
      if (allocated(error)) return
   end subroutine test_gw_default

   subroutine test_gw_so2(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx
      real :: gw
      class(depac_gw_param), allocatable :: gw_so2_f
! call set_log_level(LOG_LEVEL_NONE)

      ! ! Test: has_vegetation true, nwet = 0
      ctx%has_vegetation = .true.
      ctx%meteo%nwet = 0
      ctx%meteo%t = 0.
      ctx%meteo%rh = 80.
      ctx%state%comp_point%iratns = 1

      allocate(gw_so2_f, source=gw_so2())
      gw = gw_so2_f%apply(setup, ctx)
      call check(error, gw, 1.0227e-2, message="gw_so2 with nwet=0 failed", thr=1.0e-5)
      if (allocated(error)) return
      ! Test: rh > 81.3
      ctx%meteo%rh = 90.
      gw = gw_so2_f%apply(setup, ctx)
      call check(error, gw, 5.58799e-2, &
         message="gw_so2 with nwet=0 and rh > 81.3 failed", thr=1.0e-5)
      if (allocated(error)) return

      ! Test: T < -1
      ctx%meteo%t = -2.
      gw = gw_so2_f%apply(setup, ctx)
      call check(error, gw, 5e-3, &
         message="gw_so2 with nwet=0 and T < -1 failed", thr=1.0e-5)
      if (allocated(error)) return

      ! Test: T < -5
      ctx%meteo%t = -5.3
      gw = gw_so2_f%apply(setup, ctx)
      call check(error, gw, 2e-3, &
         message="gw_so2 with nwet=0 and T < -5 failed", thr=1.0e-5)
      if (allocated(error)) return

      ! Test: nwet = 1
      ctx%meteo%nwet = 1
      gw = gw_so2_f%apply(setup, ctx)
      call check(error, gw, 0.1, &
         message="gw_so2 with nwet=1 failed", thr=1.0e-5)
      if (allocated(error)) return


      ! Test: iratns = 3
      ctx%state%comp_point%iratns = 3

      gw = gw_so2_f%apply(setup, ctx)
      call check(error, gw, 1.66666e-2, &
         message="gw_so2 with iratns=3 failed", thr=1.0e-5)
      if (allocated(error)) return


      ! Test: no vegetation
      ctx%has_vegetation = .false.
      gw = gw_so2_f%apply(setup, ctx)
      call check(error, gw, 0.0, message="gw_so2 with no vegetation failed", thr=1.0e-5)
      if (allocated(error)) return


   end subroutine test_gw_so2

   subroutine test_gw_nh3_sutton(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx
      real :: gw
      class(depac_gw_param), allocatable :: gw_nh3_sutton_f
      ! ! Test: vegetation, non-frozen soil
      ctx%has_vegetation = .true.
      ctx%state%sai = 0.6
      setup%config%sai_grass_haarweg = 0.5 ! typical value for test
      ctx%meteo%rh = 80.
      ctx%meteo%tsurf = 0.1

      allocate(gw_nh3_sutton_f, source=gw_nh3_sutton())
      gw = gw_nh3_sutton_f%apply(setup, ctx)

      call check(error, gw, 3.00000003E-03, &
         message="gw_nh3_sutton with vegetation", thr=1.0e-5)
      if (allocated(error)) return

      gw = gw_nh3_sutton_f%apply(setup, ctx)
      call check(error, gw, ctx%state%sai / 200.0, &
         message="gw_nh3_sutton with frozen soil", thr=1.0e-5)
      if (allocated(error)) return

      ctx%has_vegetation = .false.
      gw = gw_nh3_sutton_f%apply(setup, ctx)
      call check(error, gw, 0.0, message="gw_nh3_sutton with no vegetation", thr=1.0e-5)
      if (allocated(error)) return

   end subroutine test_gw_nh3_sutton
end module m_test_gw_param
