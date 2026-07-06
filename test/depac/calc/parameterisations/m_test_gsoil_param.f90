module m_test_gsoil_param
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   use m_depac_params, only: gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance
   use c_depac_param_types, only: depac_gsoil_param, depac_rinc_param


   implicit none (type, external)
   private
   public :: collect_gsoil_param_tests
contains
   subroutine collect_gsoil_param_tests(testsuite)
      type(unittest_type), allocatable, intent(inout) :: testsuite(:)

      testsuite = [testsuite, &
         new_unittest("gsoil_default", test_gsoil_default), &
         new_unittest("rsoil_rinc_default", test_rsoil_rinc_default), &
         new_unittest("rsoil_rinc_no_path", test_rsoil_rinc_no_path), &
         new_unittest("rsoil_rinc_no_resistance", test_rsoil_rinc_no_resistance) &
         ]

   end subroutine collect_gsoil_param_tests

   subroutine test_gsoil_default(error)
      type(error_type), allocatable, intent(out) :: error
    type(depac_setup) :: setup
    type(depac_context) :: ctx

      real :: gsoil
      class(depac_gsoil_param), allocatable :: gsoil_f

      allocate(setup%rinc_param, source=rinc_no_path())

      allocate(gsoil_f, source=gsoil_default())

      gsoil = gsoil_f%apply(setup, ctx)


      call check(error, gsoil, 0.0, message="missing(rinc) failed", thr=1.0e-5)
      if (allocated(error)) return

      deallocate(setup%rinc_param)
      allocate(setup%rinc_param, source=rinc_no_resistance())

      ctx%meteo%t = -5.0 ! negative temperature
      setup%component%rsoil_frozen = 1.0
      gsoil = gsoil_f%apply(setup, ctx)
      call check(error, gsoil, 1.0, &
         message="frozen soil with no rinc resistance failed", thr=1.0e-5)
      if (allocated(error)) return

      !missing rsoil_frozen
      setup%component%rsoil_frozen = -999.0
      gsoil = gsoil_f%apply(setup, ctx)

      call check(error, gsoil, 0.0, &
         message="missing rsoil_frozen did not result in gsoil=0", thr=1.0e-5)
      if (allocated(error)) return

      ctx%meteo%t = 5.0 ! positive temperature
      ctx%meteo%nwet = 0 ! dry soil condition
      setup%land_use%rsoil = 2.0
      gsoil = gsoil_f%apply(setup, ctx)
      call check(error, gsoil, 0.5, &
         message="dry soil with no rinc resistance failed", thr=1.0e-5)
      if (allocated(error)) return

      ctx%meteo%nwet = 1 ! wet soil condition
      setup%component%rsoil_wet = 3.0
      gsoil = gsoil_f%apply(setup, ctx)


      call check(error, gsoil, 0.3333333333, &
         message="wet soil with no rinc resistance failed", thr=1.0e-5)
      if (allocated(error)) return

      ! invalid nwet value
      ctx%meteo%nwet = 2
      gsoil = gsoil_f%apply(setup, ctx)
      call check(error, gsoil, 0.0, &
         message="invalid nwet value did not result in gsoil=0", thr=1.0e-5)
      if (allocated(error)) return

      ctx%meteo%nwet = 1 ! wet soil condition
      setup%component%rsoil_wet = -999.0 ! missing rsoil_wet
      gsoil = gsoil_f%apply(setup, ctx)
      call check(error, gsoil, 0.0, &
         message="missing rsoil_wet did not result in gsoil=0", thr=1.0e-5)
      if (allocated(error)) return

      ctx%meteo%nwet = 0 ! dry soil condition
      setup%land_use%rsoil = -999.0 ! missing lu%rsoil
      gsoil = gsoil_f%apply(setup, ctx)
      call check(error, gsoil, 0.0, &
         message="missing lu%rsoil did not result in gsoil=0", thr=1.0e-5)
      if (allocated(error)) return



      deallocate(gsoil_f)
   end subroutine test_gsoil_default

   subroutine test_rsoil_rinc_default(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_context) :: ctx
      type(depac_setup) :: setup

      real :: rinc
      class(depac_rinc_param), allocatable :: rinc_f

      allocate(rinc_f, source=rinc_default())

      ctx%meteo%ust = -1.0 ! negative ust to test missing rinc resistance

      rinc = rinc_f%apply(setup, ctx)

      call check(error, rinc, 1000.0, &
         message="rinc with negative ust did not result in rinc=1000.0", thr=1.0e-5)
      if (allocated(error)) return

      ! positive ust to test normal rinc calculation
      ctx%meteo%ust = 3.0

      setup%land_use%rc_rinc%b  = 1.2
      setup%land_use%rc_rinc%h  = 5.5
      ctx%state%sai = 6.0


      rinc = rinc_f%apply(setup, ctx)

      call check(error, rinc, 13.2, &
         message="rinc with positive ust did not calculate correctly", thr=1.0e-5)
      if (allocated(error)) return


      deallocate(rinc_f)
   end subroutine test_rsoil_rinc_default

   subroutine test_rsoil_rinc_no_path(error)
      type(error_type), allocatable, intent(out) :: error

      type(depac_setup) :: setup
      type(depac_context) :: ctx
      real :: rinc
      class(depac_rinc_param), allocatable :: rinc_f

      allocate(rinc_f, source=rinc_no_path())

      rinc = rinc_f%apply(setup, ctx)


      call check(error, rinc, -999.0, message="rinc_no_path did not return -999.0", thr=1.0e-5)
      if (allocated(error)) return

      deallocate(rinc_f)
   end subroutine test_rsoil_rinc_no_path

   subroutine test_rsoil_rinc_no_resistance(error)
      type(error_type), allocatable, intent(out) :: error

      type(depac_setup) :: setup
      type(depac_context) :: ctx
      real :: rinc
      class(depac_rinc_param), allocatable :: rinc_f

      allocate(rinc_f, source=rinc_no_resistance())
      rinc = rinc_f%apply(setup, ctx)

      call check(error, rinc, 0.0, message="rinc_no_resistance did not return 0.0", thr=1.0e-5)
      if (allocated(error)) return
      deallocate(rinc_f)

   end subroutine test_rsoil_rinc_no_resistance

end module m_test_gsoil_param
