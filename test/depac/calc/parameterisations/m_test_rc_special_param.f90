module m_test_rc_special_param
      use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_depac_params, only: rc_tot_fixed, rc_tot_nitric_acid, rc_tot_nitric_oxide, rc_special_default

   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   use c_depac_param_types, only: depac_rc_special_param

   use t_depac_error_core, only: ERR_INPUT


   implicit none (type, external)
   private
   public :: collect_rc_special_param_tests
contains
   subroutine collect_rc_special_param_tests(testsuite)
      type(unittest_type), allocatable, intent(inout) :: testsuite(:)

      testsuite = [testsuite, &
         new_unittest("rc_tot_default", test_rc_tot_default), &
         new_unittest("rc_tot_fixed", test_rc_tot_fixed), &
         new_unittest("rc_tot_nitric_acid", test_rc_tot_nitric_acid), &
         new_unittest("rc_tot_nitric_oxide", test_rc_tot_nitric_oxide) &
         ]

   end subroutine collect_rc_special_param_tests

   subroutine test_rc_tot_default(error)
      type(error_type), allocatable, intent(out) :: error
      ! This is just a placeholder test to ensure that the default special parameterisation does not cause errors
      type(depac_setup) :: setup
      type(depac_context) :: ctx


      class(depac_rc_special_param), allocatable :: rc_special_f
      logical :: ready

      ready = .false.

      allocate(rc_special_f, source=rc_special_default())

! start with nwet=0 to ensure that the apply method does not set ready to true
      ctx%meteo%nwet = 0

      call rc_special_f%apply(setup, ctx, ready)


      call check(error, ready, .false., message="rc_special_default should not set ready to true")
      if (allocated(error)) return

      ! now we basically test rc_snow

      ctx%meteo%nwet = 9

      ! Test 1: ipar_snow = 1, should return constant value
      setup%component%ipar_snow = 1
      setup%config%rssnow = 150.
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 150., 'ipar_snow=1 test failed', thr=1.0e-5)
      if (allocated(error)) return

      ! Test 2: ipar_snow = 2, t < -1, should return 500
      setup%component%ipar_snow = 2
      ctx%meteo%t = -2.0
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 500., 'ipar_snow=2, t<-1 test failed', thr=1.0e-5)
      if (allocated(error)) return

      ! Test 3: ipar_snow = 2, t > 1, should return 70
      ctx%meteo%t = 2.0
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 70., 'ipar_snow=2, t>1 test failed', thr=1.0e-5)
      if (allocated(error)) return

      ! Test 4: ipar_snow = 2, -1 <= t <= 1, should return 70*(2-t)
      ctx%meteo%t = 0.0
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 140., 'ipar_snow=2, -1<=t<=1 test failed', thr=1.0e-5)
      if (allocated(error)) return

      ! test 5: invalid ipar_snow value, should set error
      setup%component%ipar_snow = 3
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%error%code, ERR_INPUT, 'Invalid ipar_snow did not set error code')
      if (allocated(error)) return

      deallocate(rc_special_f)
   end subroutine test_rc_tot_default

   subroutine test_rc_tot_fixed(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      class(depac_rc_special_param), allocatable :: rc_special_f
      logical :: ready

      allocate(rc_special_f, source=rc_tot_fixed())
      call rc_special_f%apply(setup, ctx, ready)

      call check(error, ctx%output%rc_tot, 2000.0, &
         message="rc_tot_fixed did not set rc_tot to 2000.0", thr=1.0e-5)
      if (allocated(error)) return


      call check(error, ready, .true., message="rc_tot_fixed did not set ready to true")
      if (allocated(error)) return

      ! check if we can set a different value for rc_tot_fixed
      deallocate(rc_special_f)
      allocate(rc_special_f, source=rc_tot_fixed(rc_tot_fixed=1500.0))

      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 1500.0, &
         message="rc_tot_fixed did not set rc_tot to 1500.0", thr=1.0e-5)
      if (allocated(error)) return


      deallocate(rc_special_f)
   end subroutine test_rc_tot_fixed

   subroutine test_rc_tot_nitric_acid(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      class(depac_rc_special_param), allocatable :: rc_special_f
      logical :: ready

      ctx%meteo%t = -10.0
      ctx%meteo%nwet = 9


      allocate(rc_special_f, source=rc_tot_nitric_acid())
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 50.0, &
         message="rc_tot_nitric_acid did not set rc_tot to 50.0 for T < -5 and nwet=9",&
         thr=1.0e-5)
      if (allocated(error)) return

      ctx%meteo%t = 0.0
      ctx%meteo%nwet = 0
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 10.0, &
         message="rc_tot_nitric_acid did not set rc_tot to 10.0 for T >= -5 or nwet != 9",&
         thr=1.0e-5)
      if (allocated(error)) return

      call check(error, ready, .true., message="rc_tot_nitric_acid did not set ready to true")
      if (allocated(error)) return


      deallocate(rc_special_f)
   end subroutine test_rc_tot_nitric_acid

   subroutine test_rc_tot_nitric_oxide(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      class(depac_rc_special_param), allocatable :: rc_special_f
      logical :: ready
      ready = .false.
      allocate(rc_special_f, source=rc_tot_nitric_oxide())
      call rc_special_f%apply(setup, ctx, ready)

      ctx%meteo%nwet = 0

      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ready, .false., &
         message="rc_tot_nitric_oxide did not keep ready false for nwet=0")
      if (allocated(error)) return

      ctx%meteo%nwet = 1
      call rc_special_f%apply(setup, ctx, ready)
      call check(error, ctx%output%rc_tot, 2000.0, &
         message="rc_tot_nitric_oxide did not set rc_tot to 2000.0 for nwet=1",&
         thr=1.0e-5)
      if (allocated(error)) return
      call check(error, ready, .true., &
         message="rc_tot_nitric_oxide did not set ready to true for nwet=1")
      if (allocated(error)) return

      deallocate(rc_special_f)
      allocate(rc_special_f, source=rc_tot_nitric_oxide(fixed_rc_tot=2500.0))

      call rc_special_f%apply(setup, ctx, ready)

      call check(error, ctx%output%rc_tot, 2500.0, &
         message="rc_tot_nitric_oxide did not set rc_tot to 2500.0 for &
         nwet=1 when fixed_rc_tot was set to 2500.0",&
         thr=1.0e-5)
      if (allocated(error)) return


      ctx%meteo%nwet = 9
      setup%component%ipar_snow = 1
      setup%config%rssnow = 150.
      call rc_special_f%apply(setup, ctx, ready)

      call check(error, ctx%output%rc_tot, 150., 'we expected the rc_snow routine', thr=1.0e-5)
      if (allocated(error)) return

   end subroutine test_rc_tot_nitric_oxide
end module m_test_rc_special_param



