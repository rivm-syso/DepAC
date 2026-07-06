module m_test_gstom_param
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_depac_params, only: gstom_default, gstom_emberson

   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   use c_depac_param_types, only: depac_gstom_param


   use m_gstom_emberson, only: rc_get_vpd, rc_gstom_emb, par_dir_diff

   implicit none (type, external)
   private
   public :: collect_gstom_param_tests
contains
   subroutine collect_gstom_param_tests(testsuite)
      type(unittest_type), allocatable, intent(inout) :: testsuite(:)

      testsuite = [testsuite, &
         new_unittest("gstom_default", test_gstom_default), &
         new_unittest("gstom_emberson", test_gstom_emberson) &
         ]
   end subroutine collect_gstom_param_tests

   subroutine test_gstom_default(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      class(depac_gstom_param), allocatable :: gstom_f
      real :: gstom

      allocate(gstom_f, source=gstom_default())


      gstom = gstom_f%apply(setup, ctx)

      call check(error, gstom, 0.0, message="gstom_default failed", thr=1.0e-5)
      if (allocated(error)) return


   end subroutine test_gstom_default

   subroutine test_gstom_emberson(error)
      type(error_type), allocatable, intent(out) :: error


      call test_gstom_emberson_vpd(error)
      if (allocated(error)) return

      call test_gstom_emberson_par_dir_diff(error)
      if (allocated(error)) return

      call test_gstom_emberson_rc_gstom_emb(error)
      if (allocated(error)) return



   end subroutine test_gstom_emberson

   subroutine test_gstom_emberson_vpd(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_context) :: ctx

      real :: vpd

      ctx%meteo%t = 20.0
      ctx%meteo%rh = 50.0
      vpd = rc_get_vpd(ctx%meteo)

      call check(error, vpd, 1.16841877, message="rc_get_vpd failed", thr=1.0e-3)
      if (allocated(error)) return

      ! high humidity different temperature
      ctx%meteo%t = 25.0
      ctx%meteo%rh = 90.0
      vpd = rc_get_vpd(ctx%meteo)

      call check(error, vpd, 0.316623241, message="rc_get_vpd high humidity failed", thr=1.0e-3)
      if (allocated(error)) return



   end subroutine test_gstom_emberson_vpd

   subroutine test_gstom_emberson_par_dir_diff(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_context) :: ctx
      real :: par_dir, par_diff


      ctx%meteo%pres_0 = 1013.25
      ctx%meteo%sinphi = 0.5
      ctx%meteo%glrad = 500.0

      call par_dir_diff(ctx%meteo, par_dir, par_diff)

      call check(error, par_dir, 106.727577, message="par_dir_diff par_dir failed", thr=1.0e-4)
      if (allocated(error)) return
      call check(error, par_diff, 112.688774, message="par_dir_diff par_diff failed", thr=1.0e-4)
      if (allocated(error)) return

   end subroutine test_gstom_emberson_par_dir_diff

   subroutine test_gstom_emberson_rc_gstom_emb(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_context) :: ctx
      type(depac_setup) :: setup
      real :: gstom, vpd

      associate(stom_par => setup%land_use%stom_par)
         stom_par%F_min   = 0.01
         stom_par%alpha   = 0.009
         stom_par%Topt    = 26.0
         stom_par%Tmin    = 12.0
         stom_par%Tmax    = 40.0
         stom_par%g_max   = 300.0
         stom_par%vpd_max = 0.9
         stom_par%vpd_min = 2.8

         ctx%has_vegetation = .false.

         vpd = 1.0

         gstom = rc_gstom_emb(setup, ctx, vpd)

         call check(error, gstom, 0.0, &
            message="rc_gstom_emb no vegetation failed", thr=1.0e-5)
         if (allocated(error)) return

         ctx%meteo%sinphi = 0.0
         ctx%meteo%glrad = 500.0
         ctx%meteo%pres_0 = 1013.25
         ctx%meteo%t = 25.0
         ctx%state%lai = 2.6

         vpd = 1.0

         ctx%has_vegetation = .true.

         gstom = rc_gstom_emb(setup, ctx, vpd)

         call check(error, gstom, 7.35585546, &
            message="rc_gstom_emb with vegetation failed", thr=1.0e-5)
         if (allocated(error)) return


         ctx%meteo%sinphi = 0.3
         ctx%meteo%glrad = 150.0
         ctx%state%lai = 2.0

         gstom = rc_gstom_emb(setup, ctx, vpd)
         call check(error, gstom, 128.818481, &
            message="rc_gstom_emb with low glrad and lai failed", thr=1.0e-5)
         if (allocated(error)) return

      end associate

   end subroutine test_gstom_emberson_rc_gstom_emb
end module m_test_gstom_param
