module m_test_comp_point_param
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   use m_depac_params, only: comp_point_ammonia, csoil_default, csoil_water, comp_point_default
   use c_depac_param_types, only: depac_comp_point_param, depac_csoil_param



   implicit none (type, external)
   private
   public :: collect_comp_point_param_tests

contains


   subroutine collect_comp_point_param_tests(testsuite)
      type(unittest_type), allocatable, intent(inout) :: testsuite(:)
      type(unittest_type), allocatable :: append_testsuites(:)


      append_testsuites = [ &
         new_unittest("comp_point_ammonia", test_comp_point_ammonia), &
         new_unittest("comp_point_default", test_comp_point_default), &
         new_unittest("csoil_default", test_csoil_default), &
         new_unittest("csoil_water", test_csoil_water) &
         ]

      testsuite = [testsuite, append_testsuites]

   end subroutine collect_comp_point_param_tests
   subroutine test_comp_point_ammonia(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx
      class(depac_comp_point_param), allocatable :: comp_point_ammonia_f
      real :: ccomp_tot
      ! Test the default component point parameterisation for all components and land uses
      ! using the default configuration. This checks that the parameterisation runs without
      ! errors and produces reasonable outputs (e.g. non-negative concentrations).


      ctx%meteo%tsurf = 21.0
      ctx%meteo%t = 21.0

      setup%land_use%gamma_stom_c_fac = 0.1
      setup%land_use%gamma_soil_c_fac = 0.1

      allocate(setup%csoil_param, source=csoil_default())

      ctx%has_leaves = .true.
      ctx%has_vegetation = .true.

      ctx%state%comp_point%c_ave_nh3 = 20.0
      ctx%state%comp_point%c_nh3 = 6.0
      ctx%state%comp_point%c_ave_so2 = 0.9


      ctx%output%gc_tot = 0.0009
      ctx%output%gw = 1.0
      ctx%output%gstom = 1.0
      ctx%output%gsoil_eff = 2.0

      allocate(comp_point_ammonia_f, source=comp_point_ammonia())
      ccomp_tot = comp_point_ammonia_f%apply(setup, ctx)

      call check(error, ccomp_tot, 1255.00500,&
         message="rc_comp_point ammonia failed", thr=1.0e-2)
      if (allocated(error)) return

      ! no leaves
      ctx%has_leaves = .false.

      ccomp_tot = comp_point_ammonia_f%apply(setup, ctx)
      call check(error, ccomp_tot, 1245.29651,&
         message="rc_comp_point ammonia no leaves failed", thr=1.0e-2)
      if (allocated(error)) return

      deallocate(setup%csoil_param)
      allocate(setup%csoil_param, source=csoil_water())

      ccomp_tot = comp_point_ammonia_f%apply(setup, ctx)

      call check(error, ccomp_tot, 1246.21399,&
         message="rc_comp_point ammonia with csoil_water failed", thr=1.0e-2)
      if (allocated(error)) return




      deallocate(setup%csoil_param)
      allocate(setup%csoil_param, source=csoil_default())
      ctx%has_leaves = .true.
      ! negative c_ave_nh3
      ctx%state%comp_point%c_ave_nh3 = -1.0

      ccomp_tot = comp_point_ammonia_f%apply(setup, ctx)

      call check(error, ccomp_tot, 1127.75928,&
         message="rc_comp_point ammonia negative c_ave_nh3 failed", thr=1.0e-2)
      if (allocated(error)) return

      ! no vegetation
      ctx%has_vegetation = .false.
      ctx%state%comp_point%c_ave_nh3 = 12
      ctx%state%comp_point%c_ave_so2 = -0.1

      ccomp_tot = comp_point_ammonia_f%apply(setup, ctx)

      call check(error, ccomp_tot, 5.82508612,&
         message="rc_comp_point ammonia no vegetation failed", thr=1.0e-2)

      ! negative gc_tot

      ctx%output%gc_tot = -0.1

      ccomp_tot = comp_point_ammonia_f%apply(setup, ctx)

      call check(error, ccomp_tot, 0.0,&
         message="rc_comp_point ammonia negative gc_tot failed", thr=1.0e-2)
      if (allocated(error)) return

      deallocate(comp_point_ammonia_f)
      deallocate(setup%csoil_param)
   end subroutine test_comp_point_ammonia

   subroutine test_comp_point_default(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      real :: ccomp_tot
      class(depac_comp_point_param), allocatable :: comp_point_default_f

      allocate(comp_point_default_f, source=comp_point_default())

      ccomp_tot = comp_point_default_f%apply(setup, ctx)
      call check(error, ccomp_tot, 0.0,&
         message="comp_point_default failed", thr=1.0e-5)
      if (allocated(error)) return
      deallocate(comp_point_default_f)
   end subroutine test_comp_point_default


   subroutine test_csoil_default(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      class(depac_csoil_param), allocatable :: csoil_default_f

      real :: csoil
      allocate(csoil_default_f, source=csoil_default())
      csoil = csoil_default_f%apply(setup, ctx, 1.0)

      call check(error, csoil, 0.0,&
         message="csoil_default failed", thr=1.0e-5)
      if (allocated(error)) return
      deallocate(csoil_default_f)
   end subroutine test_csoil_default

   subroutine test_csoil_water(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_setup) :: setup
      type(depac_context) :: ctx

      class(depac_csoil_param), allocatable :: csoil_water_f

      real :: csoil

      setup%land_use%gamma_soil_c_fac = 0.1

      allocate(csoil_water_f, source=csoil_water())

      csoil = csoil_water_f%apply(setup, ctx, 1.0)

      call check(error, csoil, 0.1,&
         message="csoil_water with positive gamma_soil_c_fac failed", thr=1.0e-5)
      if (allocated(error)) return

      setup%land_use%gamma_soil_c_fac = -0.1
      ctx%state%comp_point%c_ave_nh3 = 20.0

      csoil = csoil_water_f%apply(setup, ctx, 1.0)

      call check(error, csoil, 2.0,&
         message="csoil_water with negative gamma_soil_c_fac failed", thr=1.0e-5)
      if (allocated(error)) return

      deallocate(csoil_water_f)
   end subroutine test_csoil_water


end module m_test_comp_point_param
