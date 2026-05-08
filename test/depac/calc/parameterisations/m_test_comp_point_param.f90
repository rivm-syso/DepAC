module m_test_comp_point_param
   use testdrive, only : new_unittest, unittest_type, error_type, check

   use m_comp_point_param, only: comp_point_ammonia, comp_point_default, csoil_default, csoil_water

   use t_depac_meteorology, only: depac_meteorology
   use t_depac_land_use, only: depac_land_use
   use t_depac_config, only: depac_config
   use t_depac_output, only: depac_output

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
      type(depac_meteorology) :: meteo
      type(depac_land_use) :: lu
      type(depac_config) :: dp_conf
      type(depac_output) :: dp_out

      real :: ccomp_tot
      ! Test the default component point parameterisation for all components and land uses
      ! using the default configuration. This checks that the parameterisation runs without
      ! errors and produces reasonable outputs (e.g. non-negative concentrations).
      meteo%tsurf = 21.0
      meteo%t = 21.0

      lu%gamma_stom_c_fac = 0.1
      lu%gamma_soil_c_fac = 0.1

      lu%stom_par%csoil_param => csoil_default

      dp_conf%has_leaves = .true.
      dp_conf%has_vegetation = .true.
      dp_conf%comp_point%c_ave_nh3 = 20.0
      dp_conf%comp_point%c_nh3 = 6.0
      dp_conf%comp_point%c_ave_so2 = 0.9

      dp_out%gc_tot = 0.0009
      dp_out%gw = 1.0
      dp_out%gstom = 1.0
      dp_out%gsoil_eff = 2.0

      ccomp_tot = comp_point_ammonia(meteo, lu, dp_conf, dp_out)

      call check(error, ccomp_tot, 1255.00500,&
         message="rc_comp_point ammonia failed", thr=1.0e-2)
      if (allocated(error)) return

      ! no leaves
      dp_conf%has_leaves = .false.

      ccomp_tot = comp_point_ammonia(meteo, lu, dp_conf, dp_out)
      call check(error, ccomp_tot, 1245.29651,&
         message="rc_comp_point ammonia no leaves failed", thr=1.0e-2)
      if (allocated(error)) return


      lu%stom_par%csoil_param => csoil_water

      ccomp_tot = comp_point_ammonia(meteo, lu, dp_conf, dp_out)

      call check(error, ccomp_tot, 1246.21399,&
         message="rc_comp_point ammonia with csoil_water failed", thr=1.0e-2)
      if (allocated(error)) return




      lu%stom_par%csoil_param => csoil_default
      dp_conf%has_leaves = .true.
      ! negative c_ave_nh3
      dp_conf%comp_point%c_ave_nh3 = -1.0

      ccomp_tot = comp_point_ammonia(meteo, lu, dp_conf, dp_out)

      call check(error, ccomp_tot, 1127.75928,&
         message="rc_comp_point ammonia negative c_ave_nh3 failed", thr=1.0e-2)
      if (allocated(error)) return

      ! no vegetation
      dp_conf%has_vegetation = .false.
      dp_conf%comp_point%c_ave_nh3 = 12
      dp_conf%comp_point%c_ave_so2 = -0.1

      ccomp_tot = comp_point_ammonia(meteo, lu, dp_conf, dp_out)

      call check(error, ccomp_tot, 5.82508612,&
         message="rc_comp_point ammonia no vegetation failed", thr=1.0e-2)

      ! negative gc_tot

      dp_out%gc_tot = -0.1

      ccomp_tot = comp_point_ammonia(meteo, lu, dp_conf, dp_out)

      call check(error, ccomp_tot, 0.0,&
         message="rc_comp_point ammonia negative gc_tot failed", thr=1.0e-2)
      if (allocated(error)) return

   end subroutine test_comp_point_ammonia

   subroutine test_comp_point_default(error)
      type(error_type), allocatable, intent(out) :: error
        type(depac_meteorology) :: meteo
      type(depac_land_use) :: lu
      type(depac_config) :: dp_conf
      type(depac_output) :: dp_out

        real :: ccomp_tot

        ccomp_tot = comp_point_default(meteo, lu, dp_conf, dp_out)
        call check(error, ccomp_tot, 0.0,&
         message="comp_point_default failed", thr=1.0e-5)
       if (allocated(error)) return
    end subroutine test_comp_point_default


   subroutine test_csoil_default(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_land_use) :: lu
      type(depac_config) :: dp_conf

      real :: csoil

      csoil = csoil_default(lu, dp_conf, 1.0)

      call check(error, csoil, 0.0,&
         message="csoil_default failed", thr=1.0e-5)
       if (allocated(error)) return
   end subroutine test_csoil_default

   subroutine test_csoil_water(error)
      type(error_type), allocatable, intent(out) :: error
      type(depac_land_use) :: lu
      type(depac_config) :: dp_conf

      real :: csoil

      lu%gamma_soil_c_fac = 0.1

      csoil = csoil_water(lu, dp_conf, 1.0)

      call check(error, csoil, 0.1,&
         message="csoil_water with positive gamma_soil_c_fac failed", thr=1.0e-5)
       if (allocated(error)) return

      lu%gamma_soil_c_fac = -0.1
      dp_conf%comp_point%c_ave_nh3 = 20.0

      csoil = csoil_water(lu, dp_conf, 1.0)

      call check(error, csoil, 2.0,&
         message="csoil_water with negative gamma_soil_c_fac failed", thr=1.0e-5)
       if (allocated(error)) return

   end subroutine test_csoil_water


end module m_test_comp_point_param
