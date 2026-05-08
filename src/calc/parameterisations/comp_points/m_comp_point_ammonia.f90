module m_comp_point_ammonia
   use t_depac_component_core, only: depac_component_core
   use t_depac_land_use, only: depac_land_use
   use t_depac_land_use_core, only: depac_land_use_core
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_error, only: depac_error
   use t_depac_output, only: depac_output

   implicit none (type, external)
   public :: comp_point_ammonia
contains
   pure function comp_point_ammonia(meteo, lu_conf, dp_conf, dp_out) result(ccomp_tot)
      type(depac_meteorology), intent(in) :: meteo
      type(depac_land_use), intent(in) :: lu_conf
      type(depac_config), intent(in) :: dp_conf
      type(depac_output), intent(in) :: dp_out
      
      real :: cw, cstom, csoil, gamma_stom, gamma_soil, gamma_w, tk, tfac, co_dep_fac
      real :: ccomp_tot

      tk = meteo%tsurf + 273.15

      tfac = (2.75e15/tk)*exp(-1.04e4/tk)

      ! Stomatal compensation point:
      if (dp_conf%has_leaves .and. dp_conf%comp_point%c_ave_nh3 > 0.) then
         gamma_stom = lu_conf%gamma_stom_c_fac * dp_conf%comp_point%c_ave_nh3 * &
            4.7 * exp(-0.071*meteo%t)

         cstom = max(0.0, gamma_stom*tfac)
      else
         cstom = 0.0
      endif

      ! External leaf compensation point:
      if (dp_conf%has_vegetation .and. &
         dp_conf%comp_point%c_ave_nh3 > 0. .and. &
         dp_conf%comp_point%c_ave_so2 > 0.) then

         gamma_w = -850. + 1840. * dp_conf%comp_point%c_nh3 * exp(-0.11*meteo%t)
         co_dep_fac = 1.12 - 1.32 * ((dp_conf%comp_point%c_ave_so2/64.) / &
            (dp_conf%comp_point%c_ave_nh3/17.))
         co_dep_fac = max(0.0, co_dep_fac)
         gamma_w = co_dep_fac * gamma_w
         cw = max(0.0, gamma_w*tfac)
      elseif (dp_conf%has_vegetation) then
         gamma_w = -850. + 1840. * dp_conf%comp_point%c_nh3 * exp(-0.11*meteo%t)
         cw = max(0.0, gamma_w*tfac)
      else
         cw = 0.0
      endif

      ! Soil compensation point:
      csoil = lu_conf%stom_par%csoil_param(lu_conf, dp_conf, tfac)



      
      ! Total compensation point is weighed average of separate compensation points:
      if (dp_out%gc_tot > 0.0) then
         ccomp_tot = (dp_out%gw/dp_out%gc_tot)*cw + &
            (dp_out%gstom/dp_out%gc_tot)*cstom + &
            (dp_out%gsoil_eff/dp_out%gc_tot)*csoil
      else
         ccomp_tot = 0.0
      endif

   end function comp_point_ammonia


   pure function csoil_default(lu_conf, dp_conf, tfac) result(csoil)
      class(depac_land_use_core), intent(in) :: lu_conf
      type(depac_config), intent(in) :: dp_conf
      real, intent(in) :: tfac

      real :: csoil, gamma_soil

      csoil = 0.0

   end function csoil_default

   pure function csoil_water(lu_conf, dp_conf, tfac) result(csoil)
      class(depac_land_use_core), intent(in) :: lu_conf
      type(depac_config), intent(in) :: dp_conf
      real, intent(in) :: tfac

      real :: csoil, gamma_soil

      if (lu_conf%gamma_soil_c_fac > 0) then
         gamma_soil = lu_conf%gamma_soil_c_fac * 1.0
      else
         gamma_soil = abs(lu_conf%gamma_soil_c_fac) * dp_conf%comp_point%c_ave_nh3
      endif
      
      csoil = gamma_soil * tfac

   end function csoil_water


end module m_comp_point_ammonia
