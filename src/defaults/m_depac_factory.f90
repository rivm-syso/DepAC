module m_depac_factory
   use t_depac_setup, only: depac_setup
   use t_depac_component, only: depac_component
   use t_depac_land_use, only: depac_land_use

   use t_depac_land_use, only: depac_stomatal_params, depac_rc_r_params

   use c_depac_params, only: depac_comp_point_param, depac_csoil_param, depac_gsoil_param, &
      depac_gstom_param, depac_gw_param, depac_rc_special_param, depac_rinc_param

   use c_params, only: gsoil_default, gw_default, gstom_default, comp_point_default, &
      rc_special_default, rinc_default, csoil_default


   implicit none (type, external)
   private
   public :: make_component, make_land_use, make_rc_r_params, make_stom_params, make_setup
contains

   function make_setup(component, land_use, &
      gsoil_param, csoil_param, rinc_param, &
      gw_param, gstom_param, comp_point_param, rc_special_param) result(setup)
      type(depac_component), intent(in) :: component
      type(depac_land_use), intent(in) :: land_use
      class(depac_gsoil_param), intent(in), optional :: gsoil_param
      class(depac_csoil_param), intent(in), optional :: csoil_param
      class(depac_rinc_param), intent(in), optional :: rinc_param
      class(depac_gw_param), intent(in), optional :: gw_param
      class(depac_gstom_param), intent(in), optional :: gstom_param
      class(depac_comp_point_param), intent(in), optional :: comp_point_param
      class(depac_rc_special_param), intent(in), optional :: rc_special_param
      type(depac_setup) :: setup

      setup%component = component
      setup%land_use = land_use

      if (present(gsoil_param)) then
         allocate(setup%gsoil_param, source=gsoil_param)
      else
         allocate(setup%gsoil_param, source=gsoil_default())
      end if



      if (present(gw_param)) then
         allocate(setup%gw_param, source=gw_param)
      else
         allocate(setup%gw_param, source=gw_default())
      end if

      if (present(gstom_param)) then
         allocate(setup%gstom_param, source=gstom_param)

      else
         allocate(setup%gstom_param, source=gstom_default())
      end if

      if (present(comp_point_param)) then
         allocate(setup%comp_point_param, source=comp_point_param)
      else
         allocate(setup%comp_point_param, source=comp_point_default())
      end if

      if (present(rc_special_param)) then
         allocate(setup%rc_special_param, source=rc_special_param)
      else
         allocate(setup%rc_special_param, source=rc_special_default())
      end if


      if (present(csoil_param)) then
         allocate(setup%csoil_param, source=csoil_param)
      else
         allocate(setup%csoil_param, source=csoil_default())
      end if

      if (present(rinc_param)) then
         allocate(setup%rinc_param, source=rinc_param)
      else
         allocate(setup%rinc_param, source=rinc_default())
      end if

   end function make_setup

   function make_component(name, index, diffc, rw_val, ipar_snow, &
      rsoil_frozen, rsoil_wet) result(comp)
      character(len=*), intent(in) :: name
      integer, intent(in) :: index
      real, intent(in) :: diffc
      real, intent(in) :: rw_val
      integer, intent(in) :: ipar_snow
      real, intent(in) :: rsoil_frozen
      real, intent(in) :: rsoil_wet

      type(depac_component) :: comp

      comp%name = name
      comp%index = index
      comp%diffc = diffc
      comp%rw_val = rw_val
      comp%ipar_snow = ipar_snow
      comp%rsoil_frozen = rsoil_frozen
      comp%rsoil_wet = rsoil_wet

   end function make_component

   function make_land_use(name, index, gamma_stom_c_fac, gamma_soil_c_fac, rsoil, &
      stom_par, rc_rinc) result(land_use)
      character(len=*), intent(in) :: name
      integer, intent(in), optional :: index
      real, intent(in) :: gamma_stom_c_fac
      real, intent(in) :: gamma_soil_c_fac
      real, intent(in), optional :: rsoil
      type(depac_stomatal_params), intent(in), optional :: stom_par
      type(depac_rc_r_params), intent(in), optional :: rc_rinc

      type(depac_land_use) :: land_use

      land_use%name = name
      land_use%gamma_stom_c_fac = gamma_stom_c_fac
      land_use%gamma_soil_c_fac = gamma_soil_c_fac

      if (present(index)) then
         land_use%index = index
      end if

      if (present(rsoil)) then
         land_use%rsoil = rsoil
      end if

      if (present(stom_par)) then
         land_use%stom_par = stom_par
      end if

      if (present(rc_rinc)) then
         land_use%rc_rinc = rc_rinc
      else
         land_use%rc_rinc%b = -999
         land_use%rc_rinc%h = -999
      end if
   end function make_land_use

   function make_rc_r_params(b, h) result(rc_r_params)
      real, intent(in), optional :: b
      real, intent(in), optional :: h
      type(depac_rc_r_params) :: rc_r_params

      if (present(b)) then
         rc_r_params%b = b
      end if

      if (present(h)) then
         rc_r_params%h = h
      end if



   end function make_rc_r_params

   function make_stom_params(F_min, alpha, Topt, Tmin, Tmax, g_max, &
      vpd_max, vpd_min) result(stom_par)
      real, intent(in) :: F_min
      real, intent(in) :: alpha
      real, intent(in) :: Topt
      real, intent(in) :: Tmin
      real, intent(in) :: Tmax
      real, intent(in) :: g_max
      real, intent(in) :: vpd_max
      real, intent(in) :: vpd_min

      type(depac_stomatal_params) :: stom_par

      stom_par%F_min = F_min
      stom_par%alpha = alpha
      stom_par%Topt = Topt
      stom_par%Tmin = Tmin
      stom_par%Tmax = Tmax
      stom_par%g_max = g_max
      stom_par%vpd_max = vpd_max
      stom_par%vpd_min = vpd_min
   end function make_stom_params

end module m_depac_factory
