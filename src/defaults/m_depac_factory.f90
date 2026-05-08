module m_depac_factory
   use t_depac_land_use, only: depac_land_use, depac_rc_r_params, depac_stomatal_params, &
       t_csoil_parameterisation, t_gsoil_parameterisation, t_rinc_parameterisation
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use m_gstom_param, only: gstom_default
   use m_gw_param, only: gw_default
   use m_gsoil_param, only: gsoil_default, rinc_no_resistance, rinc_default
   use m_rc_special_param, only: rc_special_default
   use t_depac_component, only: depac_component, t_gstom_parameterisation, &
      t_comp_point_parameterisation, t_rc_special_param, t_gw_parameterisation

   use m_comp_point_param, only: comp_point_default, csoil_default


   implicit none (type, external)
   private
   public :: make_component, make_land_use, make_rc_r_params, make_stom_params
contains

   function make_component(name, index, diffc, rw_val, ipar_snow, rsoil_frozen, rsoil_wet, &
         gw_param, gstom_param, comp_point_param, rc_special) result(comp)
      character(len=*), intent(in) :: name
      integer, intent(in) :: index
      real, intent(in) :: diffc
      real, intent(in) :: rw_val
      integer, intent(in) :: ipar_snow
      real, intent(in) :: rsoil_frozen
      real, intent(in) :: rsoil_wet
      class(t_gw_parameterisation), intent(in), optional :: gw_param
      class(t_gstom_parameterisation), intent(in), optional :: gstom_param
      class(t_comp_point_parameterisation), intent(in), optional :: comp_point_param
      class(t_rc_special_param), intent(in), optional :: rc_special

      type(depac_component) :: comp

      comp%name = name
      comp%index = index
      comp%diffc = diffc
      comp%rw_val = rw_val
      comp%ipar_snow = ipar_snow
      comp%rsoil_frozen = rsoil_frozen
      comp%rsoil_wet = rsoil_wet

      if (present(gw_param)) then
         allocate(comp%gw_param, source=gw_param)
      else
         allocate(comp%gw_param, source=gw_default())
      end if

      if (present(gstom_param)) then
         allocate(comp%gstom_param, source=gstom_param)

      else
         allocate(comp%gstom_param, source=gstom_default())
      end if

      if (present(comp_point_param)) then
         allocate(comp%comp_point_param, source=comp_point_param)
      else
         allocate(comp%comp_point_param, source=comp_point_default())
      end if

      if (present(rc_special)) then
         allocate(comp%rc_special, source=rc_special)
      else
         allocate(comp%rc_special, source=rc_special_default())
      end if

   end function make_component

   function make_land_use(name, index, gamma_stom_c_fac, gamma_soil_c_fac, rsoil, &
         gsoil_param, stom_par, rc_rinc) result(land_use)
      character(len=*), intent(in) :: name
      integer, intent(in), optional :: index
      real, intent(in) :: gamma_stom_c_fac
      real, intent(in) :: gamma_soil_c_fac
      real, intent(in), optional :: rsoil
      class(t_gsoil_parameterisation), intent(in), optional :: gsoil_param
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

      if (present(gsoil_param)) then
         allocate(land_use%gsoil_param, source=gsoil_param)
      else
         allocate(land_use%gsoil_param, source=gsoil_default())
      end if

      if (present(stom_par)) then
         land_use%stom_par = stom_par
      end if

      if (present(rc_rinc)) then
         land_use%rc_rinc = rc_rinc
      else
         ! by default we assume no in canopy resistance (no b and h needed)
         allocate(land_use%rc_rinc%rinc_param, source=rinc_no_resistance())

         land_use%rc_rinc%b = -999
         land_use%rc_rinc%h = -999
      end if
   end function make_land_use

   function make_rc_r_params(b, h, rinc_param) result(rc_r_params)
      real, intent(in), optional :: b
      real, intent(in), optional :: h
      class(t_rinc_parameterisation), intent(in), optional :: rinc_param
      type(depac_rc_r_params) :: rc_r_params

      if (present(b)) then
         rc_r_params%b = b
      end if

      if (present(h)) then
         rc_r_params%h = h
      end if


      if (present(rinc_param)) then
         allocate(rc_r_params%rinc_param, source=rinc_param)
      else
         allocate(rc_r_params%rinc_param, source=rinc_default())
      end if

   end function make_rc_r_params

   function make_stom_params(F_min, alpha, Topt, Tmin, Tmax, g_max, &
          vpd_max, vpd_min, csoil_param) result(stom_par)
      real, intent(in) :: F_min
      real, intent(in) :: alpha
      real, intent(in) :: Topt
      real, intent(in) :: Tmin
      real, intent(in) :: Tmax
      real, intent(in) :: g_max
      real, intent(in) :: vpd_max
      real, intent(in) :: vpd_min
      class(t_csoil_parameterisation), intent(in), optional :: csoil_param

      type(depac_stomatal_params) :: stom_par

      stom_par%F_min = F_min
      stom_par%alpha = alpha
      stom_par%Topt = Topt
      stom_par%Tmin = Tmin
      stom_par%Tmax = Tmax
      stom_par%g_max = g_max
      stom_par%vpd_max = vpd_max
      stom_par%vpd_min = vpd_min

      if (present(csoil_param)) then
         allocate(stom_par%csoil_param, source=csoil_param)
      else
         allocate(stom_par%csoil_param, source=csoil_default())
      end if

   end function make_stom_params

end module m_depac_factory
