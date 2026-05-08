module m_depac_factory
   use t_depac_land_use, only: depac_land_use, depac_rc_r_params, depac_stomatal_params, &
       csoil_parameterisation, gsoil_parameterisation, rinc_parameterisation
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_config, only: depac_config
   use m_gstom_param, only: gstom_default
   use m_gw_param, only: gw_default
   use m_gsoil_param, only: gsoil_default, rinc_no_resistance, rinc_default
   use m_rc_special_param, only: t_rc_tot_fixed
   use t_depac_component, only: depac_component, gw_parameterisation, gstom_parameterisation, &
      comp_point_parameterisation, t_rc_special

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
      procedure(gw_parameterisation), pointer, optional :: gw_param
      procedure(gstom_parameterisation), pointer, optional :: gstom_param
      procedure(comp_point_parameterisation), pointer, optional :: comp_point_param
      class(t_rc_special), intent(in), optional :: rc_special

      type(depac_component) :: comp

      comp%name = name
      comp%index = index
      comp%diffc = diffc
      comp%rw_val = rw_val
      comp%ipar_snow = ipar_snow
      comp%rsoil_frozen = rsoil_frozen
      comp%rsoil_wet = rsoil_wet

      if (present(gw_param)) then
         comp%gw_param => gw_param
      else
         comp%gw_param => gw_default
      end if

      if (present(gstom_param)) then
         comp%gstom_param => gstom_param
      else
         comp%gstom_param => gstom_default
      end if

      if (present(comp_point_param)) then
         comp%comp_point_param => comp_point_param
      else
         comp%comp_point_param => comp_point_default
      end if

      if (present(rc_special)) then
         allocate(comp%rc_special, source=rc_special)
      else
         allocate(comp%rc_special, source=t_rc_tot_fixed()) 
      end if

   end function make_component

   function make_land_use(name, index, gamma_stom_c_fac, gamma_soil_c_fac, rsoil, &
         gsoil_param, stom_par, rc_rinc) result(land_use)
      character(len=*), intent(in) :: name
      integer, intent(in) :: index
      real, intent(in) :: gamma_stom_c_fac
      real, intent(in) :: gamma_soil_c_fac
      real, intent(in) :: rsoil
      procedure(gsoil_parameterisation), pointer, optional :: gsoil_param
      type(depac_stomatal_params), intent(in), optional :: stom_par
      type(depac_rc_r_params), intent(in), optional :: rc_rinc
      type(depac_land_use) :: land_use

      land_use%name = name
      land_use%index = index
      land_use%gamma_stom_c_fac = gamma_stom_c_fac
      land_use%gamma_soil_c_fac = gamma_soil_c_fac
      land_use%rsoil = rsoil

      if (present(gsoil_param)) then
         land_use%gsoil_param => gsoil_param
      else
         land_use%gsoil_param => gsoil_default
      end if

      if (present(stom_par)) then
         land_use%stom_par = stom_par
      end if

      if (present(rc_rinc)) then
         land_use%rc_rinc = rc_rinc
      else
         ! by default we assume no in canopy resistance (no b and h needed)
         land_use%rc_rinc%rinc_param => rinc_no_resistance
         land_use%rc_rinc%b = -999
         land_use%rc_rinc%h = -999
      end if
   end function make_land_use

   function make_rc_r_params(b, h, rinc_param) result(rc_r_params)
      real, intent(in), optional :: b
      real, intent(in), optional :: h
      procedure(rinc_parameterisation), pointer, optional :: rinc_param
      type(depac_rc_r_params) :: rc_r_params

      if (present(b)) then
         rc_r_params%b = b
      end if

      if (present(h)) then
         rc_r_params%h = h
      end if


      if (present(rinc_param)) then
         rc_r_params%rinc_param => rinc_param
      else
         ! when making with a b and h we assume the default rinc parameterisation
         rc_r_params%rinc_param => rinc_default
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
      procedure(csoil_parameterisation), pointer, optional :: csoil_param

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
         stom_par%csoil_param => csoil_param
      else
         ! when making with stomatal parameters we assume the default csoil parameterisation for soil compensation point calculation
         stom_par%csoil_param => csoil_default
      end if

   end function make_stom_params

end module m_depac_factory
