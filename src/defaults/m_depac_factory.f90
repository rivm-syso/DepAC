module m_depac_factory
   use t_depac_component, only: depac_component, gw_parameterisation, gstom_parameterisation
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_error, only: depac_error

   use t_depac_land_use, only: depac_land_use, gsoil_parameterisation, rinc_parameterisation, depac_stomatal_params, depac_rc_r_params

   use m_gw_param, only: gw_default
   use m_gstom_param, only: gstom_default
   use m_gsoil_param, only: gsoil_default, rinc_default, rinc_no_path, rinc_no_resistance


   implicit none (type, external)
   private
   public :: make_component
contains
   function make_component(name, index, diffc, rw_val, ipar_snow, rsoil_frozen, rsoil_wet, gw_param, gstom_param) result(comp)
      character(len=*), intent(in) :: name
      integer, intent(in) :: index
      real, intent(in) :: diffc
      real, intent(in) :: rw_val
      integer, intent(in) :: ipar_snow
      real, intent(in) :: rsoil_frozen
      real, intent(in) :: rsoil_wet
      procedure(gw_parameterisation), pointer, optional :: gw_param
      procedure(gstom_parameterisation), pointer, optional :: gstom_param

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
   end function make_component

   function make_land_use(name, index, gamma_stom_c_fac, gamma_soil_c_fac, rsoil, gsoil_param, stom_par, rc_rinc_params) result(land_use)
      character(len=*), intent(in) :: name
      integer, intent(in) :: index
      real, intent(in) :: gamma_stom_c_fac
      real, intent(in) :: gamma_soil_c_fac
      real, intent(in) :: rsoil
      procedure(gsoil_parameterisation), pointer, optional :: gsoil_param
      type(depac_stomatal_params), intent(in), optional :: stom_par
      type(depac_rc_r_params), intent(in), optional :: rc_rinc_params
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

      if (present(rc_rinc_params)) then
         land_use%rc_rinc = rc_rinc_params
      else
         ! by default we assume no in canopy resistance (no b and h needed)
         land_use%rc_rinc%rinc_param => rinc_no_resistance
         land_use%rc_rinc%b = -999
         land_use%rc_rinc%h = -999
      end if


   end function make_land_use

   function make_rc_r_params(b, h, rinc_param) result(rc_r_params)
      real, intent(in) :: b
      real, intent(in) :: h
      procedure(rinc_parameterisation), pointer, optional :: rinc_param
      type(depac_rc_r_params) :: rc_r_params

      rc_r_params%b = b
      rc_r_params%h = h
      if (present(rinc_param)) then
         rc_r_params%rinc_param => rinc_param
      else
         ! when making with a b and h we assume the default rinc parameterisation
         rc_r_params%rinc_param => rinc_default
      end if

   end function make_rc_r_params

end module m_depac_factory
