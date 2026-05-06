module m_depac_factory
   use t_depac_component, only: depac_component, gw_parameterisation
   use t_depac_config, only: depac_config
   use t_depac_meteorology, only: depac_meteorology
   use t_depac_error, only: depac_error

   use m_gw_default, only: gw_default


   implicit none (type, external)
   private
   public :: make_component
contains
   function make_component(name, index, diffc, rw_val, ipar_snow, rsoil_frozen, rsoil_wet, gw_param) result(comp)
      character(len=*), intent(in) :: name
      integer, intent(in) :: index
      real, intent(in) :: diffc
      real, intent(in) :: rw_val
      integer, intent(in) :: ipar_snow
      real, intent(in) :: rsoil_frozen
      real, intent(in) :: rsoil_wet
      procedure(gw_parameterisation), pointer, optional :: gw_param

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
   end function make_component

end module m_depac_factory
