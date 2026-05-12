!-------------------------------------------------------------------------------
! File:        m_depac_factory.f90
! Module:      m_depac_factory
! Purpose:     Factory functions to create DEPAC configuration objects
!              Creates components, land uses, parametrizations, and setups
!              with default parameter values where optional parameters not provided
! Author:      Marte Voorneveld, RIVM
! Created:     May 12, 2026
!-------------------------------------------------------------------------------

module m_depac_factory
   use t_depac_setup, only: depac_setup
   use t_depac_component, only: depac_component
   use t_depac_land_use, only: depac_land_use

   use t_depac_land_use, only: depac_stomatal_params, depac_rc_r_params

   use c_depac_param_types, only: depac_comp_point_param, depac_csoil_param, depac_gsoil_param, &
      depac_gstom_param, depac_gw_param, depac_rc_special_param, depac_rinc_param

   use m_depac_params, only: gsoil_default, gw_default, gstom_default, comp_point_default, &
      rc_special_default, rinc_default, csoil_default


   implicit none (type, external)
   private
   public :: make_depac_component, make_depac_land_use, make_depac_rc_r_params, &
      make_depac_stom_params, make_depac_setup
contains

   !---------------------------------------------------------------------------
   ! Function: make_depac_setup
   ! Purpose:  Creates a complete DEPAC setup object with all parameterizations
   !           Assigns provided parameters or uses default values for optional ones
   ! Inputs:
   !    component           - depac_component defining pollutant properties
   !    land_use            - depac_land_use defining surface characteristics
   !    gsoil_param         - soil conductance parameter (optional, default used if absent)
   !    csoil_param         - soil concentration parameter (optional, default used if absent)
   !    rinc_param          - rain interception parameter (optional, default used if absent)
   !    gw_param            - ground resistance parameter (optional, default used if absent)
   !    gstom_param         - stomatal conductance parameter (optional, default used if absent)
   !    comp_point_param    - component point parameter (optional, default used if absent)
   !    rc_special_param    - special resistance parameter (optional, default used if absent)
   ! Output:
   !    setup - depac_setup object with all parameters initialized
   !---------------------------------------------------------------------------
   function make_depac_setup(component, land_use, &
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

   end function make_depac_setup

   !---------------------------------------------------------------------------
   ! Function: make_depac_component
   ! Purpose:  Creates a DEPAC component object defining pollutant properties
   !           and snow-related resistance parameters
   ! Inputs:
   !    name         - character string naming the component (e.g., 'NH3', 'O3')
   !    index        - integer index for component identification
   !    diffc        - real, diffusion coefficient for stomatal conductivity
   !    rw_val       - real, constant rw value (optional)
   !    ipar_snow    - integer, snow resistance parameterization (1=constant,
   !                   2=temperature dependent)
   !    rsoil_frozen - real, resistance for frozen soil (s/m)
   !    rsoil_wet    - real, resistance for wet soil (s/m)
   ! Output:
   !    comp - depac_component object with specified properties
   !---------------------------------------------------------------------------
   function make_depac_component(name, index, diffc, rw_val, ipar_snow, &
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

   end function make_depac_component

   !---------------------------------------------------------------------------
   ! Function: make_depac_land_use
   ! Purpose:  Creates a DEPAC land use object defining surface characteristics
   !           and resistance parameters for vegetation and soil
   ! Inputs:
   !    name              - character string naming the land use type (e.g., 'grass', 'forest')
   !    index             - integer index for the land use type (optional)
   !    gamma_stom_c_fac  - real, stomatal compensation point factor
   !    gamma_soil_c_fac  - real, gamma_soil c factor (only for water land use), otherwise -999.0
   !    rsoil             - real, soil resistance for this land use type and component (optional)
   !    stom_par          - depac_stomatal_params object with stomatal parameters (optional)
   !    rc_rinc           - depac_rc_r_params in-canopy resistance parameters (optional)
   ! Output:
   !    land_use - depac_land_use object with surface characteristics initialized
   !---------------------------------------------------------------------------
   function make_depac_land_use(name, index, gamma_stom_c_fac, gamma_soil_c_fac, rsoil, &
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
   end function make_depac_land_use

   !---------------------------------------------------------------------------
   ! Function: make_depac_rc_r_params
   ! Purpose:  Creates in-canopy resistance parameters for a given land use type
   !           Parameters are used for rain interception and canopy resistance
   ! Inputs:
   !    b - real, empirical parameter for in-canopy resistance (optional)
   !    h - real, empirical parameter for in-canopy resistance (optional)
   ! Output:
   !    rc_r_params - depac_rc_r_params object with parameters initialized
   !                  Default value (-999.0) indicates parameter undefined or not applicable
   !---------------------------------------------------------------------------
   function make_depac_rc_r_params(b, h) result(rc_r_params)
      real, intent(in), optional :: b
      real, intent(in), optional :: h
      type(depac_rc_r_params) :: rc_r_params

      if (present(b)) then
         rc_r_params%b = b
      end if

      if (present(h)) then
         rc_r_params%h = h
      end if

   end function make_depac_rc_r_params

   !---------------------------------------------------------------------------
   ! Function: make_depac_stom_params
   ! Purpose:  Creates stomatal conductance parameterization parameters
   !           Parameters define temperature and vapor pressure deficit (vpd)
   !           dependencies for plant stomatal conductance
   ! Inputs:
   !    F_min    - real, Minimum stomatal conductance(0-1) (s/m)
   !    alpha    - real, Alpha for F_light calculation
   !    Topt     - real, optimal temperature for stomatal opening (°C)
   !    Tmin     - real, minimum temperature for stomatal opening (°C)
   !    Tmax     - real, maximum temperature for stomatal opening (°C)
   !    g_max    - real, maximum stomatal conductance (m/s)
   !    vpd_max  - real, vapor pressure deficit threshold (Pa) - maximum allowed
   !    vpd_min  - real, vapor pressure deficit threshold (Pa) - minimum allowed
   ! Output:
   !    stom_par - depac_stomatal_params object with parameters initialized
   !---------------------------------------------------------------------------
   function make_depac_stom_params(F_min, alpha, Topt, Tmin, Tmax, g_max, &
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

   end function make_depac_stom_params

end module m_depac_factory
