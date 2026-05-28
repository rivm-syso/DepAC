!------------------------------------------------------------------------------
! Module:     m_check_depac_input
! Author:     Marte Voorneveld, RIVM
! Created:    November 14 2025
! Updated:    November 18 2025
! Description:
!   This module provides input validation routines for the DepAC atmospheric
!   deposition model. It checks required fields for components, land use,
!   configuration, location, and meteorology, setting errors for missing or
!   invalid values. This is useful to ensure that all necessary input data is
!   provided before running the model.
!------------------------------------------------------------------------------

module m_check_depac_input

   use m_depac_error, only: set_error
   use m_logger, only: log_warn, log_error
   use m_helpers, only: missing
   use t_depac_context, only: depac_context
   use t_depac_setup, only: depac_setup
   use t_depac_error_core, only: ERR_INPUT

   implicit none (type, external)
   public
contains

   subroutine check_component_input(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      associate(comp => setup%component, dp_err => ctx%error)
         ! make sure name is set
         if (trim(comp%name) == '') then
            call set_error(dp_err, ERR_INPUT, 'Component name is empty.')
            return
         end if
      end associate


   end subroutine check_component_input

   subroutine check_land_use_input(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      associate(lu => setup%land_use, dp_err => ctx%error)
         if (trim(lu%name) == '') then
            call set_error(dp_err, ERR_INPUT, 'Land use name is empty.')
            return
         end if
        !  end if

      end associate
   end subroutine check_land_use_input

   subroutine check_depac_parameterizations(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      associate(dp_err => ctx%error)
         if(.not. allocated(setup%gsoil_param)) then
            call set_error(dp_err, ERR_INPUT, 'gsoil_param is not allocated in depac_config.')
            return
         end if
         if(.not. allocated(setup%csoil_param)) then
            call set_error(dp_err, ERR_INPUT, 'csoil_param is not allocated in depac_config.')
            return
         end if
         if(.not. allocated(setup%rinc_param)) then
            call set_error(dp_err, ERR_INPUT, 'rinc_param is not allocated in depac_config.')
            return
         end if
         if(.not. allocated(setup%gw_param)) then
            call set_error(dp_err, ERR_INPUT, 'gw_param is not allocated in depac_config.')
            return
         end if
         if(.not. allocated(setup%gstom_param)) then
            call set_error(dp_err, ERR_INPUT, 'gstom_param is not allocated in depac_config.')
            return
         end if
         if(.not. allocated(setup%comp_point_param)) then
            call set_error(dp_err, ERR_INPUT, 'comp_point_param is not allocated in depac_config.')
            return
         end if
         if(.not. allocated(setup%rc_special_param)) then
            call set_error(dp_err, ERR_INPUT, 'rc_special_param is not allocated in depac_config.')
            return
         end if

      end associate

   end subroutine check_depac_parameterizations

   subroutine check_depac_config(setup, ctx)
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(inout) :: ctx

      associate(dp_conf => setup%config, dp_err => ctx%error, state => ctx%state)
         ! lai and sai can be missing (-999.0) as they may not be needed
         !for all components and are checked elsewhere
         ! but we provide a warning here
         if (missing(state%lai)) then
            call log_warn('depac_config%lai is missing (-999.0). This may lead to incorrect ' &
            // 'calculations for components that require LAI.')
         end if

         if (missing(state%sai)) then
            call log_warn('depac_config%sai is missing (-999.0). This may lead to incorrect ' &
               // 'calculations for components that require SAI.')
         end if

         if(missing(dp_conf%rssnow)) then
            call set_error(dp_err, ERR_INPUT, 'rssnow is missing in depac_config.')
            return
         end if

         if(missing(dp_conf%sai_grass_haarweg)) then
            call set_error(dp_err, ERR_INPUT, 'sai_grass_haarweg is missing in depac_config.')
            return
         end if

         if(dp_conf%calc_effective_rc) then
            ! RA and Rb are required
            if(missing(state%ra_obs)) then
               call set_error(dp_err, ERR_INPUT,&
                  'ra is missing in depac_config while calc_effective_rc is true.')
               return
            end if
            if(missing(state%rb)) then
               call set_error(dp_err, ERR_INPUT,&
                  'rb is missing in depac_config while calc_effective_rc is true.')
               return
            end if
         end if

         if(dp_conf%calc_comp_points) then
            if(missing(state%comp_point%iratns)) then
               call set_error(dp_err, ERR_INPUT, &
                  'comp_point%iratns is missing in depac_config while calc_comp_points is true.')
               return
            end if

            if(missing(state%comp_point%c_nh3)) then
               call set_error(dp_err, ERR_INPUT, &
                  'comp_point%c_nh3 is missing in depac_config while calc_comp_points is true.')
               return
            end if

            if(missing(state%comp_point%c_so2)) then
               call set_error(dp_err, ERR_INPUT, &
                  'comp_point%c_so2 is missing in depac_config while calc_comp_points is true.')
               return
            end if

            if(missing(state%comp_point%c_ave_nh3)) then
               call set_error(dp_err, ERR_INPUT, &
                  'comp_point%c_ave_nh3 is missing in depac_config while calc_comp_points true.')
               return
            end if

            if(missing(state%comp_point%c_ave_so2)) then
               call set_error(dp_err, ERR_INPUT, &
                  'comp_point%c_ave_so2 is missing in depac_config while calc_comp_points true.')
               return
            end if
         end if
      end associate
   end subroutine check_depac_config



   subroutine check_meteorology_input(ctx)
      type(depac_context), intent(inout) :: ctx

      associate(meteo => ctx%meteo, dp_err => ctx%error)

         if(missing(meteo%t)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology temperature (t) is missing.')
            return
         end if

         if(missing(meteo%rh)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology relative humidity (rh) is missing.')
            return
         end if

         if(missing(meteo%glrad)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology global radiation (glrad) is missing.')
            return
         end if

         if(missing(meteo%pres_0)) then
            call set_error(dp_err, ERR_INPUT, &
               'Meteorology surface level pressure (pres_0) is missing.')
            return
         end if

         if(missing(meteo%tsurf)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology surface temp (tsurf) is missing.')
            return
         end if


         if(missing(meteo%sinphi)) then
            call set_error(dp_err, ERR_INPUT, &
               'Meteorology sine of solar elevation angle (sinphi) is missing.')
            return
         end if

         if(missing(meteo%nwet)) then
            call set_error(dp_err, ERR_INPUT, 'Meteorology nwet (wetness indicator) is missing.')
            return
         end if
      end associate

   end subroutine check_meteorology_input

end module m_check_depac_input
