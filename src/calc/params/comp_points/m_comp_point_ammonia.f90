!------------------------------------------------------------------------------
! Module:     m_comp_point_ammonia
! Author:     Marte Voorneveld, Addo van Pul, Jan Willem Erisman,
!                       Ferd Sauter, Margreet van Zanten, Roy Wichink Kruit
! Created:   May 11, 2026
! Modified:  May 12, 2026
! Description:
!   This module provides routines for calculating compensation points for
!   atmospheric deposition modeling components, especially NH3. The main
!   subroutine, comp_point_ammonia_apply, computes compensation points for stomatal,
!   external leaf, and soil pathways, using meteorological, land use, and
!   component-specific parameters. The results are used in dry deposition
!   calculations and model output.
!------------------------------------------------------------------------------

module m_comp_point_ammonia
   use c_depac_param_types, only: depac_comp_point_param, depac_csoil_param
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)
   private
   public :: comp_point_ammonia, csoil_default, csoil_water

   type, extends(depac_comp_point_param) :: comp_point_ammonia
   contains
      procedure :: apply => comp_point_ammonia_apply
   end type comp_point_ammonia

   type, extends(depac_csoil_param) :: csoil_default
   contains
      procedure :: apply => csoil_default_apply
   end type csoil_default

   type, extends(depac_csoil_param) :: csoil_water
   contains
      procedure :: apply => csoil_water_apply
   end type csoil_water
contains

   ! Compensation point parameterisation for ammonia
   pure function comp_point_ammonia_apply(this, setup, ctx) result(ccomp_tot)
      class(comp_point_ammonia), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx

      real :: cw, cstom, csoil, gamma_stom, gamma_soil, gamma_w, tk, tfac, co_dep_fac
      real :: ccomp_tot

      select type (setup)
       type is (depac_setup)

         associate(dp_conf => setup%config, comp_point => setup%config%comp_point, &
            meteo => ctx%meteo, lu_conf => setup%land_use, output => ctx%output)

            tk = meteo%tsurf + 273.15

            tfac = (2.75e15/tk)*exp(-1.04e4/tk)

            ! Stomatal compensation point:
            if (ctx%has_leaves .and. comp_point%c_ave_nh3 > 0.) then
               gamma_stom = lu_conf%gamma_stom_c_fac * dp_conf%comp_point%c_ave_nh3 * &
                  4.7 * exp(-0.071*meteo%t)

               cstom = max(0.0, gamma_stom*tfac)
            else
               cstom = 0.0
            endif

            ! External leaf compensation point:
            if (ctx%has_vegetation .and. &
               comp_point%c_ave_nh3 > 0. .and. &
               comp_point%c_ave_so2 > 0.) then

               gamma_w = -850. + 1840. * comp_point%c_nh3 * exp(-0.11*meteo%t)
               co_dep_fac = 1.12 - 1.32 * ((comp_point%c_ave_so2/64.) / &
                  (comp_point%c_ave_nh3/17.))
               co_dep_fac = max(0.0, co_dep_fac)
               gamma_w = co_dep_fac * gamma_w
               cw = max(0.0, gamma_w*tfac)
            elseif (ctx%has_vegetation) then
               gamma_w = -850. + 1840. * comp_point%c_nh3 * exp(-0.11*meteo%t)
               cw = max(0.0, gamma_w*tfac)
            else
               cw = 0.0
            endif

            ! Soil compensation point:
            csoil = setup%csoil_param%apply(setup, ctx, tfac)




            ! Total compensation point is weighed average of separate compensation points:
            if (output%gc_tot > 0.0) then
               ccomp_tot = (output%gw/output%gc_tot)*cw + &
                  (output%gstom/output%gc_tot)*cstom + &
                  (output%gsoil_eff/output%gc_tot)*csoil
            else
               ccomp_tot = 0.0
            endif
         end associate

       class default
         ! If the setup is not of type depac_setup, return missing value
         ccomp_tot = -999.0
      end select

   end function comp_point_ammonia_apply

   ! Default soil compensation point parameterisation: returns 0.0 for all conditions
   pure function csoil_default_apply(this, setup, ctx, tfac) result(csoil)
      class(csoil_default), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real, intent(in) :: tfac

      real :: csoil
      select type (setup)
       type is (depac_setup)
         ! Default soil compensation point parameterisation: returns 0.0 for all conditions
         ! This can be used as a placeholder or for land use types that do not require a specific soil compensation point parameterisation.
         csoil = 0.0
       class default
         ! If the setup is not of type depac_setup, return missing value
         csoil = -999.0
      end select

   end function csoil_default_apply
   ! Water compensation point parameterisation: soil compensation point is calculated
   ! based on water compensation point factor and average ammonia concentration.
   pure function csoil_water_apply(this, setup, ctx, tfac) result(csoil)
      class(csoil_water), intent(in) :: this
      class(*), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real, intent(in) :: tfac

      real :: csoil, gamma_soil
      select type (setup)
       type is (depac_setup)
         ! Water compensation point parameterisation: soil compensation point is calculated based on water compensation point factor and average ammonia concentration.
         associate(comp_point => setup%config%comp_point, &
            lu_conf => setup%land_use)

            if (lu_conf%gamma_soil_c_fac > 0) then
               gamma_soil = lu_conf%gamma_soil_c_fac * 1.0
            else
               gamma_soil = abs(lu_conf%gamma_soil_c_fac) * comp_point%c_ave_nh3
            endif

            csoil = gamma_soil * tfac
         end associate
       class default
         ! If the setup is not of type depac_setup, return missing value
         csoil = -999.0
      end select
   end function csoil_water_apply


end module m_comp_point_ammonia
