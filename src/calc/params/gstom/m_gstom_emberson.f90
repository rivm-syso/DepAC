module m_gstom_emberson
   use c_depac_params, only: depac_gstom_param
   use c_depac_core, only: depac_meteorology_core
   use t_depac_setup, only: depac_setup
   use t_depac_context, only: depac_context

   implicit none (type, external)

   private
   public :: gstom_emberson, rc_get_vpd, rc_gstom_emb, par_dir_diff

   type, extends(t_gstom_parameterisation) :: gstom_emberson
   contains
      procedure :: apply => gstom_emberson_apply
   end type gstom_emberson
contains
   pure function gstom_emberson_apply(this, setup, ctx) result(gstom)
      class(gstom_emberson), intent(in) :: this
      type(depac_setup), intent(in) :: setup
      type(depac_context), intent(in) :: ctx
      real :: gstom
      real :: vpd

      associate(config => setup%config, meteo => ctx%meteo,&
         comp => setup%component)

         if (ctx%has_vegetation) then
            if (meteo%glrad > 0.0) then
               vpd = rc_get_vpd(meteo)

               ! Scale by component diffusion coefficient
               gstom = rc_gstom_emb(setup, ctx, vpd, config) * comp%diffc / &
                  config%dO3
            else
               gstom = 0.0
            end if
         else
            gstom = 0.0
         end if
      end associate
   end function gstom_emberson_apply

   pure function rc_get_vpd(meteo) result(vpd)
      type(depac_meteorology_core), intent(in) :: meteo     ! meteorology
      real :: vpd    ! vapour pressure deficit (kPa)
      real :: esat
      ! Fit parameters for saturation vapour pressure
      real, parameter :: a1 = 6.113718e-1
      real, parameter :: a2 = 4.43839e-2
      real, parameter :: a3 = 1.39817e-3
      real, parameter :: a4 = 2.9295e-5
      real, parameter :: a5 = 2.16e-7
      real, parameter :: a6 = 3.0e-9
      esat = a1 + a2*meteo%t + a3*meteo%t**2 + a4*meteo%t**3 + a5*meteo%t**4 + a6*meteo%t**5
      vpd  = esat * (1 - meteo%rh / 100)
   end function rc_get_vpd

   !------------------------------------------------------------------------------
   ! Subroutine: par_dir_diff
   ! Purpose   : Partition global radiation into direct and diffuse PAR components
   ! Arguments :
   !   meteo    - meteorology type
   !   par_dir  - output direct PAR (W m-2)
   !   par_diff - output diffuse PAR (W m-2)
   ! Notes     : Implements Weiss & Norman (1985) and related parameterizations.
   !------------------------------------------------------------------------------
   pure subroutine par_dir_diff(meteo, par_dir, par_diff)
      type(depac_meteorology_core), intent(in) :: meteo ! meteorology
      real, intent(out) :: par_dir ! PAR direct : visible direct beam radiation(W m-2)
      real, intent(out) :: par_diff ! PAR diffuse: visible diffuse radiation (W m-2)
      real :: rdu, rdv, ww, rdm, rdn, rv, rn, ratio, sv, fv
      real :: pres
      pres = meteo%pres_0
      ! Calculate visible (PAR) direct beam radiation
      rdu = 600. * exp(-0.185 * (pres / meteo%pres_0) / meteo%sinphi) * meteo%sinphi
      ! Calculate potential visible diffuse radiation
      rdv = 0.4 * (600. - rdu) * meteo%sinphi
      ! Calculate the water absorption in the-near infrared
      ww = 1320 * 10 ** (-1.195 + 0.4459 * log10(1. / meteo%sinphi) - &
         0.0345 * (log10(1. / meteo%sinphi)) ** 2)
      ! Calculate potential direct beam near-infrared radiation
      rdm = (720. * exp(-0.06 * (pres / meteo%pres_0) / meteo%sinphi) - ww) * meteo%sinphi
      ! Calculate potential diffuse near-infrared radiation
      rdn = 0.6 * (720 - rdm - ww) * meteo%sinphi
      ! Compute visible and near-infrared radiation
      rv = max(0.1, rdu + rdv)
      rn = max(0.01, rdm + rdn)
      ! Compute ratio between input global radiation and total radiation computed here
      ratio = min(0.9, meteo%glrad / (rv + rn))
      ! Calculate total visible radiation
      sv = ratio * rv
      ! Calculate fraction of PAR in the direct beam
      fv = min(0.99, (0.9 - ratio) / 0.7)
      fv = max(0.01, rdu/rv*(1.0-(fv**2)**(1.0/3.0)))
      ! Compute direct and diffuse parts of PAR
      par_dir = fv * sv
      par_diff = sv - par_dir
   end subroutine par_dir_diff


   !------------------------------------------------------------------------------
   ! Subroutine: rc_gstom_emb
   ! Purpose   : Embedded calculation of stomatal conductance using land use,
   !             meteorology, VPD, and configuration parameters.
   ! Arguments :
   !   lu_conf  - land use configuration
   !   meteo    - meteorology (inout)
   !   vpd      - vapour pressure deficit (kPa)
   !   dp_conf  - DepAC configuration
   !   dp_out   - output structure
   ! Notes     : Implements radiation, temperature, VPD, and phenology corrections.
   !------------------------------------------------------------------------------
   pure function rc_gstom_emb(setup, ctx, vpd) result(gstom)
      type(depac_setup), intent(in) :: setup       ! stomatal parameters
      type(depac_context), intent(in) :: ctx        ! context with meteorology and output
      real, intent(in)  :: vpd       ! vapour pressure deficit (kPa)
      real :: F_light, F_phen, F_temp, F_vpd, F_swp, bt, F_env, pres
      real :: PARdir, PARdiff, PARshade, PARsun, LAIsun, LAIshade
      real :: gstom

      real :: sinphi

      associate(stom_par  => setup%land_use%stomatal_params, dp_conf => setup%config, meteo => ctx%meteo)

         ! Check whether vegetation is present:
         if (ctx%has_vegetation) THEN

            sinphi = meteo%sinphi
            ! Correction for very low solar elevation
            if (sinphi <= 0.0) then
               sinphi = 0.0001
            end if


            ! Direct and diffuse PAR, Photoactive (=visible) radiation:
            call par_dir_diff(meteo, PARdir, PARdiff)
            ! Calculate PARshade using Zhang et al. (2001) or Norman (1982)
            if (meteo%glrad > 200 .and. dp_conf%lai > 2.5) then
               PARshade = PARdiff*exp(-0.5*dp_conf%lai**0.8) + &
                  0.07*PARdir*(1.1-0.1*dp_conf%lai)*exp(-sinphi)
            else
               PARshade = PARdiff * exp(-0.5 * dp_conf%lai**0.7) + &
                  0.07*PARdir*(1.1-0.1*dp_conf%lai)*exp(-sinphi)
            end if
            ! PAR for sunlit leaves (canopy averaged)
            if (meteo%glrad > 200 .and. dp_conf%lai > 2.5) then
               PARsun = PARdir**0.8*0.5/sinphi + PARshade
            else
               PARsun = PARdir*0.5/sinphi + PARshade
            end if
            ! Leaf area index for sunlit and shaded leaves
            ! only works for sinphi > 0, but this is already checked and adjusted above
            LAIsun = 2*sinphi*(1-exp(-0.5*dp_conf%lai/sinphi ))
            LAIshade = dp_conf%lai - LAIsun


            ! Correction factor for radiation (Emberson)
            F_light = (LAIsun*(1 - exp(-1.*stom_par%alpha*PARsun)) + &
               LAIshade*(1 - exp(-1.*stom_par%alpha*PARshade)))/dp_conf%lai

            F_light = max(F_light, stom_par%F_min)
            ! Temperature influence
            bt = (stom_par%Tmax-stom_par%Topt)/(stom_par%Topt-stom_par%Tmin)
            F_temp  = ((meteo%t-stom_par%Tmin)/(stom_par%Topt-stom_par%Tmin)) * &
               ((stom_par%Tmax-meteo%t)/(stom_par%Tmax-stom_par%Topt))**bt
            F_temp = max(F_temp, stom_par%F_min)
            ! Vapour pressure deficit influence
            F_vpd = min(1.,((1.-stom_par%F_min)*(stom_par%vpd_min-vpd)/ &
               (stom_par%vpd_min-stom_par%vpd_max) + stom_par%F_min ))
            F_vpd = max(F_vpd, stom_par%F_min)
            ! Water potential influence (currently set to 1)
            F_swp = 1.
            ! Phenology influence (currently set to 1)
            F_phen = 1.
            ! Evaluate total stomatal conductance
            F_env = F_temp*F_vpd*F_swp
            F_env = max(F_env, stom_par%F_min)
            gstom = stom_par%G_max * F_light * F_phen * F_env
            ! gstom expressed per m2 leafarea; convert with LAI to m2 surface
            gstom = dp_conf%lai*gstom    ! in m/s

         else
            gstom = 0.0
         end if

      end associate
   end function rc_gstom_emb


end module m_gstom_emberson
