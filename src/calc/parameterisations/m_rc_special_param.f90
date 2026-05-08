module m_rc_special_param
    use m_rc_tot_fixed, only: t_rc_tot_fixed
    use m_rc_tot_nitric_acid, only: t_rc_tot_nitric_acid
    use m_rc_tot_nitric_oxide, only: t_rc_tot_nitric_oxide
    use m_special_default, only: t_special_default

    implicit none (type, external)
    private
    public :: t_rc_tot_fixed, t_rc_tot_nitric_acid, t_rc_tot_nitric_oxide, t_special_default
end module m_rc_special_param
