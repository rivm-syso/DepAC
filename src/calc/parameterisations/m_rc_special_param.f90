module m_rc_special_param
    use m_rc_tot_fixed, only: rc_tot_fixed
    use m_rc_tot_nitric_acid, only: rc_tot_nitric_acid
    use m_rc_tot_nitric_oxide, only: rc_tot_nitric_oxide
    use m_special_default, only: rc_special_default

    implicit none (type, external)
    private
    public :: rc_tot_fixed, rc_tot_nitric_acid, rc_tot_nitric_oxide, rc_special_default
end module m_rc_special_param
