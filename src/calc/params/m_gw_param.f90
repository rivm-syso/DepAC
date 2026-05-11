! This module just exports gw parameterisations
module m_gw_param
    use m_gw_default, only: gw_default
    use m_gw_so2, only: gw_so2
    use m_gw_nh3_sutton, only: gw_nh3_sutton

    implicit none (type, external)
    private
    public :: gw_default, gw_so2, gw_nh3_sutton
end module m_gw_param