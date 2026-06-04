program main
  use depac, only: depac_calc
  use m_version, only: VERSION, BUILD_DATE
  implicit none (type, external)
  print *, "DepAC (Deposition of Airborne Compounds) Model"
  print *, "Version:", VERSION
  print *, "Build Date:", BUILD_DATE

  print *, "Copyright (C) RIVM 2026"
  print *, "RIVM - Rijksinstituut voor volksgezondheid en milieu"
  print *, "(National Institute for Public Health and the Environment)"
  print *, "All rights reserved."
  print *
  print *, "Released under the EUROPEAN UNION PUBLIC LICENCE v. 1.2 (EUPL-1.2)"
  print *, "You can use and redistribute this software according to the terms of the EUPL &
    (see LICENSE file)."
  print *, "For support, questions, or to report issues, please visit our GitHub repository:"
  print *, "https://github.com/rivm-syso/DepAC"
  print *, "For scientific details, we refer to the documentation."
end program main
