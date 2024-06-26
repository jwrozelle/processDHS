#' A list of Variable vectors
#'
#'
#'
#' @format A list with the following vectors:
#' \describe{
#'   \item{IMCI_lt2m_Vars}{Variables used for IMCI among children 2 months old or less}
#'   \item{IMCI_gt2m_Vars}{Variables used for IMCI among children more than 2 months old}
#'   \item{counsSCVars}{Variables used in counseling caretakers for sick children}
#'   \item{peSCVars}{Variables regarding the physical exam of a sick child.}
#'   \item{Service Readiness Index Variable vectors}{
#'    \describe{
#'      \item{sri_infprev_Vars}{Variables for the SRI infection prevention domain}
#'      \item{sri_diagcapacity_vars}{Variables for the SRI diagnostic capacity domain}
#'      \item{sri_med_Vars}{Variables for the  SRI medicines in stock domain}
#'      \item{sri_basicamenities_Vars}{Variables for the  SRI basic amenities domain}
#'      \item{sri_basicequip_Vars}{Variables for the  SRI basic equipment domain}
#'      \item{sri_Vars}{All variables for the service readiness index}
#'    }
#'    }
#'   \item{Service Readiness Index Variable (JDK) vectors}{
#'    \describe{
#'      \item{sriJDK_infprev_Vars}{JDK version of variables for the SRI infection prevention domain}
#'      \item{sriJDK_diagcapacity_vars}{JDK version of variables for the SRI diagnostic capacity domain}
#'      \item{sriJDK_med_Vars}{JDK version of variables for the  SRI medicines in stock domain}
#'      \item{sriJDK_basicamenities_Vars}{JDK version of variables for the  SRI basic amenities domain}
#'      \item{sriJDK_basicequip_Vars}{JDK version of variables for the  SRI basic equipment domain}
#'    }
#'    }
#'   \item{svc_Vars}{Variables describing if a service is offered (=1), or not (=0)}
#'   \item{danger_sign_vars}{AN danger signs known by the mother}
#' }
#' @source Variables created as part of cleanIR_selfCareBarriers()
#' @name varVecs
NULL