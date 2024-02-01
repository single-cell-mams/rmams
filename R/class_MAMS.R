#' Class to create MAMS object
#'
#' @slot FOM list. 
#' @slot FEA list. 
#' @slot OBS list. 
#' @slot FID list. 
#' @slot OID list. 
#' @slot REC list. 
#' @slot ONG list.
#' @slot FNG list.
#'
#' @return
#' @export
#'
#' @examples
setClass(
  "MAMS",
  slots = list(
    FOM = "list",
    FEA = "list",
    OBS = "list",
    FID = "list",
    OID = "list",
    REC = "list",
    ONG = "list",
    FNG = "list"
  )
)

#' Constructor function to create a MAMS object
#'
#' @param FOM 
#' @param FEA 
#' @param OBS 
#' @param FID 
#' @param OID 
#' @param REC 
#' @param ONG
#' @param FNG
#'
#' @return
#' @export
#'
#' @examples
create_MAMS_Object <- function(
    FOM = list(),
    FEA = list(),
    OBS = list(),
    FID = list(),
    OID = list(),
    REC = list(),
    ONG = list(),
    FNG = list()
) {
  # Create an instance of the MAMS class
  mams_obj <- new("MAMS",
                  FOM = FOM,
                  FEA = FEA,
                  OBS = OBS,
                  FID = FID,
                  OID = OID,
                  REC = REC,
                  ONG = ONG,
                  FNG = FNG
  )
  
  # Validate the MAMS object
  #validate_MAMS_Object(mams_obj)
  
  return(mams_obj)
}

# fom function to get attributes
setMethod("fom", signature(mams = "MAMS", fom_id = "character", key = "character"), function(mams, fom_id, key) {
    if (is.null(mams@FOM[[fom_id]])){
        stop("No FOM object with the provided fom_id exists.")
    }
    slot(mams@FOM[[fom_id]], key) 
})

# fom function to set attributes
setMethod("fom<-", signature(mams = "MAMS", fom_id = "character", key = "character"), function(mams, fom_id, key, value) {
    if (is.null(mams@FOM[[fom_id]])){
        mams@FOM[[fom_id]] <- create_FOM_Object(id = fom_id)
    }
    slot(mams@FOM[[fom_id]], key) <- value
    return(mams)
})

# fid function to get attributes
setMethod("fid", signature(mams = "MAMS", fom_id = "character", key = "character"), function(mams, fid_id, key) {
    if (is.null(mams@FOM[[fid_id]])){
        stop("No FID object with the provided fom_id exists.")
    }
    slot(mams@FOM[[fom_id]], key) 
})

# fid function to set attributes
setMethod("fom<-", signature(mams = "MAMS", fom_id = "character", key = "character"), function(mams, fom_id, key, value) {
    if (is.null(mams@FOM[[fom_id]])){
        mams@FOM[[fom_id]] <- create_FOM_Object(id = fom_id)
    }
    slot(mams@FOM[[fom_id]], key) <- value
    return(mams)
})