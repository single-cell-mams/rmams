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
create_MAMS_object <- function(
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
        mams@FOM[[fom_id]] <- create_FOM_object(id = fom_id)
    }
    slot(mams@FOM[[fom_id]], key) <- value
    return(mams)
})

# fid function to get attributes
setMethod("fid", signature(mams = "MAMS", fid_id = "character", key = "character"), function(mams, fid_id, key) {
    if (is.null(mams@FID[[fid_id]])){
        stop("No FID object with the provided fid_id exists.")
    }
    slot(mams@FID[[fid_id]], key) 
})

# fid function to set attributes
setMethod("fid<-", signature(mams = "MAMS", fid_id = "character", key = "character"), function(mams, fid_id, key, value) {
    if (is.null(mams@FID[[fid_id]])){
        mams@FID[[fid_id]] <- create_FID_object(id = fid_id)
    }
    slot(mams@FID[[fid_id]], key) <- value
    return(mams)
})

# oid function to get attributes
setMethod("oid", signature(mams = "MAMS", oid_id = "character", key = "character"), function(mams, oid_id, key) {
    if (is.null(mams@OID[[oid_id]])){
        stop("No OID object with the provided oid_id exists.")
    }
    slot(mams@OID[[oid_id]], key) 
})

# oid function to set attributes
setMethod("oid<-", signature(mams = "MAMS", oid_id = "character", key = "character"), function(mams, oid_id, key, value) {
    if (is.null(mams@OID[[oid_id]])){
        mams@OID[[oid_id]] <- create_OID_object(id = oid_id)
    }
    slot(mams@OID[[oid_id]], key) <- value
    return(mams)
})

# fea function to get attributes
setMethod("fea", signature(mams = "MAMS", fea_id = "character", key = "character"), function(mams, fea_id, key) {
    if (is.null(mams@FEA[[fea_id]])){
        stop("No FEA object with the provided fea_id exists.")
    }
    slot(mams@FEA[[fea_id]], key) 
})

# fea function to set attributes
setMethod("fea<-", signature(mams = "MAMS", fea_id = "character", key = "character"), function(mams, fea_id, key, value) {
    if (is.null(mams@FEA[[fea_id]])){
        mams@FEA[[fea_id]] <- create_FEA_object(id = fea_id)
    }
    slot(mams@FEA[[fea_id]], key) <- value
    return(mams)
})

# obs function to get attributes
setMethod("obs", signature(mams = "MAMS", obs_id = "character", key = "character"), function(mams, obs_id, key) {
    if (is.null(mams@OBS[[obs_id]])){
        stop("No OBS object with the provided obs_id exists.")
    }
    slot(mams@OBS[[obs_id]], key) 
})

# obs function to set attributes
setMethod("obs<-", signature(mams = "MAMS", obs_id = "character", key = "character"), function(mams, obs_id, key, value) {
    if (is.null(mams@OBS[[obs_id]])){
        mams@OBS[[obs_id]] <- create_OBS_object(id = obs_id)
    }
    slot(mams@OBS[[obs_id]], key) <- value
    return(mams)
})

# rec function to get attributes
setMethod("rec", signature(mams = "MAMS", rec_id = "character", key = "character"), function(mams, rec_id, key) {
    if (is.null(mams@REC[[rec_id]])){
        stop("No REC object with the provided rec_id exists.")
    }
    slot(mams@REC[[rec_id]], key) 
})

# rec function to set attributes
setMethod("rec<-", signature(mams = "MAMS", rec_id = "character", key = "character"), function(mams, rec_id, key, value) {
    if (is.null(mams@REC[[rec_id]])){
        mams@REC[[rec_id]] <- create_REC_object(id = rec_id)
    }
    slot(mams@REC[[rec_id]], key) <- value
    return(mams)
})

# ong function to get attributes
setMethod("ong", signature(mams = "MAMS", ong_id = "character", key = "character"), function(mams, ong_id, key) {
    if (is.null(mams@ONG[[ong_id]])){
        stop("No ONG object with the provided ong_id exists.")
    }
    slot(mams@ONG[[ong_id]], key) 
})

# ong function to set attributes
setMethod("ong<-", signature(mams = "MAMS", ong_id = "character", key = "character"), function(mams, ong_id, key, value) {
    if (is.null(mams@ONG[[ong_id]])){
        mams@ONG[[ong_id]] <- create_ONG_object(id = ong_id)
    }
    slot(mams@ONG[[ong_id]], key) <- value
    return(mams)
})

# fng function to get attributes
setMethod("fng", signature(mams = "MAMS", fng_id = "character", key = "character"), function(mams, fng_id, key) {
    if (is.null(mams@FNG[[fng_id]])){
        stop("No FNG object with the provided fng_id exists.")
    }
    slot(mams@FNG[[fng_id]], key) 
})

# fng function to set attributes
setMethod("fng<-", signature(mams = "MAMS", fng_id = "character", key = "character"), function(mams, fng_id, key, value) {
    if (is.null(mams@FNG[[fng_id]])){
        mams@FNG[[fng_id]] <- create_FNG_object(id = fng_id)
    }
    slot(mams@FNG[[fng_id]], key) <- value
    return(mams)
})

