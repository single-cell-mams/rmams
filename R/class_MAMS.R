#' Define the main MAMS (Matrix and Analysis Metadata Standards) S4 object, used to store all of the other objects.
#' @title class MAMS
#' @description Stores the MAMS data as a list of lists (each of S4 objects).
#' 
#' @slot FOM list 
#' @slot FEA list 
#' @slot OBS list 
#' @slot FID list 
#' @slot OID list 
#' @slot REC list 
#' @slot ONG list
#' @slot FNG list
#'
#' @return the main MAMS class
#' @export
#'
#' 
setClass(
  "MAMS",
  slots = list(
    FOM = "ListOrNULL",
    FEA = "ListOrNULL",
    OBS = "ListOrNULL",
    FID = "ListOrNULL",
    OID = "ListOrNULL",
    REC = "ListOrNULL",
    ONG = "ListOrNULL",
    FNG = "ListOrNULL"
  )
)

#' Constructor function to create a MAMS object
#'
#' @param FOM FOM
#' @param FEA FEA
#' @param OBS OBS
#' @param FID FID
#' @param OID OID
#' @param REC REC
#' @param ONG ONG
#' @param FNG FNG
#'
#' @return a MAMS object for further use
#' @export

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

#' fom
#' @description FOM getter
#' @rdname fom-MAMS-get
#' @param mams MAMS object
#' @param fom_id FOM subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("fom", signature(mams = "MAMS", fom_id = "character", key = "character"), function(mams, fom_id, key) {
    if (is.null(mams@FOM[[fom_id]])){
        stop("No FOM object with the provided fom_id exists.")
    }
    slot(mams@FOM[[fom_id]], key) 
})

#' fom<-
#' @description FOM setter
#' @rdname fom-MAMS-set
#' @param mams MAMS object
#' @param fom_id FOM subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("fom<-", signature(mams = "MAMS", fom_id = "character", key = "character"), function(mams, fom_id, key, value) {
    if (is.null(mams@FOM[[fom_id]])){
        mams@FOM[[fom_id]] <- create_FOM_object(id = fom_id)
    }
    slot(mams@FOM[[fom_id]], key) <- value
    return(mams)
})

#' fid
#' @description FID getter
#' @rdname fid-MAMS-get
#' @param mams MAMS object
#' @param fid_id FID subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("fid", signature(mams = "MAMS", fid_id = "character", key = "character"), function(mams, fid_id, key) {
    if (is.null(mams@FID[[fid_id]])){
        stop("No FID object with the provided fid_id exists.")
    }
    slot(mams@FID[[fid_id]], key) 
})

#' fid<-
#' @description FID setter
#' @rdname fid-MAMS-set
#' @param mams MAMS object
#' @param fid_id FID subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("fid<-", signature(mams = "MAMS", fid_id = "character", key = "character"), function(mams, fid_id, key, value) {
    if (is.null(mams@FID[[fid_id]])){
        mams@FID[[fid_id]] <- create_FID_object(id = fid_id)
    }
    slot(mams@FID[[fid_id]], key) <- value
    return(mams)
})

#' oid
#' @description OID getter
#' @rdname oid-MAMS-get
#' @param mams MAMS object
#' @param oid_id OID subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("oid", signature(mams = "MAMS", oid_id = "character", key = "character"), function(mams, oid_id, key) {
    if (is.null(mams@OID[[oid_id]])){
        stop("No OID object with the provided oid_id exists.")
    }
    slot(mams@OID[[oid_id]], key) 
})

#' oid<-
#' @description OID setter
#' @rdname oid-MAMS-set
#' @param mams MAMS object
#' @param oid_id OID subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("oid<-", signature(mams = "MAMS", oid_id = "character", key = "character"), function(mams, oid_id, key, value) {
    if (is.null(mams@OID[[oid_id]])){
        mams@OID[[oid_id]] <- create_OID_object(id = oid_id)
    }
    slot(mams@OID[[oid_id]], key) <- value
    return(mams)
})

#' fea
#' @description FEA getter
#' @rdname fea-MAMS-get
#' @param mams MAMS object
#' @param fea_id FEA subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("fea", signature(mams = "MAMS", fea_id = "character", key = "character"), function(mams, fea_id, key) {
    if (is.null(mams@FEA[[fea_id]])){
        stop("No FEA object with the provided fea_id exists.")
    }
    slot(mams@FEA[[fea_id]], key) 
})

#' fea<-
#' @description FEA setter
#' @rdname fea-MAMS-set
#' @param mams MAMS object
#' @param fea_id FEA subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("fea<-", signature(mams = "MAMS", fea_id = "character", key = "character"), function(mams, fea_id, key, value) {
    if (is.null(mams@FEA[[fea_id]])){
        mams@FEA[[fea_id]] <- create_FEA_object(id = fea_id)
    }
    slot(mams@FEA[[fea_id]], key) <- value
    return(mams)
})

#' obs
#' @description OBS getter
#' @rdname obs-MAMS-get
#' @param mams MAMS object
#' @param obs_id OBS subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("obs", signature(mams = "MAMS", obs_id = "character", key = "character"), function(mams, obs_id, key) {
    if (is.null(mams@OBS[[obs_id]])){
        stop("No OBS object with the provided obs_id exists.")
    }
    slot(mams@OBS[[obs_id]], key) 
})

#' obs<-
#' @description OBS setter
#' @rdname obs-MAMS-set
#' @param mams MAMS object
#' @param obs_id OBS subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("obs<-", signature(mams = "MAMS", obs_id = "character", key = "character"), function(mams, obs_id, key, value) {
    if (is.null(mams@OBS[[obs_id]])){
        mams@OBS[[obs_id]] <- create_OBS_object(id = obs_id)
    }
    slot(mams@OBS[[obs_id]], key) <- value
    return(mams)
})

#' rec
#' @description REC getter
#' @rdname rec-MAMS-get
#' @param mams MAMS object
#' @param rec_id REC subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("rec", signature(mams = "MAMS", rec_id = "character", key = "character"), function(mams, rec_id, key) {
    if (is.null(mams@REC[[rec_id]])){
        stop("No REC object with the provided rec_id exists.")
    }
    slot(mams@REC[[rec_id]], key) 
})

#' rec<-
#' @description REC setter
#' @rdname rec-MAMS-set
#' @param mams MAMS object
#' @param rec_id REC subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("rec<-", signature(mams = "MAMS", rec_id = "character", key = "character"), function(mams, rec_id, key, value) {
    if (is.null(mams@REC[[rec_id]])){
        mams@REC[[rec_id]] <- create_REC_object(id = rec_id)
    }
    slot(mams@REC[[rec_id]], key) <- value
    return(mams)
})

#' ong
#' @description ONG getter
#' @rdname ong-MAMS-get
#' @param mams MAMS object
#' @param ong_id ONG subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("ong", signature(mams = "MAMS", ong_id = "character", key = "character"), function(mams, ong_id, key) {
    if (is.null(mams@ONG[[ong_id]])){
        stop("No ONG object with the provided ong_id exists.")
    }
    slot(mams@ONG[[ong_id]], key) 
})

#' ong<-
#' @description ONG setter
#' @rdname ong-MAMS-set
#' @param mams MAMS object
#' @param ong_id ONG subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("ong<-", signature(mams = "MAMS", ong_id = "character", key = "character"), function(mams, ong_id, key, value) {
    if (is.null(mams@ONG[[ong_id]])){
        mams@ONG[[ong_id]] <- create_ONG_object(id = ong_id)
    }
    slot(mams@ONG[[ong_id]], key) <- value
    return(mams)
})

#' fng
#' @description FNG getter
#' @rdname fng-MAMS-set
#' @param mams MAMS object
#' @param fng_id FNG subobject ID
#' @param key key
#' @return value of key
#' @export
setMethod("fng", signature(mams = "MAMS", fng_id = "character", key = "character"), function(mams, fng_id, key) {
    if (is.null(mams@FNG[[fng_id]])){
        stop("No FNG object with the provided fng_id exists.")
    }
    slot(mams@FNG[[fng_id]], key) 
})
#' fng<-
#' @description FNG setter
#' @rdname fng-MAMS-set
#' @param mams MAMS object
#' @param fng_id FNG subobject ID
#' @param key key
#' @param value value
#' @return nothing (setter)
#' @export
setMethod("fng<-", signature(mams = "MAMS", fng_id = "character", key = "character"), function(mams, fng_id, key, value) {
    if (is.null(mams@FNG[[fng_id]])){
        mams@FNG[[fng_id]] <- create_FNG_object(id = fng_id)
    }
    slot(mams@FNG[[fng_id]], key) <- value
    return(mams)
})

