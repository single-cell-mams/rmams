#' Class to create MAMS object
#'
#' @slot FOM list. 
#' @slot FEA list. 
#' @slot OBS list. 
#' @slot FID list. 
#' @slot OID list. 
#' @slot REC list. 
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
    REC = "list"
  )
)

#' Class to create MAMS object
#'
#' @slot FOM list. 
#' @slot FEA list. 
#' @slot OBS list. 
#' @slot FID list. 
#' @slot OID list. 
#' @slot REC list. 
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
    ONG = "list"
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
    ONG = list()
) {
  # Create an instance of the MAMS class
  mams_obj <- new("MAMS",
                  FOM = FOM,
                  FEA = FEA,
                  OBS = OBS,
                  FID = FID,
                  OID = OID,
                  REC = REC,
                  ONG = ONG
  )
  
  # Validate the MAMS object
  #validate_MAMS_Object(mams_obj)
  
  return(mams_obj)
}
}

#' Function to validate a MAMS object
#'
#' @param mams_obj 
#'
#' @return
#' @export
#'
#' @examples
validate_MAMS_Object <- function(mams_obj) {
  # Check classes
  classes_FOM <- sapply(mams_obj@FOM, class)
  if(all(classes_FOM == "FOM")){
    stop("Invalid FOM list")
  }
  classes_FEA <- sapply(mams_obj@FEA, class)
  if(all(classes_FEA == "FEA")){
    stop("Invalid FEA list")
  }
  classes_OBS <- sapply(mams_obj@OBS, class)
  if(all(classes_OBS == "OBS")){
    stop("Invalid OBS list")
  }
  classes_FID <- sapply(mams_obj@FID, class)
  if(all(classes_FID == "FID")){
    stop("Invalid FID list")
  }
  classes_OID <- sapply(mams_obj@OID, class)
  if(all(classes_OID == "OID")){
    stop("Invalid OID list")
  }
  classes_REC <- sapply(mams_obj@REC, class)
  if(all(classes_REC == "REC")){
    stop("Invalid REC list")
  }
  
  # Check length if required
  # if (length(mams_obj@FOM) == 0) {
  #   stop("The 'FOM' list in the 'MAMS' object must not be empty.")
  # }
  
  # Other checks
  
  # If everything is valid, return TRUE
  return(TRUE)
}


# fom function to set attributes
setMethod("fom", signature(mams = "MAMS"), function(mams, fom_id, key) {
    if (is.null(mams@FOM[[fom_id]][[key]])) {
        mams@FOM[[fom_id]][[key]] <- NA
    }
    else
        mams@FOM[[fom_id]][[key]]
})
setMethod("fom<-", signature(mams = "MAMS"), function(mams, fom_id, key, value) {
    mams@FOM[[fom_id]][[key]] <- value
    return(mams)
})

# call this in setMethod 
fom <- function(mams, fom_id, key) { # this function just returns value? why need to create object here (should be in the other one where value is added)
    if (is.null(mams@FOM[[fom_id]])) {
        mams@FOM[[fom_id]] <- create_FOM_Object()
        # need to add id as well 
        print("NA")
    }
    else if (is.null(mams@FOM[[fom_id]][[key]])) { #nested?
        mams@FOM[[fom_id]][[key]] <- NA
        print("NA")
    }
    else {
        print(mams@FOM[[fom_id]][[key]])
    }
    return(mams@FOM[[fom_id]][[key]]) # return value from the key 
}