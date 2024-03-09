#' Constructor function to create a FOM object
#' @description Internal check function to determine if all necessary MAMS object slots
#' exist within the object
#' @param mams_object Input MAMS object to be checked
#' @export 
check_MAMS <- function(mams_object){
    #Check 1. Check to see all classes exist in the object
    #The following classes are required for each MAMS object:
    all_possible_classes = c("FOM", "ONG", "FEA", "OBS", "FID", "OID", "REC", "FNG")
    if(!all(all_possible_classes %in% methods::slotNames(mams_object))){

        missing_classes <- paste(all_possible_classes[which(!all_possible_classes %in% methods::slotNames(mams_object))], collapse = ', ')

        stop(paste("The following classes are missing from the MAMS object:", missing_classes))
    }

    #Check 2. Check to see all required fields for each MAMS class (FOM, ONG, etc.) exist
    required_field_list = list(FOM = c("id", "dataset_id", "data_type", "analyte"),
                               ONG = c("id", "dataset_id"),
                               FEA = c("id", "dataset_id", "feature_modality"),
                               OBS = c("id", "dataset_id"),
                               FID = c("id", "dataset_id"),
                               OID = c("id", "dataset_id"),
                               REC = c("record_package_name", "record_package_version",
                                       "record_function_name"),
                               FNG = c("id", "dataset_id"))

    #Q1. What ID do I refer to when returning error message?
    #Q2. What is the correct way for the user to input the missing information?

    for(mams_class in all_possible_classes){
        slot_obj <- methods::slot(mams_object, mams_class)

        check_result <- lapply(slot_obj, function(x){
            if(!all(required_field_list[[mams_class]] %in% methods::slotNames(x))){
                return(required_field_list[[mams_class]][which(!required_field_list[[mams_class]] %in% methods::slotNames(x))])
            }else{
                missing_fields <- c()
                for(fields in required_field_list[[mams_class]]){
                    if(is.na(methods::slot(x, fields))){
                        missing_fields <- c(missing_fields, fields)
                    }
                }
                if(!is.null(missing_fields)){
                    return(missing_fields)
                }else{
                    return("")
                }
            }
        })

        #Temp solution: Add another line to give user information (within error message) to
        #identify which field is missing slot info
        identifier_info <- sapply(slot_obj, function(x){
            if(!is.na(id(x))){
                return(id(x))
            }else{
                return("")
            }
        })

        if(!all(check_result == "")){
            #Subset to erroring objects
            identifier_info = identifier_info[which(check_result != "")]
            check_result = check_result[which(check_result != "")]

            message("Error: Please use the setter functions to add the missing field information for the following slots: \n(Ex. If missing 'filepath' for fom1, then 'fom(mams = mams_object, fom_id = 'fom1', key = 'filepath') <- ...')\n:")
            stop(paste0(paste0(names(check_result), "(ID:",identifier_info,")"), ' is missing the field(s): ', check_result, '\n'))

        }
    }
    #If passes to here, all clear
    message("All clear!")
}