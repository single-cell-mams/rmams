create_MAMS_from_JSON <- function(JSON){
  #loop through each datasets stored in this JSON file
  MAMS_list = list()
  for (i in 1:length(JSON)){
    #check what slots are saved
    slots_present <- names(JSON[[i]])
    #Save FOMs
    FOM_list = list()
    if ('FOM' %in% slots_present){
      for (j in 1:length(JSON[[i]][["FOM"]])){
        FOM_list[j] = create_FOM_object(id = JSON[[i]][["FOM"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["FOM"]][[j]][['dataset_id']],
                                        filepath = JSON[[i]][["FOM"]][[j]][['filepath']],
                                        accessor = JSON[[i]][["FOM"]][[j]][['accessor']],
                                        data_type = JSON[[i]][["FOM"]][[j]][['data_type']],
                                        representation = JSON[[i]][["FOM"]][[j]][['representation']],
                                        representation_description = JSON[[i]][["FOM"]][[j]][['representation_description']],
                                        obs_unit = JSON[[i]][["FOM"]][[j]][['obs_unit']],
                                        processing = JSON[[i]][["FOM"]][[j]][['processing']],
                                        processing_description = JSON[[i]][["FOM"]][[j]][['processing_description']],
                                        analyte = JSON[[i]][["FOM"]][[j]][['analyte']],
                                        analyte_description = JSON[[i]][["FOM"]][[j]][['analyte_description']],
                                        modality = JSON[[i]][["FOM"]][[j]][['modality']],
                                        obs_subset = JSON[[i]][["FOM"]][[j]][['obs_subset']],
                                        obs_subset_description = JSON[[i]][["FOM"]][[j]][['obs_subset_description']],
                                        feature_subset = JSON[[i]][["FOM"]][[j]][['feature_subset']],
                                        feature_subset_description = JSON[[i]][["FOM"]][[j]][['feature_subset_description']],
                                        record_id = JSON[[i]][["FOM"]][[j]][['record_id']],
                                        parent_id = JSON[[i]][["FOM"]][[j]][['parent_id']],
                                        parent_relationship = JSON[[i]][["FOM"]][[j]][['parent_relationship']],
                                        parent_relationship_description = JSON[[i]][["FOM"]][[j]][['parent_relationship_description']],
                                        oid = JSON[[i]][["FOM"]][[j]][['oid']],
                                        fid = JSON[[i]][["FOM"]][[j]][['fid']],
                                        obs = JSON[[i]][["FOM"]][[j]][['obs']],
                                        fea = JSON[[i]][["FOM"]][[j]][['fea']],
                                        ong = JSON[[i]][["FOM"]][[j]][['ong']],
                                        fng = JSON[[i]][["FOM"]][[j]][['fng']]
        )
      }
      names(FOM_list) = names(JSON[[i]][["FOM"]])
    }
    #Repeat for FEA
    FEA_list = list()
    if ('FEA' %in% slots_present){
      for (j in 1:length(JSON[[i]][["FEA"]])){
        FEA_list[j] = create_FEA_object(id = JSON[[i]][["FEA"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["FEA"]][[j]][['dataset_id']],
                                        filepath = JSON[[i]][["FEA"]][[j]][['filepath']],
                                        accessor = JSON[[i]][["FEA"]][[j]][['accessor']],
                                        feature_modality = JSON[[i]][["FEA"]][[j]][["feature_modality"]],
                                        reference_database = JSON[[i]][["FEA"]][[j]][['reference_database']],
                                        reference_organism = JSON[[i]][["FEA"]][[j]][['reference_organism']],
                                        record_id = JSON[[i]][["FEA"]][[j]][['record_id']]
        )
      }
      names(FEA_list) = names(JSON[[i]][["FEA"]])
    }
    #Repeat for OBS
    OBS_list = list()
    if ('OBS' %in% slots_present){
      for (j in 1:length(JSON[[i]][["OBS"]])){
        OBS_list[j] = create_OBS_object(id = JSON[[i]][["OBS"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["OBS"]][[j]][['dataset_id']],
                                        filepath = JSON[[i]][["OBS"]][[j]][['filepath']],
                                        accessor = JSON[[i]][["OBS"]][[j]][['accessor']],
                                        record_id = JSON[[i]][["OBS"]][[j]][['record_id']]
        )
      }
      names(OBS_list) = names(JSON[[i]][["OBS"]])
    }
    #Repeat for OID
    OID_list = list()
    if ('OID' %in% slots_present){
      for (j in 1:length(JSON[[i]][["OID"]])){
        OID_list[j] = create_OID_object(id = JSON[[i]][["OID"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["OID"]][[j]][['dataset_id']],
                                        filepath = JSON[[i]][["OID"]][[j]][['filepath']],
                                        accessor = JSON[[i]][["OID"]][[j]][['accessor']]
        )
      }
      names(OID_list) = names(JSON[[i]][["OID"]])
    }
    #Repeat for FID
    FID_list = list()
    if ('FID' %in% slots_present){
      for (j in 1:length(JSON[[i]][["FID"]])){
        FID_list[j] = create_FID_object(id = JSON[[i]][["FID"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["FID"]][[j]][['dataset_id']],
                                        filepath = JSON[[i]][["FID"]][[j]][['filepath']],
                                        accessor = JSON[[i]][["FID"]][[j]][['accessor']]
        )
      }
      names(FID_list) = names(JSON[[i]][["FID"]])
    }
    #Repeat for ONG
    ONG_list = list()
    if ('ONG' %in% slots_present){
      for (j in 1:length(JSON[[i]][["ONG"]])){
        ONG_list[j] = create_ONG_object(id = JSON[[i]][["ONG"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["ONG"]][[j]][['dataset_id']],
                                        filepath = JSON[[i]][["ONG"]][[j]][['filepath']],
                                        accessor = JSON[[i]][["ONG"]][[j]][['accessor']],
                                        parent_id = JSON[[i]][["ONG"]][[j]][['parent_id']],
                                        record_id = JSON[[i]][["ONG"]][[j]][['record_id']],
                                        edge_metric = JSON[[i]][["ONG"]][[j]][['edge_metric']],
                                        metric_type = JSON[[i]][["ONG"]][[j]][['metric_type']]
        )
      }
      names(ONG_list) = names(JSON[[i]][["ONG"]])
    }
    #Repeat for FNG
    FNG_list = list()
    if ('FNG' %in% slots_present){
      for (j in 1:length(JSON[[i]][["FNG"]])){
        FNG_list[j] = create_FNG_object(id = JSON[[i]][["FNG"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["FNG"]][[j]][['dataset_id']],
                                        filepath = JSON[[i]][["FNG"]][[j]][['filepath']],
                                        accessor = JSON[[i]][["FNG"]][[j]][['accessor']],
                                        parent_id = JSON[[i]][["FNG"]][[j]][['parent_id']],
                                        record_id = JSON[[i]][["FNG"]][[j]][['record_id']],
                                        edge_metric = JSON[[i]][["FNG"]][[j]][['edge_metric']],
                                        metric_type = JSON[[i]][["FNG"]][[j]][['metric_type']]
        )
      }
      names(FNG_list) = names(JSON[[i]][["FNG"]])
      
    }
    #Repeat for REC
    REC_list = list()
    if ('REC' %in% slots_present){
      for (j in 1:length(JSON[[i]][["REC"]])){
        REC_list[j] = create_REC_object(id = JSON[[i]][["REC"]][[j]][['id']],
                                        dataset_id = JSON[[i]][["REC"]][[j]][['dataset_id']],
                                        record_id = JSON[[i]][["REC"]][[j]][['record_id']],
                                        record_package_name = JSON[[i]][["REC"]][[j]][['record_package_name']],
                                        record_package_version = JSON[[i]][["REC"]][[j]][['record_package_version']],
                                        record_function_name = JSON[[i]][["REC"]][[j]][['record_function_name']],
                                        record_function_parameters = JSON[[i]][["REC"]][[j]][['record_function_parameters']],
                                        record_workflow_link = JSON[[i]][["REC"]][[j]][['record_workflow_link']],
                                        record_runtime_start = JSON[[i]][["REC"]][[j]][['record_runtime_start']],
                                        record_runtime_end = JSON[[i]][["REC"]][[j]][['record_runtime_end']],
                                        record_runtime_duration = JSON[[i]][["REC"]][[j]][['record_runtime_duration']]
        )
      }
      names(REC_list) = names(JSON[[i]][["REC"]])
    }
    
    ##Create a MAMS object for this dataset
    MAMS_list[i] = create_MAMS_object(
      FOM = FOM_list,
      FEA = FEA_list,
      OBS = OBS_list,
      FID = FID_list,
      OID = OID_list,
      REC = REC_list,
      ONG = ONG_list,
      FNG = FNG_list
    )
  }
  return(MAMS_list)
}

write_MAMS_to_JSON<-function(MAMS,filepath){
  if (is.list(MAMS)){
    list_to_write = list()
    for (i in 1:length(MAMS)){
      MAMSobj = MAMS[[i]]
      list_to_write[[i]] = list("FOM"= lapply(MAMSobj@FOM,collapse_to_list),
                           "FEA"= lapply(MAMSobj@FEA,collapse_to_list),
                           "OBS"= lapply(MAMSobj@OBS,collapse_to_list),
                           "FID"= lapply(MAMSobj@FID,collapse_to_list),
                           "OID"= lapply(MAMSobj@OID,collapse_to_list),
                           "ONG"= lapply(MAMSobj@ONG,collapse_to_list),
                           "FNG"= lapply(MAMSobj@FNG,collapse_to_list),
                           "REC"= lapply(MAMSobj@REC,collapse_to_list))
      names(list_to_write[[i]]) = c("FOM","FEA","OBS","FID","OID","ONG","FNG","REC")
    }
  }
  else if (is(MAMS, "MAMS")){
    list_to_write  = list("FOM"= lapply(MAMS@FOM,collapse_to_list),
                          "FEA"= lapply(MAMS@FEA,collapse_to_list),
                          "OBS"= lapply(MAMS@OBS,collapse_to_list),
                          "FID"= lapply(MAMS@FID,collapse_to_list),
                          "OID"= lapply(MAMS@OID,collapse_to_list),
                          "ONG"= lapply(MAMS@ONG,collapse_to_list),
                          "FNG"= lapply(MAMS@FNG,collapse_to_list),
                          "REC"= lapply(MAMS@REC,collapse_to_list))
    names(list_to_write) = c("FOM","FEA","OBS","FID","OID","ONG","FNG","REC")
  }
  else(
    stop("MAMS must be a MAMS object , or a list of MAMS objects")
  )
  names(list_to_write) = names(MAMS)
  json_data <- jsonlite::toJSON(list_to_write, pretty = TRUE)
  #return(json_data)
  jsonlite::write_json(json_data, filepath)
}



