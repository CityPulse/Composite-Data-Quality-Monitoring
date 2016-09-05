#loadDataSets
combined_datasets_data[['odaa_traffic_data']] <- readRDS("data/odaa_traffic_data.RDS")
combined_datasets_metainformation[['odaa_traffic_metainformation']] <- readRDS("data/odaa_traffic_geometries.RDS")
simple_datasets[['tomtom_icons']] <- readRDS("data/tomtom_icons.RDS")
simple_datasets[['tomtom_flow']] <- readRDS("data/tomtom_flow.RDS")
simple_datasets[['tomtom_incidents']] <- readRDS("data/tomtom_incidents.RDS")
