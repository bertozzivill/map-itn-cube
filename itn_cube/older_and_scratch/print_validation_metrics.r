

# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial:1.1.0  --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-16 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs --input in_dir=gs://map_users/amelia/itn/itn_cube/results/20200530_no_ihs/03_inla_outputs.Rdata CODE=gs://map_users/amelia/itn/code/generate_cube/print_validation_metrics.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200530_no_ihs/ --command 'Rscript ${CODE}'

in_dir <- Sys.getenv("in_dir")

library(INLA) 
load(in_dir)

for(this_name in names(inla_outputs)){
  print(this_name)
  
  print("WAIC:")
  print(sum(inla_outputs[[this_name]]$model_output$waic$local.waic))
  
  print("LOO:")
  print(sum(log(inla_outputs[[this_name]]$model_output$cpo$cpo)))
}