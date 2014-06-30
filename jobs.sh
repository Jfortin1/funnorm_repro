cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/dis_val_datasets

# To create the discovery/validation rgsets:
for i in {1..3}
do
	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.dis.val.sh $i;
	sleep 100
done

# To create the discovery/validation methylumi objects:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/dis_val_datasets
for i in {1..3}
do
	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.dis.val.methylumi.sh $i;
	sleep 100
done


# To normalize the datasets with the 5 first methods
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/norm_datasets
for i in {1..8}
do
	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.norm.sh $i;
	sleep 50
done


# To normalize the datasets with funnorm + noob
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/norm_datasets
for i in {1..8}
do
	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.funnorm.noob.sh $i;
	sleep 5
done

# To normalize the datasets with noob:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/norm_datasets
for i in {1..8}
do
	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.norm.noob.sh $i;
	sleep 50
done

# To split the samples for BMIQ
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/norm_datasets
for i in {1..8}
do
	qsub -cwd -V -l mem_free=100G,h_vmem=120G split.bmiq.sh $i;
	sleep 50
done

# To normalize the splitted samples with BMIQ:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/norm_datasets

for i in {1..1460}
do
	qsub -cwd -V -l mem_free=1,h_vmem=2G create.norm.bmiq.sh $i;
	sleep 1
done

# To merge the normalized BMIQ samples:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/norm_datasets
for i in {1..8}
do
	qsub -cwd -V -l mem_free=50,h_vmem=60G merge.norm.bmiq.sh $i;
	sleep 10
done


# To merge all the normalized samples:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/norm_datasets
for i in {1..8}
do
	qsub -cwd -V -l mem_free=50,h_vmem=60G merge.all.norm.sh $i;
	sleep 10
done


cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sva_results
# To submit the SVA jobs:
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=150G,h_vmem=160G create.sva.results.sh $i;
	sleep 2000
done


cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sva_funnorm_results
# To submit the SVA+Funnorm jobs:
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=150G,h_vmem=160G create.sva.funnorm.results.sh $i;
	sleep 500
done


cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sva_funnorm_noob_results
# To submit the SVA+Funnorm+Noob jobs:
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=150G,h_vmem=160G create.sva.funnorm.noob.results.sh $i;
	sleep 500
done



cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/dmps
# To create dmps for the norm matrices:
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=40G,h_vmem=50G create.dmps.sh $i;
	sleep 5
done

# To create ROC data: (only relevant for 1 to 3)
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/roc_data
for i in {1..3}
do
	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.roc.data.sh $i;
	sleep 5
done

# To create overlap data: (only relevant for 1 to 3)
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/roc_data
for i in {1..3}
do
	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.overlap.data.sh $i;
	sleep 5
done

# External validations
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/external_validations
qsub -cwd -V -l jabba,mem_free=20G,h_vmem=30G external.validation.aml.sh;
qsub -cwd -V -l jabba,mem_free=20G,h_vmem=30G external.validation.kirc.sh;





# To create overlap data: (only relevant for 1 to 3)
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sensitivity_analysis
for i in {1..25}
do
	qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.roc.data.sh $i;
	sleep 1
done





# The following code is to submit jobs for the sample size simulation:

# To create the fun norm data for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.norm.sh $k $j;
		sleep 1
	done
done

# To create the other norm data for each simulation (SWAN, Quantile, Dasen)
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.norm.other.sh $k $j;
	done
done




# Create raw
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=5G,h_vmem=10G create.raw.sh $k $j;
	done
done


# Create noob
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=5G,h_vmem=10G create.noob.sh $k $j;
	done
done


# Create noob
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=15G,h_vmem=20G create.norm.new.funnorm.sh $k $j;
	done
done

# Create bmiq
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=5G,h_vmem=10G create.bmiq.sh $k $j;
	done
done



# To create the dmps for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.dmps.sh $k $j;
		sleep 1
	done
done

# To create the dmps raw for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.dmps.raw.sh $k $j;
	done
done

# To create the dmps noob for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.dmps.noob.sh $k $j;
	done
done


# To create the dmps new norm for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=15G,h_vmem=20G create.dmps.norm.new.sh $k $j;
	done
done

# To create the dmps bmiq for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=15G,h_vmem=20G create.dmps.bmiq.sh $k $j;
	done
done




# To create the dmps norm other for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=15G,h_vmem=20G create.dmps.norm.other.sh $k $j;
	done
done




# To create the dmps norm other for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=15G,h_vmem=20G create.dmps.norm.other.sh $k $j;
	done
done



# To create the ROC data for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.roc.data.sh $k $j;
	done
done

# To create the ROC data raw for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.roc.data.raw.sh $k $j;
	done
done


# To create the ROC data new norm for each simulation: 
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.roc.data.norm.new.sh $k $j;
	done
done


# To create the ROC data norm other for each simulation: 
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.roc.data.norm.other.sh $k $j;
	done
done


# To create the ROC data bmiq for each simulation: 
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.roc.data.bmiq.sh $k $j;
	done
done



# To create the ROC data noob for each simulation
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/simulation_samplesize
for k in {1..5}
do
	for j in {1..100}
	do
		qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.roc.data.noob.sh $k $j;
	done
done

# Create combat results
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/combat_results
for k in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=10G,h_vmem=12G create.combat.results.sh $k;
done


# To submit ruv jobs:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_results
for i in {1..8}
do
	for k in {0..40}
	do
		qsub -cwd -V -l jabba,mem_free=25G,h_vmem=26G create.ruv.results.tuning.sh $i $k;
	done
done

# To submit ruv + funnorm jobs: 
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_funnorm_results
for i in {1..8}
do
	for k in {0..40}
	do
		qsub -cwd -V -l jabba,mem_free=25G,h_vmem=26G create.ruv.funnorm.results.tuning.sh $i $k;
		sleep 5
	done
done





# To create tuning plots data:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_results
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=25G,h_vmem=26G create.tuning.plots.data.sh $i;
done

# To create tuning plots data:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_funnorm_results
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=25G,h_vmem=26G create.tuning.plots.data.sh $i;
done



# To create tuning plots data:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_results
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=25G,h_vmem=26G create.tuning.plots.data.2.sh $i;
done


# To create tuning plots data:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_funnorm_results
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=25G,h_vmem=26G create.tuning.plots.data.2.sh $i;
done


# Create optimal RUV:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_results
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=35G,h_vmem=36G create.optimal.ruv.sh $i;
done


# Create optimal RUV + Funnorm
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/ruv_funnorm_results
for i in {1..8}
do
	qsub -cwd -V -l jabba,mem_free=35G,h_vmem=36G create.optimal.ruv.sh $i;
done




# Sensitivity analysis: creation of the normalized datasets:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sensitivity_analysis
for i in {1..8}
do
	for k in {1..10}
	do
		qsub -cwd -V -l jabba,mem_free=20G,h_vmem=30G sensitivity.norm.sh $i $k;
	done
done

# Sensitivity analysis: creation of the  dmps:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sensitivity_analysis
for i in {1..8}
do
	for k in {1..10}
	do
		qsub -cwd -V -l jabba,mem_free=5G,h_vmem=6G sensitivity.dmps.sh $i $k;
	done
done



# Sensitivity analysis: creation of the  dmps:
cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/sensitivity_analysis
for i in {1..8}
do
	for k in {1..10}
	do
		qsub -cwd -V -l jabba,mem_free=5G,h_vmem=6G sensitivity.dmps.sh $i $k;
	done
done



# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach
# # To normalize the datasets with new approach
# for i in {1..8}
# do
# 	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.norm.sh $i;
# 	sleep 50
# done



# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach
# # To normalize the datasets with new approach
# for i in {1..6}
# do
# 	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.dmps.sh $i;
# 	sleep 5
# done


#cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach
# To normalize the datasets with new approach
#for i in {1..3}
#do
#	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.roc.data.sh $i;
#done



#cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach
# To normalize the datasets with new approach
#for i in {1..3}
#do
#	qsub -cwd -V -l mem_free=50G,h_vmem=120G create.overlap.data.sh $i;
#done

#cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach
## To normalize the datasets with new approach
#qsub -cwd -V -l mem_free=50G,h_vmem=120G external.validation.kirc.sh

#cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach
# To normalize the datasets with new approach
#qsub -cwd -V -l mem_free=50G,h_vmem=120G external.validation.aml.sh





## WO Dye bias normalization: 


# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_2
# # To normalize the datasets with new approach
# for i in {1..8}
# do
# 	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.norm.sh $i;
# done



# # Create dmps for noob+funnorm without dye bias normalization
# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_2
# # To normalize the datasets with new approach
# for i in {1..8}
# do
# 	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.dmps.sh $i;
# done


# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_2
# # To normalize the datasets with new approach
# for i in {1..3}
# do
# 	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.roc.data.sh $i;
# done


# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_2
# # To normalize the datasets with new approach
# for i in {1..3}
# do
# 	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.overlap.data.sh $i;
# done



# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_2
# # To normalize the datasets with new approach
# qsub -cwd -V -l mem_free=50G,h_vmem=120G external.validation.kirc.sh

# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_2
# # To normalize the datasets with new approach
# qsub -cwd -V -l mem_free=50G,h_vmem=120G external.validation.aml.sh




# ## Plate: 

# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_3
# # To normalize the datasets with new approach
# for i in {1..8}
# do
# 	qsub -cwd -V -l mem_free=100G,h_vmem=120G create.norm.sh $i;
# done

# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_3
# # To normalize the datasets with new approach
# for i in {1..8}
# do
# 	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.dmps.sh $i;
# done


# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_3
# # To normalize the datasets with new approach
# for i in {1..3}
# do
# 	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.roc.data.sh $i;
# done



# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_3
# # To normalize the datasets with new approach
# for i in {1..3}
# do
# 	qsub -cwd -V -l jabba,mem_free=20G,h_vmem=22G create.overlap.data.sh $i;
# done

# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_3
# # To normalize the datasets with new approach
# qsub -cwd -V -l mem_free=50G,h_vmem=120G external.validation.kirc.sh

# cd /amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/new_approach_3
# # To normalize the datasets with new approach
# qsub -cwd -V -l mem_free=50G,h_vmem=120G external.validation.aml.sh




