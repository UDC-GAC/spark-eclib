## What can be found in this directory?

The contents of this directory were used to run the experiments to compare the time per iteration of the optimized and non-optimized versions of the parallel implementations of the PSO template implemented by the **spark-eclib** framework and evaluate the impact of the optimizations. The experiments were run on Spark clusters (Spark v3.2.4, Hadoop with YARN v2.10.2) dynamically deployed with BDEv v3.8 (https://bdev.des.udc.es) on a partition of our compute cluster Pluton (https://pluton.dec.udc.es). Configuration files, logs and detailed analysis of the results of the experiments are available from [here](https://doi.org/10.5281/zenodo.8369329).

### Submitting a job

The scripts and configuration files used to submit Spark jobs that use the **spark-eclib** framework are included in the `experiments` dir. 

The scripts are the following:

* `submit.sh`, uses SLURM to submit a job with the number of nodes, cores and partitions passed as arguments, and using the **spark-eclib** configuration in the `eclib-pso.conf` file. The job standard output is stored in `./out/pso-{N}-{P}.out`
* `bdev.sh`, called by `submit.sh` to run the **spark-eclib** job in a Spark cluster dynamically deployed using BDEv. BDEv stores its output in `./BDEv_OUT` and the Spark event logs are stored in `./history/`

These scripts assume the following:

* The working dir with a copy of the contents of this directory is located at `$HOME/eclib-experiment`. The location can be changed by editing the `ECLIB` var in `bdev.sh`
* BDEv is installed in `$HOME/apps/bdev` and its configuration files in `$ECLIB/BDEv-configs/1W`. Follow the instructions [here](http://bdev.des.udc.es) to install BDEv. Locations can be changed by editing the `BDEv_HOME` and `EXP_DIR` vars in `bdev.sh` 
* The fat jar `eclib-0.0.1-test-jar-with-dependencies.jar` built by the **spark-eclib** project is located in the `jar` folder. Also the `log4j` properties files to configure logging levels are placed in this directory.

Additionally, to enable using the Biological System models (Circadian, Nfkb, Mendes, B1-B5), provided by the [Computational Biology Lab](https://www.bangalab.org) 
(formerly [(Bio)Process Engineering Group](http://gingproc.iim.csic.es/)), the `lib` and `benchmarks` dirs with the required files and dependencies must be copied into the working dir. Refer to the README in `testbed/kubernetes` for the details.

