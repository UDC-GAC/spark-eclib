## What can be found in this directory?

The contents of this directory were used to run the experiments to validate the distributed versions
of the PSO template implemented by the **spark-eclib** framework. The experiments were performed on a Spark cluster 
deployed using the official Apache Spark image (apache/spark:latest, v3.4.1) pulled from Docker Hub, in 
the single-node standalone Kubernetes cluster included with Docker Desktop.
Configuration files, logs and detailed analysis of the results of the experiments
are available from [here](https://doi.org/10.5281/zenodo.8369329).

### Submitting a job

Several scripts used to submit Spark jobs that use the **spark-eclib** framework are included in this directory. 

>These scripts assume that the fat jar `eclib-0.0.1-test-jar-with-dependencies.jar` is located in the `jar` folder and
the configuration files in the `conf` folder.

The scripts are the following:
* `submit.sh` and `submit-with-history.sh`, submit a job with the configuration and 
number of executors, cores and partitions passed as arguments. 
`submit-with-history.sh` also enables Spark event logs that can be viewed with the Spark history 
server after the execution of the job finishes.
* `submit-test.sh`, run an experiment, which comprises submitting the job plus storing in a given directory
a description of the job, the **spark-eclib** configuration used, the job output, the kubernetes logs and the Spark event logs.
* `kill.sh`, kill a submitted job.

### Local directories mounted into the Spark cluster

The following directories are mounted by the submission scripts into the driver and executor pods of the Spark cluster:
* `jar`, is where the fat jar `eclib-0.0.1-test-jar-with-dependencies.jar` built by the **spark-eclib** project 
must be copied. Also the `log4j` properties files to configure logging levels are placed in this directory.
* `conf`, contains the **spark-eclib** configuration files passed as parameters when submitting a job.
* `lib` and `benchmarks`, contain the **SBeclib** dynamic library (`libSBeclib.so`), and some dependencies and files required
to use the Biological System models (Circadian, Nfkb, Mendes, B1-B5), provided by the [Computational Biology Lab](https://www.bangalab.org) 
(formerly [(Bio)Process Engineering Group](http://gingproc.iim.csic.es/)), as objective functions.