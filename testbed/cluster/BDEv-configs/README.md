The dir `1W` includes an example of BDEv (v3.8) configuration files for running **spark-eclib** on a Spark cluster using 1 executor per node and all the available threads. 

> To use a different cluster configuration you only have to copy this directory and edit `solutions-conf` to change the number of executors per node and threads used.

> For using clusters of different sizes with the same configuration no changes to BDEv configuration files are necessary. The number of nodes of the cluster is set through `qsub` parameters when launching the experiment.

The following files have been edited:

* `benchmarks.lst`

    Uncomment `command` (execute a user-provided command)

* `cluster_sizes.lst`

    Uncomment `MAX` (use all the available nodes)

* `solutions.lst`

    Uncomment the line of the Spark version you use

* `experiment-conf.sh`

    Set `NUM_EXECUTIONS` to 1 (the number of repetitions will be configured using a parameter of the experiment `.conf` file)
    Set `METHOD_COMMAND` to `spark-submit $SPARKOPTS --class $SPARKCLASS $SPARKJAR $PROPFILE` (the environment variables used by this command will be set in the script used to launch the experiment)

* `solutions-conf.sh`

    Uncomment some lines of the Spark sections (common&YARN) of this file

    - Set `SPARK_DRIVER_CORES` to 1 (change this if you need a multithreaded driver in the master node)

    - Set `SPARK_EXECUTORS_PER_NODE` to 1 (change this to have more executors per worker node)

     - Set `SPARK_CORES_PER_EXECUTOR=<default>` (change this to set the number of threads per executor, by default BDEv uses all available threads)


