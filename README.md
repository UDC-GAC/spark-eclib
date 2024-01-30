spark-eclib
===========

[![License](http://img.shields.io/:License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![Scala](http://img.shields.io/:Scala-2.12.15-green.svg)]()
[![DOI](https://zenodo.org/badge/702997685.svg)](https://zenodo.org/badge/latestdoi/702997685)

**spark-eclib** is a framework written in Scala to support the development of distributed population-based metaheuristics and 
their application to the global optimization of large-scale problems in [Spark](https://spark.apache.org/) clusters.

The **spark-eclib** framework is being developed by the Computer Architecture Group ([GAC](http://gac.udc.es/)) 
at Universidade da Coruña ([UDC](http://www.udc.gal/index.html?language=en))
in collaboration with the [Computational Biology Lab](https://www.bangalab.org)
(formerly [(Bio)Process Engineering Group](http://gingproc.iim.csic.es/)) at Misión Biológica de Galicia ([MBG-CSIC](http://www.mbg.csic.es/en/the-mision-biologica-de-galicia/)).
Both groups maintain a long-term research collaboration on the field of global optimization of large-scale problems 
from Computational Biology using distributed frameworks on computational clusters.
After having implemented and evaluated different ad-hoc implementations of distributed population-based metaheuristics 
on frameworks like Hadoop or Spark (e.g. [SiPDE](https://bitbucket.org/xcpardo/sipde/src/master/), eSS),
the development of **spark-eclib** was started with the main objective of avoiding to reinvent the wheel every time 
a new metaheuristic is implemented and to improve the automation and reproducibility of the optimization experiments.

The framework provides a reduced set of abstractions to represent the general structure of population-based metaheuristics as templates from which 
different variants of algorithms can be instantiated by the implementation of strategies. Strategies can be reused between metaheuristics, thus enforcing code reusability.
To validate the approach, a template for Particle Swarm Optimization (PSO) was implemented applying the general abstractions provided by the framework.
The template supports the instantiation of different variants of the PSO algorithm, a long list of configurable topologies, and 
several execution models (i.e. sequential, master-worker and island-based).

This repository contains a snapshot of the state of the source code of the framework as described in [10.1016/j.swevo.2024.101483](https://doi.org/10.1016/j.swevo.2024.101483)

Citation
--------

Please, if you use **spark-eclib**, cite our work using the following reference:
>Xoán C. Pardo, Patricia González, Julio R. Banga, Ramón Doallo. _Population based metaheuristics in Spark: towards a general framework using PSO as a case study_. Swarm and Evolutionary Computation, 85 (2024), article 101483, [10.1016/j.swevo.2024.101483](https://doi.org/10.1016/j.swevo.2024.101483)

Usage
-----

To build the project use the Maven command:

`mvn clean package`

The resulting fat jar `eclib-0.0.1-test-jar-with-dependencies.jar` will be placed in the `target` folder of the project.

The simplest command to submit to a Spark cluster a job using **spark-eclib** would be:

    spark-submit --class gal.udc.gac.eclib.EclibTest \
      --master <master-url> \
      eclib-0.0.1-test-jar-with-dependencies.jar \
      <configuration_file>

Examples
--------

Refer to the **README.md** file in the `testbed\kubernetes` and `testbed\cluster` directories.

Branches
--------

Besides `master`, there are branches with different combinations of the optimizations implemented 
to reduce the number of Spark jobs per iteration. These branches were used to profile the
performance of the parallel implementations with and without optimizations.

License
-------

This code is open source software licensed under the [GPLv3 License](https://www.gnu.org/licenses/gpl-3.0.html).

Contact
-------

**Xoán C. Pardo**  <[xoan.pardo@udc.gal](mailto:xoan.pardo@udc.gal)> \
[Computer Architecture Group](https://gac.udc.es/english/) / [CITIC]( https://citic.udc.es/en/home-english-2/ )\
Universidade da Coruña ([UDC]( https://www.udc.gal/en/ ))\
Spain

[![ORCID](http://img.shields.io/:ORCID-0000--0001--8577--6980-green.svg)](https://orcid.org/0000-0001-8577-6980)
[![ResearchID](http://img.shields.io/:ResearchID-D--8250--2015-yellow.svg)](https://publons.com/researcher/2420618/xoan-c-pardo/)
[![Scopus](http://img.shields.io/:Scopus-14625465500-blue.svg)](http://www.scopus.com/authid/detail.uri?authorId=14625465500)

[mailto:xoan.pardo@udc.gal]: xoan.pardo@udc.gal
