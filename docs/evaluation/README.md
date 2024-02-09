# Executing the Evaluation

In order to run the experiment included with the publication, clone this repository and install all requirements (see also the [README](../../README.md)). Then, execute the following command line. Results will be located in the projects root folder. Note, that intermediate results are stored and the experimental run is only finished, once the application terminates. To this end, also refer to the live progress report.

```sh
sbt "runMain org.softlang.s2s.profile"
```

In order to customize the setup of the experiment, consider the file [Profile](../../src/main/scala/org.softlang.s2s/main/Profile.scala) which includes the setup of the experiment, as well as some documentation of the various settings used.
