EFMP: EDSL For Managing Processes
=================================

This is for now highly exerimental.

The idea is to launch and keep track of processes running on many hosts.  They
can run as `nohup setsid` jobs, PBS jobs, or alike.  Everything is done from a
single clear source file, manipulating the state of the `Execution_engine`
module in an *EDSL-ish* way.

If you're curious you may try to run `omake` and look at the
`./src/test/efmp_test.ml` for an example of usage.


Any questions? use [a github issue](https://github.com/smondet/efmp/issues) or
email `seb —at— mondet.org`.
