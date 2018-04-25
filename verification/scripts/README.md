### 1. Verification test suites

* riscv-tests
* riscv-torture
* csmith-run
* benchmarks
* peripherals

### 2. Verification Scripts

* **Initial Setup**
```
export SHAKTI_E_HOME=<repo-check-out-path>/e-class
```

* Compile and simulate a single test

```
perl makeTest.pl --test=<test_name> --suite=<test_directory>

Eg. perl makeTest.pl --test=add --suite=directed/riscv-tests/isa/rv64ui
```
* Run a smoke regression of riscv-tests before any checkin
```
perl makeRegress.pl --sub
```
* Run a regression with random and directed tests
```
perl makeRegress.pl --gen --sub
```
* Generate and compile a single riscv-torture tests
```
perl makeTorture.pl --test_config=bringup --sub
```
* Generate riscv-torture tests for a specific config file
```
perl makeTorture.pl --test_config=<test_config_name> --test_count=<count>

Eg. perl makeTorture.pl --test_config=bringup --test_count=5
```
