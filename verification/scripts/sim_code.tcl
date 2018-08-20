coverage -setup -dut main.top
coverage -setup -testname test
coverage -code -reset
coverage -fsm -reset
coverage -toggle -reset
run
exit

