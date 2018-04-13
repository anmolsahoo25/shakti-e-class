set curdir [ file dirname [ file normalize [ info script ] ] ]
source $curdir/env.tcl

if { $argc != 2 } {
    puts "The script requires 2 arguments"
    puts "-tclargs <input_width> <pipestages>"
    puts "Please try again."
    exit
} else {
      puts "Operand Width: [lindex $argv 0] PipeStages  [lindex $argv 1]"
}

set width [lindex $argv 0]
set stages [lindex $argv 1]

open_project $ip_project_dir/$ip_project.xpr
set_property "simulator_language" "Mixed" [current_project]
set_property "target_language" "Verilog" [current_project]
if { [get_ips -quiet divider] eq "" } {
    create_ip -name div_gen -vendor xilinx.com -library ip -version 5.1 -module_name divider
} else {
    reset_run divider_synth_1
}
set_property -dict [list CONFIG.algorithm_type {Radix2} \
CONFIG.dividend_and_quotient_width $width\
CONFIG.divisor_width $width\
CONFIG.remainder_type {Remainder}\
CONFIG.clocks_per_division {1}\
CONFIG.divide_by_zero_detect {false}\
CONFIG.FlowControl {NonBlocking}\
CONFIG.OutTready {false}\
CONFIG.latency_configuration {Manual}\
CONFIG.ACLKEN {false}\
CONFIG.divisor_width $width\
CONFIG.fractional_width $width\
CONFIG.operand_sign {Signed}\
CONFIG.OptimizeGoal {Performance}\
CONFIG.latency $stages] [get_ips divider]
create_ip_run [get_ips divider]
launch_run divider_synth_1
wait_on_run divider_synth_1

exit
