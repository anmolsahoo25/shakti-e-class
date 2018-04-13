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
if { [get_ips -quiet multiplier] eq "" } {
    create_ip -name mult_gen -vendor xilinx.com -library ip -version 12.0 -module_name multiplier 
} else {
    reset_run multiplier_synth_1
}

set_property -dict [list CONFIG.PortAType {Unsigned} \
CONFIG.PortAWidth $width \
CONFIG.PortBType {Unsigned} \
CONFIG.PortBWidth $width \
CONFIG.Multiplier_Construction \
{Use_Mults} CONFIG.OptGoal {Speed} \
CONFIG.Use_Custom_Output_Width {false} \
CONFIG.PipeStages $stages \
CONFIG.ClockEnable {false} \
CONFIG.SyncClear {false} ] [get_ips multiplier] 
create_ip_run [get_ips multiplier]
launch_run multiplier_synth_1
wait_on_run multiplier_synth_1

exit
