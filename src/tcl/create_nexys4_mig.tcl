set curdir [ file dirname [ file normalize [ info script ] ] ]
source $curdir/env.tcl

open_project $ip_project_dir/$ip_project.xpr
set_property "simulator_language" "Mixed" [current_project]
set_property "target_language" "Verilog" [current_project]
if { [get_ips -quiet mig_7series_0] eq "" } {
    create_ip -name mig_7series -vendor xilinx.com -library ip -module_name mig_7series_0 
} else {
    reset_run mig_synth_1
}
set_property CONFIG.XML_INPUT_FILE [file normalize $curdir/mig_config.prj] [get_ips mig_7series_0]
create_ip_run [get_ips mig_7series_0]
launch_run mig_7series_0_synth_1
wait_on_run mig_7series_0_synth_1

exit
