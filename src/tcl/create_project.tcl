set curdir [ file dirname [ file normalize [ info script ] ] ]
source $curdir/env.tcl

if { $argc != 1 } {
    puts "Please pass the FPGA part number is Argument"
    exit
}
# create folders
file mkdir $fpga_dir

# create project
create_project -force $ip_project -dir $ip_project_dir -part [lindex $argv 0]
exit
