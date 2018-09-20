global home_dir
global fpga_dir
global ip_project_dir
global core_project_dir
global ip_project

# set different directories as variables
set home_dir $::env(SHAKTI_HOME)
set fpga_dir $home_dir/fpga
set ip_project_dir $fpga_dir/manage_ip
set core_project_dir $fpga_dir/e-class

# set ip project name
set ip_project manage_ip
set core_project e-class
puts "home_dir: $home_dir"
puts "fpga_dir: $fpga_dir"
puts "ip_project_dir: $ip_project_dir"
puts "core_project_dir: $core_project_dir"
